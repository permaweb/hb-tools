interface Tag {
  name: string
  value: string
}

interface Message {
  messageId: string
  processId: string
  cu: string
}

interface PushedMessage {
  Target: string
  Tags: Tag[]
  [key: string]: any
}

interface GraphQLNode {
  id: string
  tags: Tag[]
  recipient: string
  anchor: string
}

interface GraphQLResponse {
  data?: {
    transactions?: {
      edges: Array<{
        node: GraphQLNode
      }>
    }
  }
}

interface AlreadyPushedMessage {
  processId: string
  originalMessageId: string
  pushedMessage: GraphQLNode
  slot: number
}

interface UnpushedMessage {
  processId: string
  originalMessageId: string
  unpushedMessage: PushedMessage
  slot: number
  target: string
}

interface MessageError {
  slot?: number
  messageId?: string
  processId?: string
  cu?: string
  error?: any
  message?: Message
  idAlreadyExisting?: string
}

interface CheckIfPushedResult {
  alreadyPushedMessages: AlreadyPushedMessage[]
  unpushedMessages: UnpushedMessage[]
  errors: MessageError[]
}

const getQuery = (tags: string, target: string): string => `
query GetMessageByTags {
  transactions(
    tags: ${tags}
    recipients:["${target}"]
  ) {
    edges {
      node {
        id
        tags {
          name
          value
        }
        recipient
        anchor
      }
    }
  }
}
`

export async function checkIfPushed(messages: Message[]): Promise<CheckIfPushedResult> {
  let alreadyPushedMessages: AlreadyPushedMessage[] = []
  let unpushedMessages: UnpushedMessage[] = []
  let errors: MessageError[] = []
  let i = 0
  for (const message of messages) {
    try {
      console.log(`${i}/${messages.length}`)
      let pushedMessages: PushedMessage[] = []
      const { messageId, processId, cu } = message
      const url = `https://${cu}/result/${messageId}?process-id=${processId}`
      console.log(`Fetching ${url}`)
      const response: { Messages?: PushedMessage[], [key: string]: any } = await fetch(url, {
          method: "GET",
      }).then(res => res.json())
      if (response.Messages && response.Messages.length > 0) {
        pushedMessages.push(...response.Messages)
      } else {
        console.log(`No messages`, response)
        errors.push({...message, error: response})
      }
      let j = 0
      for (const pushedMessage of pushedMessages) {
        console.log(`(${i}) ${j}/${pushedMessages.length}`)
        const target = pushedMessage.Target
        const tags = pushedMessage.Tags
        let tagsStr = "[\n"
        tags.forEach(t => {
          tagsStr += `{ name: "${t.name}", values: ["${t.value}"] }\n`
        })
        tagsStr += "]"
        const query = getQuery(tagsStr, target)
        const gqlResponse: GraphQLResponse = await fetch(`https://ao-search-gateway.goldsky.com/graphql`, {
          method: "POST",
          body: JSON.stringify({query})
        }).then(res => { return res.json() })
        if (gqlResponse?.data?.transactions?.edges?.length && gqlResponse.data.transactions.edges.length > 0) {
          const node = gqlResponse.data.transactions.edges[0].node
          let suResponse: { error?: any, [key: string]: any } = await fetch(`https://su-router.ao-testnet.xyz/${node.id}?process-id=${node.recipient}`)
            .then((res) => res.json())
          if(!suResponse.error) {
            alreadyPushedMessages.push({ processId, originalMessageId: messageId, pushedMessage: node, slot: j })
          } else {
            let suPidResponse: { error?: any, [key: string]: any } = await fetch(`https://su-router.ao-testnet.xyz/${node.recipient}`)
                        .then((res) => res.json())
            // this is a valid process id so there the gateway id does not exist on the su
            if(!suPidResponse.error) {
              errors.push({slot: j, ...message, error: suResponse.error, idAlreadyExisting: node.id})
            } else {
              // this is a wallet so its correct that it is just on the gateway
              alreadyPushedMessages.push({ processId, originalMessageId: messageId, pushedMessage: node, slot: j })
            }
          }
        } else {
          console.log(pushedMessage)
          const deviceTag = pushedMessage.Tags.find((t) => t.name === 'device' && t.value === 'patch@1.0') 
          if(deviceTag) {
            continue
          }
          unpushedMessages.push({ processId, originalMessageId: messageId, unpushedMessage: pushedMessage, slot: j, target })
        }
        j++
      }
    } catch (e) {
      console.log(e)
      errors.push({message})
    }
    i++
    }
  return { alreadyPushedMessages, unpushedMessages, errors }
}

const getQueryById = (id: string): string => `
query GetMessageByTags {
  transactions(
    ids: ["${id}"]
  ) {
    edges {
      node {
        id
        tags {
          name
          value
        }
        recipient
        anchor
      }
    }
  }
}
`

const CUSTOM_CU_MAP: any = {
  "qNvAoz0TgcH7DMg8BCVn8jF32QH5L6T29VjHxhHqqGE": "cu6201.ao-testnet.xyz"
}

export async function missingNonceReport(txs: string[], customCu: boolean): Promise<CheckIfPushedResult> {
    const messages: Message[] = []
    const errors: any[] = []
    for(let i = 0; i < txs.length; i++) {
      try {
        const messageId = txs[i]
        const query = getQueryById(messageId)
        const body = JSON.stringify({query})
        const gqlResponse: GraphQLResponse = await fetch(`https://ao-search-gateway.goldsky.com/graphql`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body
        }).then(res => res.json())
  
        const processId = gqlResponse?.data?.transactions?.edges[0].node.recipient
  
        if(processId) {
          messages.push({ 
            messageId, 
            processId, 
            cu: customCu && CUSTOM_CU_MAP[processId] ? CUSTOM_CU_MAP[processId] : 'cu.ao-testnet.xyz'
          })
        }
      } catch (e) {
        console.log(e)
        console.log(`Error on message ${txs[i]}`)
      }
      
      await new Promise((resolve) => setTimeout(resolve, 100))
    }

    return await checkIfPushed(messages)
}