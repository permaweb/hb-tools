interface PushMuParams {
  messageId: string
  outboxSlot: number
  pid: string
  fileResult?: boolean
  customCu?: boolean
  skipRepushChecksToken?: string
}

export const pushMu = async ({
  messageId,
  outboxSlot,
  pid,
  fileResult,
  customCu,
  skipRepushChecksToken
}: PushMuParams): Promise<Response> => {
  console.log(`Pushing ${process.env.REPUSHER_URL}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}${skipRepushChecksToken ? ' [SKIP REPUSH CHECKS]' : ''}`)

  const muUrl = process.env.REPUSHER_URL
  const url = `${muUrl}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}`

  const headers: Record<string, string> = {
    'Content-Type': 'application/octet-stream',
    Accept: 'application/json'
  }

  // Add skip-repush-checks-token header if provided
  if (skipRepushChecksToken) {
    headers['skip-repush-checks-token'] = skipRepushChecksToken
    console.log('Adding skip-repush-checks-token header to MU request')
  }

  const response = await fetch(url, {
    method: 'POST',
    headers
  })

  return response
}
