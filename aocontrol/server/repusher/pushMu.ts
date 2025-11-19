interface PushMuParams {
  messageId: string
  outboxSlot: number
  pid: string
  fileResult?: boolean
  customCu?: boolean
}

export const pushMu = async ({
  messageId,
  outboxSlot,
  pid,
  fileResult,
  customCu
}: PushMuParams): Promise<Response> => {
  console.log(`Pushing ${process.env.REPUSHER_URL}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}`)

  const muUrl = process.env.REPUSHER_URL
  const url = `${muUrl}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}`
  const response = await fetch(
    url,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/octet-stream',
        Accept: 'application/json'
      }
    }
  )

  return response
}
