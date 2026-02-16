interface PushMuParams {
  messageId: string
  outboxSlot: number
  pid: string
  fileResult?: boolean
  customCu?: boolean
  skipRepushChecks?: boolean
}

export const pushMu = async ({
  messageId,
  outboxSlot,
  pid,
  fileResult,
  customCu,
  skipRepushChecks
}: PushMuParams): Promise<Response> => {
  console.log(`Pushing ${process.env.REPUSHER_URL}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}${skipRepushChecks ? ' [SKIP REPUSH CHECKS]' : ''}`)

  const muUrl = process.env.REPUSHER_URL
  const url = `${muUrl}/push/${messageId}/${outboxSlot}?process-id=${pid}${customCu ? '&custom-cu=true' : ''}${fileResult ? '&result-file=true' : ''}`

  const headers: Record<string, string> = {
    'Content-Type': 'application/octet-stream',
    Accept: 'application/json'
  }

  // Add skip-repush-checks-token header if enabled
  if (skipRepushChecks && process.env.SKIP_REPUSH_CHECKS_TOKEN) {
    headers['skip-repush-checks-token'] = process.env.SKIP_REPUSH_CHECKS_TOKEN
    console.log('Adding skip-repush-checks-token header to MU request')
  }

  const response = await fetch(url, {
    method: 'POST',
    headers
  })

  return response
}
