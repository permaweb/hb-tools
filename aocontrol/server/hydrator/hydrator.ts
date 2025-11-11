import { retryWithBackoff, NetworkError, ProcessError, ErrorHandler } from './errors.js'
import { SlotLogger } from './slotLogger.js'

interface Logger {
  info: (message: string, meta?: any) => void
  debug: (message: string, meta?: any) => void
  warn: (message: string, meta?: any) => void
  error: (message: string, meta?: any) => void
  success?: (message: string, meta?: any) => void
  child: (meta: any) => Logger
  close?: () => void
  [key: string]: any
}

interface SlotLoggingConfig {
  enabled: boolean
  logFile?: string | null
  format?: 'json' | 'structured'
  includeMetrics?: boolean
}

interface RetryConfig {
  maxAttempts: number
  backoff: number
}

interface WaitingConfig {
  adaptive: boolean
  minInterval: number
  maxInterval: number
}

interface OutputConfig {
  format: 'simple' | 'verbose'
}

interface HydratorConfig {
  slotThreshold?: number
  retry: RetryConfig
  waiting: WaitingConfig
  output: OutputConfig
  slotLogging?: SlotLoggingConfig
}

interface TokenInfo {
  nonce: number
  name: string
}

interface HydrationResult {
  success: boolean
  token: string
  name?: string
  target?: number
  iterations?: number
  duration: number
  error?: string
}

/**
 * Token hydrator class that handles the core hydration logic
 */
export class TokenHydrator {
  config: HydratorConfig
  logger: Logger
  errorHandler: ErrorHandler
  abortControllers: Map<string, AbortController>
  slotLogger: SlotLogger | null

  constructor(config: HydratorConfig, logger: Logger, errorHandler: ErrorHandler) {
    this.config = config
    this.logger = logger
    this.errorHandler = errorHandler
    this.abortControllers = new Map()

    // Initialize slot logger if enabled in config
    this.slotLogger = null
    if (config.slotLogging && config.slotLogging.enabled) {
      this.slotLogger = new SlotLogger({
        logger: this.logger,
        enabled: true,
        logFile: config.slotLogging.logFile || null,
        format: config.slotLogging.format || 'json',
        includeMetrics: config.slotLogging.includeMetrics !== false
      })
    }
  }

  /**
   * Fetch token information from SU router
   * @param token - Token process ID
   * @returns Token information
   */
  async fetchTokenInfo(token: string): Promise<TokenInfo> {
    const controller = new AbortController()
    this.abortControllers.set(`info-${token}`, controller)

    try {
      const response = await retryWithBackoff(
        async () => {
          const res = await fetch(`https://su-router.ao-testnet.xyz/${token}/latest`, {
            signal: controller.signal
          })

          if (!res.ok) {
            throw new NetworkError(`Failed to fetch token info: HTTP ${res.status}`, {
              statusCode: res.status,
              token
            })
          }

          return res
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying token info fetch for ${token} (attempt ${attempt}, delay ${delay}ms)`, {
              token,
              attempt,
              delay
            })

            // Log retry attempt in slot logger if available
            if (this.slotLogger) {
              this.slotLogger.logRetry(token, 0, attempt, delay, {
                operation: 'fetchTokenInfo',
                error: error.message
              })
            }
          },
          shouldRetry: (error) => error instanceof NetworkError && (error.details.statusCode as number) >= 500
        }
      )

      const data = await response.json()

      // Extract nonce from assignment tags
      const nonce = data?.assignment?.tags?.find((t: any) => t.name === 'Nonce')?.value || 0
      const name = data?.assignment?.tags?.find((t: any) => t.name === 'Name')?.value || token

      return { nonce: parseInt(nonce), name }
    } catch (error) {
      if ((error as Error).name === 'AbortError') {
        throw new NetworkError(`Token info fetch aborted for ${token}`, { token })
      }
      throw error
    } finally {
      this.abortControllers.delete(`info-${token}`)
    }
  }

  /**
   * Import process to the node
   * @param token - Token process ID
   * @param name - Token name
   * @param nonce - Target nonce
   * @param nodeUrl - Node URL to use
   */
  async importProcess(token: string, name: string, nonce: number, nodeUrl: string): Promise<void> {
    const threshold = this.config.slotThreshold || 300000
    if (nonce <= threshold) {
      this.logger.debug(`Skipping import for ${token} - nonce ${nonce} <= ${threshold}`)
      return
    }

    const controller = new AbortController()
    this.abortControllers.set(`import-${token}`, controller)

    try {
      this.logger.info(`Importing process ${token} (${name}), target: ${nonce}`)

      await retryWithBackoff(
        async () => {
          const response = await fetch(
            `${nodeUrl}/${token}~genesis-wasm@1.0/import&process-id=${token}`,
            {
              signal: controller.signal
            }
          )

          if (!response.ok) {
            throw new NetworkError(`Process import failed: HTTP ${response.status}`, {
              statusCode: response.status,
              token,
              importFailed: true
            })
          }

          return response
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying process import for ${token} (attempt ${attempt})`, {
              token,
              attempt,
              delay
            })
          }
        }
      )

      if (this.logger.success) {
        this.logger.success(`Process ${token} imported successfully`)
      }
    } catch (error) {
      throw new ProcessError(`Failed to import process ${token}`, token, {
        importFailed: true,
        originalError: (error as Error).message
      })
    } finally {
      this.abortControllers.delete(`import-${token}`)
    }
  }

  /**
   * Trigger cron for the process
   * @param token - Token process ID
   * @param cronType - Type of cron operation ('once' or 'every')
   * @param nodeUrl - Node URL to use
   */
  async triggerCron(token: string, cronType: 'once' | 'every', nodeUrl: string): Promise<void> {
    const controller = new AbortController()
    this.abortControllers.set(`cron-${token}`, controller)

    try {
      const cronPath = `${token}~process@1.0/now`
      const url = cronType === 'once'
        ? `${nodeUrl}/${token}~cron@1.0/once?cron-path=${cronPath}`
        : `${nodeUrl}/${token}~cron@1.0/every?interval=5-minutes&cron-path=${cronPath}`

      console.log(url)

      this.logger.debug(`Triggering ${cronType} cron for ${token}`)

      await retryWithBackoff(
        async () => {
          const response = await fetch(url, {
            signal: controller.signal
          })

          if (!response.ok) {
            throw new NetworkError(`Cron trigger failed: HTTP ${response.status}`, {
              statusCode: response.status,
              token,
              cronFailed: true
            })
          }

          return response
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying cron trigger for ${token} (attempt ${attempt})`, {
              token,
              attempt,
              delay
            })
          }
        }
      )

      this.logger.debug(`Cron ${cronType} triggered successfully for ${token}`)
    } catch (error) {
      throw new ProcessError(`Failed to trigger cron for process ${token}`, token, {
        cronFailed: true,
        cronType,
        originalError: (error as Error).message
      })
    } finally {
      this.abortControllers.delete(`cron-${token}`)
    }
  }

  /**
   * Get current slot for a process
   * @param token - Token process ID
   * @param nodeUrl - Node URL to use
   * @returns Current slot number
   */
  async getCurrentSlot(token: string, nodeUrl: string): Promise<number> {
    const controller = new AbortController()
    const timeoutId = setTimeout(() => controller.abort(), 2000)
    this.abortControllers.set(`slot-${token}`, controller)

    try {
      const response = await retryWithBackoff(
        async () => {
          const res = await fetch(
            `${nodeUrl}/${token}~process@1.0/compute/at-slot`,
            {
              signal: controller.signal
            }
          )

          if (!res.ok) {
            throw new NetworkError(`Failed to get current slot: HTTP ${res.status}`, {
              statusCode: res.status,
              token
            })
          }

          return res
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          shouldRetry: (error) => error instanceof NetworkError && (error.details.statusCode as number) >= 500
        }
      )

      const slotText = await response.text()
      return parseInt(slotText)
    } catch (error) {
      if ((error as Error).name === 'AbortError') {
        throw new NetworkError(`Slot check aborted for ${token}`, { token })
      }
      throw error
    } finally {
      this.abortControllers.delete(`slot-${token}`)
    }
  }

  /**
   * Hydrate a single token process
   * @param token - Token process ID
   * @param nodeUrl - Node URL to use
   * @returns Hydration result
   */
  async hydrateToken(token: string, nodeUrl: string): Promise<HydrationResult> {
    const startTime = Date.now()
    const processLogger = this.logger.child({ processId: token })

    try {
      processLogger.info(`Starting hydration for process ${token}`)

      // Initialize slot logging for this process if enabled
      if (this.slotLogger) {
        this.slotLogger.startSlot(token, 0, 0, {
          operation: 'hydration_start',
          timestamp: new Date().toISOString()
        })
      }

      // Fetch token information
      processLogger.debug('Fetching token information')
      const { nonce, name } = await this.fetchTokenInfo(token)
      const target = parseInt(String(nonce))

      processLogger.info(`Token ${token} (${name}) - Target: ${target}`)

      // Import process if needed
      await this.importProcess(token, name, target, nodeUrl)

      // Trigger initial cron
      await this.triggerCron(token, 'once', nodeUrl)

      // Wait for process to reach target slot
      let currentSlot = 0
      let iterations = 0
      let lastLoggedSlot = 0

      while (currentSlot < target) {
        try {
          iterations++
          currentSlot = await this.getCurrentSlot(token, nodeUrl)

          // Log slot progress if changed
          if (currentSlot !== lastLoggedSlot && this.slotLogger) {
            this.slotLogger.updateProgress(token, currentSlot, target, {
              iteration: iterations,
              interval: this.config.waiting.minInterval
            })
            lastLoggedSlot = currentSlot
          }

          if (currentSlot >= target) {
            break
          }

          // Calculate next polling interval
          const nextInterval = this.config.waiting.minInterval
          processLogger.debug(`Waiting ${nextInterval}ms before next check`)

          await new Promise(resolve => setTimeout(resolve, nextInterval))

        } catch (error) {
          if (error instanceof NetworkError) {
            processLogger.warn(`Network error during slot check: ${error.message}`)

            // Log slot error in slot logger if available
            if (this.slotLogger) {
              this.slotLogger.logSlotError(token, currentSlot, error, {
                iteration: iterations,
                operation: 'slot_check'
              })
            }

            if (iterations > 5) {
              throw error
            }

            // Continue with longer interval on network errors
            await new Promise(resolve => setTimeout(resolve, this.config.waiting.maxInterval))
          } else {
            throw error
          }
        }
      }

      // Final wait and setup recurring cron
      processLogger.info(`Reached target for process ${token} (${name}), setting up recurring cron`)

      // Log final slot completion
      if (this.slotLogger) {
        this.slotLogger.completeSlot(token, target, {
          totalIterations: iterations,
          operation: 'target_reached'
        })
      }

      await new Promise(resolve => setTimeout(resolve, 10000))

      await this.triggerCron(token, 'every', nodeUrl)

      const duration = Date.now() - startTime
      if (processLogger.success) {
        processLogger.success(`Hydration completed for process ${token} (${name}) in ${duration}ms`)
      }

      return {
        success: true,
        token,
        name,
        target,
        iterations,
        duration
      }

    } catch (error) {
      const duration = Date.now() - startTime

      // Log error in slot logger if available
      if (this.slotLogger) {
        this.slotLogger.logSlotError(token, 0, error as Error, {
          duration,
          operation: 'hydration_failed'
        })
      }

      this.errorHandler.handle(error as Error, {
        processId: token,
        duration,
        operation: 'hydrateToken'
      })

      return {
        success: false,
        token,
        error: (error as Error).message,
        duration
      }
    }
  }

  /**
   * Hydrate multiple token processes
   * @param tokens - Array of token process IDs
   * @param nodeUrl - Node URL to use
   * @returns Array of hydration results
   */
  async hydrateTokens(tokens: string[], nodeUrl: string): Promise<HydrationResult[]> {
    this.logger.info(`Starting hydration for ${tokens.length} processes`)

    const results: HydrationResult[] = []

    // Process tokens sequentially to avoid overwhelming the node
    for (const token of tokens) {
      const result = await this.hydrateToken(token, nodeUrl)
      results.push(result)

      // Small delay between processes
      await new Promise(resolve => setTimeout(resolve, 100))
    }

    return results
  }

  /**
   * Abort all ongoing operations
   */
  abort() {
    this.logger.info('Aborting all operations...')

    for (const [key, controller] of this.abortControllers) {
      controller.abort()
      this.logger.debug(`Aborted operation: ${key}`)
    }

    this.abortControllers.clear()
  }

  /**
   * Cleanup resources including slot logger
   */
  cleanup() {
    this.logger.info('Cleaning up hydrator resources...')

    // Close slot logger if it exists
    if (this.slotLogger) {
      this.slotLogger.close()
      this.logger.debug('Slot logger closed')
    }

    // Close main logger file stream if it exists
    if (this.logger && this.logger.close) {
      this.logger.close()
      this.logger.debug('Main logger file stream closed')
    }
  }
}
