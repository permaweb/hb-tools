/**
 * Custom error classes for hydrate-tokens CLI
 */

interface ErrorDetails {
  [key: string]: any
}

export class HydrateTokensError extends Error {
  code: string
  details: ErrorDetails
  timestamp: string

  constructor(message: string, code: string, details: ErrorDetails = {}) {
    super(message)
    this.name = 'HydrateTokensError'
    this.code = code
    this.details = details
    this.timestamp = new Date().toISOString()
  }

  toJSON() {
    return {
      name: this.name,
      message: this.message,
      code: this.code,
      details: this.details,
      timestamp: this.timestamp,
      stack: this.stack
    }
  }
}

export class ConfigurationError extends HydrateTokensError {
  constructor(message: string, details: ErrorDetails = {}) {
    super(message, 'CONFIG_ERROR', details)
    this.name = 'ConfigurationError'
  }
}

export class NetworkError extends HydrateTokensError {
  constructor(message: string, details: ErrorDetails = {}) {
    super(message, 'NETWORK_ERROR', details)
    this.name = 'NetworkError'
  }
}

export class ValidationError extends HydrateTokensError {
  constructor(message: string, details: ErrorDetails = {}) {
    super(message, 'VALIDATION_ERROR', details)
    this.name = 'ValidationError'
  }
}

export class ProcessError extends HydrateTokensError {
  processId: string

  constructor(message: string, processId: string, details: ErrorDetails = {}) {
    super(message, 'PROCESS_ERROR', { processId, ...details })
    this.name = 'ProcessError'
    this.processId = processId
  }
}

interface ErrorLogEntry {
  error: {
    name: string
    message: string
    code?: string
    details?: ErrorDetails
    stack?: string
  }
  context: ErrorDetails
  timestamp: string
}

interface Logger {
  error: (message: string, meta?: any) => void
  info: (message: string, meta?: any) => void
  [key: string]: any
}

/**
 * Error handler for the application
 */
export class ErrorHandler {
  logger: Logger
  errorLog: ErrorLogEntry[]

  constructor(logger: Logger) {
    this.logger = logger
    this.errorLog = []
  }

  /**
   * Handle an error
   * @param error - Error to handle
   * @param context - Additional context
   */
  handle(error: Error | HydrateTokensError, context: ErrorDetails = {}) {
    this.errorLog.push({
      error: (error as HydrateTokensError).toJSON ? (error as HydrateTokensError).toJSON() : {
        name: error.name,
        message: error.message,
        stack: error.stack
      },
      context,
      timestamp: new Date().toISOString()
    })

    // Log the error
    if (error instanceof HydrateTokensError) {
      this.logger.error(`${error.name}: ${error.message}`, {
        code: error.code,
        details: error.details,
        ...context
      })
    } else {
      this.logger.error(`Unexpected error: ${error.message}`, {
        name: error.name,
        stack: error.stack,
        ...context
      })
    }

    // Handle specific error types
    if (error instanceof ConfigurationError) {
      this.handleConfigurationError(error)
    } else if (error instanceof NetworkError) {
      this.handleNetworkError(error)
    } else if (error instanceof ValidationError) {
      this.handleValidationError(error)
    } else if (error instanceof ProcessError) {
      this.handleProcessError(error)
    }
  }

  /**
   * Handle configuration errors
   * @param error - Configuration error
   */
  handleConfigurationError(error: ConfigurationError) {
    this.logger.error('Configuration error occurred. Please check your configuration.')

    if (error.details.missingFile) {
      this.logger.info('Tip: Create a configuration file or use command-line arguments.')
    } else if (error.details.validationErrors) {
      this.logger.info('Tip: Fix the validation errors in your configuration.')
    }
  }

  /**
   * Handle network errors
   * @param error - Network error
   */
  handleNetworkError(error: NetworkError) {
    this.logger.error('Network error occurred. Please check your connection and node URL.')

    if (error.details.statusCode) {
      this.logger.info(`Tip: HTTP ${error.details.statusCode} - Check if the node is running and accessible.`)
    } else if (error.details.timeout) {
      this.logger.info('Tip: Connection timeout - The node might be overloaded or unreachable.')
    }
  }

  /**
   * Handle validation errors
   * @param error - Validation error
   */
  handleValidationError(error: ValidationError) {
    this.logger.error('Validation error occurred. Please check your input data.')

    if (error.details.field) {
      this.logger.info(`Tip: Invalid value for field '${error.details.field}'.`)
    }
  }

  /**
   * Handle process-specific errors
   * @param error - Process error
   */
  handleProcessError(error: ProcessError) {
    this.logger.error(`Process ${error.processId} encountered an error.`)

    if (error.details.importFailed) {
      this.logger.info('Tip: Process import failed - Check if the process ID is valid.')
    } else if (error.details.cronFailed) {
      this.logger.info('Tip: Cron setup failed - The process might not support cron operations.')
    }
  }

  /**
   * Get error summary
   * @returns Error summary
   */
  getErrorSummary() {
    const totalErrors = this.errorLog.length
    const errorsByType: { [key: string]: number } = {}
    const errorsByCode: { [key: string]: number } = {}

    this.errorLog.forEach(entry => {
      const error = entry.error
      const type = error.name || 'UnknownError'
      const code = error.code || 'UNKNOWN'

      errorsByType[type] = (errorsByType[type] || 0) + 1
      errorsByCode[code] = (errorsByCode[code] || 0) + 1
    })

    return {
      totalErrors,
      errorsByType,
      errorsByCode,
      recentErrors: this.errorLog.slice(-5)
    }
  }

  /**
   * Export error log
   * @param format - Export format (json, csv)
   * @returns Formatted error log
   */
  exportErrorLog(format: 'json' | 'csv' = 'json'): string {
    switch (format) {
      case 'json':
        return JSON.stringify(this.errorLog, null, 2)

      case 'csv':
        const headers = ['Timestamp', 'Error Type', 'Error Code', 'Message', 'Details']
        const rows = this.errorLog.map(entry => [
          entry.timestamp,
          entry.error.name || 'Unknown',
          entry.error.code || 'UNKNOWN',
          entry.error.message,
          JSON.stringify(entry.error.details || {})
        ])
        return [headers, ...rows].map(row => row.join(',')).join('\n')

      default:
        throw new Error(`Unsupported export format: ${format}`)
    }
  }

  /**
   * Clear error log
   */
  clear() {
    this.errorLog = []
  }
}

interface RetryOptions {
  maxAttempts?: number
  backoff?: number
  maxBackoff?: number
  onRetry?: (error: Error, attempt: number, delay: number) => void
  shouldRetry?: (error: Error) => boolean
}

/**
 * Retry utility with exponential backoff
 * @param fn - Function to retry
 * @param options - Retry options
 * @returns Result of the function
 */
export async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  options: RetryOptions = {}
): Promise<T> {
  const {
    maxAttempts = 3,
    backoff = 1000,
    maxBackoff = 30000,
    onRetry = null,
    shouldRetry = () => true
  } = options

  let lastError: Error | undefined

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn()
    } catch (error) {
      lastError = error as Error

      if (attempt === maxAttempts || !shouldRetry(lastError)) {
        throw error
      }

      const delay = Math.min(backoff * Math.pow(2, attempt - 1), maxBackoff)

      if (onRetry) {
        onRetry(lastError, attempt, delay)
      }

      await new Promise(resolve => setTimeout(resolve, delay))
    }
  }

  throw lastError
}

/**
 * Validate process ID format
 * @param processId - Process ID to validate
 * @returns Whether the process ID is valid
 */
export function validateProcessId(processId: string): boolean {
  // Basic validation - can be extended based on actual AO process ID format
  return typeof processId === 'string' &&
         processId.length > 0 &&
         processId.length <= 256 &&
         /^[a-zA-Z0-9_-]+$/.test(processId)
}

/**
 * Validate URL format
 * @param url - URL to validate
 * @returns Whether the URL is valid
 */
export function validateUrl(url: string): boolean {
  try {
    new URL(url)
    return true
  } catch {
    return false
  }
}
