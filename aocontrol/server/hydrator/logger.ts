import { EventEmitter } from 'events'
import { createWriteStream, existsSync, mkdirSync, statSync, renameSync, unlinkSync, WriteStream } from 'fs'
import { dirname } from 'path'

// Log levels hierarchy
export const LOG_LEVELS = {
  debug: 0,
  info: 1,
  warn: 2,
  error: 3
} as const

type LogLevel = keyof typeof LOG_LEVELS

interface LoggerOptions {
  level?: LogLevel
  format?: 'simple' | 'json' | 'slot'
  colors?: boolean
  showTimestamp?: boolean
  showProcessId?: boolean
  fileOutput?: string | null
  maxFileSize?: number
  maxFiles?: number
  fileRotation?: boolean
}

interface LogMeta {
  [key: string]: any
  slot?: number
  processId?: string
  target?: number
  current?: number
  progress?: number
  duration?: number
  retryCount?: number
}

/**
 * Structured logger with multiple output formats and file streaming support
 * @extends EventEmitter
 */
export class Logger extends EventEmitter {
  level: LogLevel
  format: 'simple' | 'json' | 'slot'
  colors: boolean
  showTimestamp: boolean
  showProcessId: boolean
  fileOutput: string | null
  fileStream: WriteStream | null
  maxFileSize: number
  maxFiles: number
  currentFileSize: number
  fileRotationEnabled: boolean
  context?: LogMeta

  constructor(options: LoggerOptions = {}) {
    super()
    this.level = options.level || 'info'
    this.format = options.format || 'simple'
    this.colors = options.colors !== false
    this.showTimestamp = options.showTimestamp !== false
    this.showProcessId = options.showProcessId || false

    // File streaming configuration
    this.fileOutput = options.fileOutput || null
    this.fileStream = null
    this.maxFileSize = options.maxFileSize || 50 * 1024 * 1024 // 50MB default
    this.maxFiles = options.maxFiles || 5
    this.currentFileSize = 0
    this.fileRotationEnabled = options.fileRotation !== false

    // Initialize file stream if file output is specified
    if (this.fileOutput) {
      this._initializeFileStream()
    }
  }

  /**
   * Check if a log level should be displayed
   * @param level - Log level to check
   * @returns Whether to display the log
   */
  shouldLog(level: LogLevel): boolean {
    return LOG_LEVELS[level] >= LOG_LEVELS[this.level]
  }

  /**
   * Initialize file stream for logging
   * @private
   */
  private _initializeFileStream() {
    try {
      // Ensure directory exists
      const logDir = dirname(this.fileOutput!)
      if (!existsSync(logDir)) {
        mkdirSync(logDir, { recursive: true })
      }

      // Get current file size if file exists
      if (existsSync(this.fileOutput!)) {
        const stats = statSync(this.fileOutput!)
        this.currentFileSize = stats.size
      }

      // Create write stream
      this.fileStream = createWriteStream(this.fileOutput!, { flags: 'a' })

      this.fileStream.on('error', (error) => {
        this.emit('fileError', error)
        console.error('Logger file stream error:', error.message)
      })

    } catch (error) {
      this.emit('fileError', error)
      console.error('Failed to initialize file stream:', (error as Error).message)
    }
  }

  /**
   * Rotate log files when size limit is reached
   * @private
   */
  private _rotateFiles() {
    if (!this.fileRotationEnabled || !this.fileOutput) return

    try {
      // Close current stream
      if (this.fileStream) {
        this.fileStream.end()
        this.fileStream = null
      }

      // Rotate existing files
      for (let i = this.maxFiles - 1; i > 0; i--) {
        const oldFile = `${this.fileOutput}.${i}`
        const newFile = `${this.fileOutput}.${i + 1}`

        if (existsSync(oldFile)) {
          if (i === this.maxFiles - 1) {
            // Remove the oldest file
            unlinkSync(oldFile)
          } else {
            // Rename to next number
            renameSync(oldFile, newFile)
          }
        }
      }

      // Rename current file to .1
      if (existsSync(this.fileOutput)) {
        renameSync(this.fileOutput, `${this.fileOutput}.1`)
      }

      // Reset file size and reinitialize stream
      this.currentFileSize = 0
      this._initializeFileStream()

    } catch (error) {
      this.emit('fileError', error)
      console.error('File rotation failed:', (error as Error).message)
      // Try to reinitialize stream on error
      try {
        this._initializeFileStream()
      } catch (reinitError) {
        console.error('Stream reinitialization failed:', (reinitError as Error).message)
      }
    }
  }

  /**
   * Write log entry to file
   * @private
   * @param logEntry - Formatted log entry
   */
  private _writeToFile(logEntry: string) {
    if (!this.fileStream) return

    const logLine = logEntry + '\n'
    const logSize = Buffer.byteLength(logLine, 'utf8')

    // Check if rotation is needed before writing
    if (this.fileRotationEnabled && this.currentFileSize + logSize > this.maxFileSize) {
      this._rotateFiles()
    }

    // Write to file synchronously for testing reliability
    try {
      this.fileStream.write(logLine)
      this.currentFileSize += logSize
    } catch (error) {
      this.emit('fileError', error)
    }
  }

  /**
   * Format timestamp
   * @returns Formatted timestamp
   */
  formatTimestamp(): string {
    if (!this.showTimestamp) return ''
    const now = new Date()
    return now.toISOString().replace('T', ' ').substring(0, 19)
  }

  /**
   * Get colored level string
   * @param level - Log level
   * @returns Colored level string
   */
  getColoredLevel(level: LogLevel): string {
    if (!this.colors) return level.toUpperCase()

    // Simple color codes without chalk dependency
    const colors: Record<LogLevel, string> = {
      debug: '\x1b[90m',    // gray
      info: '\x1b[34m',     // blue
      warn: '\x1b[33m',     // yellow
      error: '\x1b[31m'     // red
    }
    const reset = '\x1b[0m'

    return `${colors[level]}${level.toUpperCase()}${reset}`
  }

  /**
   * Format log message based on output format
   * @param level - Log level
   * @param message - Log message
   * @param meta - Additional metadata
   * @returns Formatted log message
   */
  formatMessage(level: LogLevel, message: string, meta: LogMeta = {}): string {
    const timestamp = this.formatTimestamp()
    const levelStr = this.getColoredLevel(level)

    switch (this.format) {
      case 'json':
        return JSON.stringify({
          timestamp: timestamp || new Date().toISOString(),
          level,
          message,
          ...meta
        })

      case 'slot':
        // Slot-specific format for real-time logging
        const slotData = {
          timestamp: timestamp || new Date().toISOString(),
          level,
          message,
          slot: meta.slot || null,
          processId: meta.processId || null,
          target: meta.target || null,
          current: meta.current || null,
          progress: meta.progress || null,
          duration: meta.duration || null,
          retryCount: meta.retryCount || 0,
          ...meta
        }
        return JSON.stringify(slotData)

      case 'simple':
        const parts: string[] = []
        if (timestamp) {
          parts.push(`[${timestamp}]`)
        }
        parts.push(levelStr)

        // Add metadata to simple format for child logger compatibility
        const metaKeys = Object.keys(meta)
        for (const key of metaKeys) {
          if (!['slot', 'processId', 'target', 'current', 'progress', 'duration', 'retryCount'].includes(key)) {
            parts.push(`[${key}:${meta[key]}]`)
          }
        }

        if (this.showProcessId && meta.processId) {
          parts.push(`[${meta.processId}]`)
        }
        if (meta.slot) {
          parts.push(`[Slot:${meta.slot}]`)
        }
        parts.push(message)
        return parts.join(' ')

      default:
        return message
    }
  }

  /**
   * Log a debug message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  debug(message: string, meta: LogMeta = {}) {
    if (this.shouldLog('debug')) {
      const formattedMessage = this.formatMessage('debug', message, meta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log an info message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  info(message: string, meta: LogMeta = {}) {
    if (this.shouldLog('info')) {
      const formattedMessage = this.formatMessage('info', message, meta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log a warning message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  warn(message: string, meta: LogMeta = {}) {
    if (this.shouldLog('warn')) {
      const formattedMessage = this.formatMessage('warn', message, meta)
      console.warn(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log an error message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  error(message: string, meta: LogMeta = {}) {
    if (this.shouldLog('error')) {
      const formattedMessage = this.formatMessage('error', message, meta)
      console.error(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log a success message
   * @param message - Success message
   * @param meta - Additional metadata
   */
  success(message: string, meta: LogMeta = {}) {
    if (this.shouldLog('info')) {
      const successMessage = `✓ ${message}`
      const formattedMessage = this.formatMessage('info', successMessage, meta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log a step in a process
   * @param step - Step description
   * @param current - Current step number
   * @param total - Total number of steps
   * @param meta - Additional metadata
   */
  step(step: string, current: number, total: number, meta: LogMeta = {}) {
    if (this.shouldLog('info')) {
      const stepMessage = `[${current}/${total}] ${step}`
      const formattedMessage = this.formatMessage('info', stepMessage, { ...meta, current, total })
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log slot start event
   * @param processId - Process ID
   * @param slot - Slot number
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  slotStart(processId: string, slot: number, target: number, meta: LogMeta = {}) {
    if (this.shouldLog('info')) {
      const message = `Slot ${slot} started for process ${processId}, target: ${target}`
      const slotMeta: LogMeta = {
        processId,
        slot,
        target,
        event: 'slot_start',
        ...meta
      }
      const formattedMessage = this.formatMessage('info', message, slotMeta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log slot progress update
   * @param processId - Process ID
   * @param current - Current slot
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  slotProgress(processId: string, current: number, target: number, meta: LogMeta = {}) {
    if (this.shouldLog('debug')) {
      const progress = ((current / target) * 100).toFixed(1)
      const message = `Slot progress: ${current}/${target} (${progress}%) for process ${processId}`
      const slotMeta: LogMeta = {
        processId,
        current,
        target,
        progress: parseFloat(progress),
        event: 'slot_progress',
        ...meta
      }
      const formattedMessage = this.formatMessage('debug', message, slotMeta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log slot completion event
   * @param processId - Process ID
   * @param slot - Completed slot
   * @param duration - Duration in milliseconds
   * @param meta - Additional metadata
   */
  slotComplete(processId: string, slot: number, duration: number, meta: LogMeta = {}) {
    if (this.shouldLog('info')) {
      const message = `Slot ${slot} completed for process ${processId} in ${duration}ms`
      const slotMeta: LogMeta = {
        processId,
        slot,
        duration,
        event: 'slot_complete',
        ...meta
      }
      const formattedMessage = this.formatMessage('info', message, slotMeta)
      console.log(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Log slot error event
   * @param processId - Process ID
   * @param slot - Slot number
   * @param error - Error object
   * @param meta - Additional metadata
   */
  slotError(processId: string, slot: number, error: Error, meta: LogMeta = {}) {
    if (this.shouldLog('error')) {
      const message = `Slot ${slot} error for process ${processId}: ${error.message}`
      const slotMeta: LogMeta = {
        processId,
        slot,
        error: error.message,
        errorType: error.constructor.name,
        event: 'slot_error',
        ...meta
      }
      const formattedMessage = this.formatMessage('error', message, slotMeta)
      console.error(formattedMessage)
      this._writeToFile(formattedMessage)
    }
  }

  /**
   * Create a child logger with additional context
   * @param context - Additional context to include in all logs
   * @returns Child logger instance
   */
  child(context: LogMeta): Logger {
    const childLogger = new Logger({
      level: this.level,
      format: this.format,
      colors: this.colors,
      showTimestamp: this.showTimestamp,
      showProcessId: this.showProcessId,
      fileOutput: this.fileOutput,
      maxFileSize: this.maxFileSize,
      maxFiles: this.maxFiles,
      fileRotation: this.fileRotationEnabled
    })

    // Store context for use in formatMessage
    childLogger.context = { ...context }

    // Override formatMessage to include context
    const originalFormat = childLogger.formatMessage.bind(childLogger)
    childLogger.formatMessage = (level: LogLevel, message: string, meta: LogMeta = {}) => {
      const mergedMeta = { ...childLogger.context, ...meta }
      return originalFormat(level, message, mergedMeta)
    }

    return childLogger
  }

  /**
   * Close file stream and cleanup resources
   */
  close() {
    if (this.fileStream) {
      this.fileStream.end()
      this.fileStream = null
    }
  }

  /**
   * Get current file size and rotation info
   * @returns File statistics
   */
  getFileStats() {
    if (!this.fileOutput) return null

    return {
      currentFile: this.fileOutput,
      currentSize: this.currentFileSize,
      maxSize: this.maxFileSize,
      rotationEnabled: this.fileRotationEnabled,
      maxFiles: this.maxFiles
    }
  }
}

// Create default logger instance
export const defaultLogger = new Logger()
