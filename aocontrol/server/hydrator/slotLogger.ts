interface Logger {
  debug: (message: string, meta?: any) => void
  info: (message: string, meta?: any) => void
  warn: (message: string, meta?: any) => void
  error: (message: string, meta?: any) => void
  slotStart?: (processId: string, slot: number, target: number, meta?: any) => void
  slotProgress?: (processId: string, current: number, target: number, meta?: any) => void
  slotComplete?: (processId: string, slot: number, duration: number, meta?: any) => void
  slotError?: (processId: string, slot: number, error: Error, meta?: any) => void
  close?: () => void
  [key: string]: any
}

interface SlotLoggerOptions {
  logger?: Logger
  enabled?: boolean
  logFile?: string | null
  format?: 'json' | 'structured'
  includeMetrics?: boolean
}

interface SlotMetric {
  processId: string
  slot: number
  target: number
  startTime: number
  retryCount: number
  [key: string]: any
}

interface ProcessMetrics {
  totalSlots: number
  completedSlots: number
  failedSlots: number
  totalDuration: number
  averageDuration: number
  lastSlot: number | null
}

interface ProcessSummary {
  totalSlots: number
  completedSlots: number
  failedSlots: number
  successRate: string | number
  averageDuration: number
  lastSlot: number | null
}

interface Summary {
  totalProcesses: number
  totalSlots: number
  completedSlots: number
  failedSlots: number
  averageDuration: number
  processes: { [key: string]: ProcessSummary }
}

/**
 * Dedicated slot logging utility for tracking slot-level events and performance metrics
 */
export class SlotLogger {
  logger: Logger
  enabled: boolean
  logFile: string | null
  format: 'json' | 'structured'
  includeMetrics: boolean
  slotMetrics: Map<string, SlotMetric>
  processMetrics: Map<string, ProcessMetrics>
  fileLogger?: Logger

  constructor(options: SlotLoggerOptions = {}) {
    this.logger = options.logger || console as any
    this.enabled = options.enabled !== false
    this.logFile = options.logFile || null
    this.format = options.format || 'json'
    this.includeMetrics = options.includeMetrics !== false

    // Performance tracking
    this.slotMetrics = new Map()
    this.processMetrics = new Map()

    // Create dedicated file logger if log file is specified
    // Note: fileLogger creation would need actual Logger implementation
    // For now, leaving it undefined unless explicitly provided
    if (this.logFile) {
      // Would need Logger class implementation here
      // this.fileLogger = new Logger({ ... })
    }
  }

  /**
   * Start tracking a slot
   * @param processId - Process ID
   * @param slot - Slot number
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  startSlot(processId: string, slot: number, target: number, meta: any = {}) {
    if (!this.enabled) return

    const startTime = Date.now()
    const slotKey = `${processId}-${slot}`

    // Store slot metrics
    this.slotMetrics.set(slotKey, {
      processId,
      slot,
      target,
      startTime,
      retryCount: 0,
      ...meta
    })

    // Update process metrics
    if (!this.processMetrics.has(processId)) {
      this.processMetrics.set(processId, {
        totalSlots: 0,
        completedSlots: 0,
        failedSlots: 0,
        totalDuration: 0,
        averageDuration: 0,
        lastSlot: null
      })
    }

    const processStats = this.processMetrics.get(processId)!
    processStats.totalSlots++
    processStats.lastSlot = slot

    // Log slot start
    const logData = {
      event: 'slot_start',
      processId,
      slot,
      target,
      timestamp: new Date().toISOString(),
      ...meta
    }

    if (this.logger.slotStart) {
      this.logger.slotStart(processId, slot, target, meta)
    }

    if (this.fileLogger) {
      this.fileLogger.info('Slot started', logData)
    }
  }

  /**
   * Update slot progress
   * @param processId - Process ID
   * @param current - Current slot
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  updateProgress(processId: string, current: number, target: number, meta: any = {}) {
    if (!this.enabled) return

    const progress = ((current / target) * 100).toFixed(1)

    const logData = {
      event: 'slot_progress',
      processId,
      current,
      target,
      progress: parseFloat(progress),
      timestamp: new Date().toISOString(),
      ...meta
    }

    if (this.logger.slotProgress) {
      this.logger.slotProgress(processId, current, target, meta)
    }

    if (this.fileLogger) {
      this.fileLogger.debug('Slot progress updated', logData)
    }
  }

  /**
   * Complete a slot
   * @param processId - Process ID
   * @param slot - Slot number
   * @param meta - Additional metadata
   */
  completeSlot(processId: string, slot: number, meta: any = {}) {
    if (!this.enabled) return

    const slotKey = `${processId}-${slot}`
    const slotData = this.slotMetrics.get(slotKey)

    if (!slotData) {
      this.logger.warn(`No start data found for slot ${slot} of process ${processId}`)
      return
    }

    const duration = Date.now() - slotData.startTime

    // Update process metrics
    const processStats = this.processMetrics.get(processId)
    if (processStats) {
      processStats.completedSlots++
      processStats.totalDuration += duration
      processStats.averageDuration = processStats.totalDuration / processStats.completedSlots
    }

    // Log slot completion
    const logData = {
      event: 'slot_complete',
      processId,
      slot,
      duration,
      retryCount: slotData.retryCount || 0,
      timestamp: new Date().toISOString(),
      ...meta
    }

    if (this.logger.slotComplete) {
      this.logger.slotComplete(processId, slot, duration, meta)
    }

    if (this.fileLogger) {
      this.fileLogger.info('Slot completed', logData)
    }

    // Clean up slot metrics
    this.slotMetrics.delete(slotKey)
  }

  /**
   * Log slot error
   * @param processId - Process ID
   * @param slot - Slot number
   * @param error - Error object
   * @param meta - Additional metadata
   */
  logSlotError(processId: string, slot: number, error: Error, meta: any = {}) {
    if (!this.enabled) return

    const slotKey = `${processId}-${slot}`
    const slotData = this.slotMetrics.get(slotKey)

    if (slotData) {
      slotData.retryCount = (slotData.retryCount || 0) + 1
    }

    // Update process metrics - ensure process exists
    if (!this.processMetrics.has(processId)) {
      this.processMetrics.set(processId, {
        totalSlots: 0,
        completedSlots: 0,
        failedSlots: 0,
        totalDuration: 0,
        averageDuration: 0,
        lastSlot: null
      })
    }

    const processStats = this.processMetrics.get(processId)!
    processStats.failedSlots++

    // Log slot error
    const logData = {
      event: 'slot_error',
      processId,
      slot,
      error: error.message,
      errorType: error.constructor.name,
      retryCount: slotData ? slotData.retryCount : 0,
      timestamp: new Date().toISOString(),
      ...meta
    }

    if (this.logger.slotError) {
      this.logger.slotError(processId, slot, error, meta)
    }

    if (this.fileLogger) {
      this.fileLogger.error('Slot error occurred', logData)
    }
  }

  /**
   * Log retry attempt for a slot
   * @param processId - Process ID
   * @param slot - Slot number
   * @param attempt - Attempt number
   * @param delay - Delay before retry
   * @param meta - Additional metadata
   */
  logRetry(processId: string, slot: number, attempt: number, delay: number, meta: any = {}) {
    if (!this.enabled) return

    const slotKey = `${processId}-${slot}`
    const slotData = this.slotMetrics.get(slotKey)

    if (slotData) {
      slotData.retryCount = attempt
    }

    const logData = {
      event: 'slot_retry',
      processId,
      slot,
      attempt,
      delay,
      timestamp: new Date().toISOString(),
      ...meta
    }

    this.logger.warn(`Retrying slot ${slot} for process ${processId} (attempt ${attempt}, delay ${delay}ms)`, {
      processId,
      slot,
      attempt,
      delay,
      ...meta
    })

    if (this.fileLogger) {
      this.fileLogger.warn('Slot retry attempt', logData)
    }
  }

  /**
   * Get metrics for a specific process
   * @param processId - Process ID
   * @returns Process metrics
   */
  getProcessMetrics(processId: string): ProcessMetrics | null {
    return this.processMetrics.get(processId) || null
  }

  /**
   * Get all process metrics
   * @returns Map of process metrics
   */
  getAllMetrics(): Map<string, ProcessMetrics> {
    return new Map(this.processMetrics)
  }

  /**
   * Get active slot metrics
   * @returns Array of active slot metrics
   */
  getActiveSlots(): SlotMetric[] {
    return Array.from(this.slotMetrics.values())
  }

  /**
   * Generate summary report
   * @returns Summary statistics
   */
  generateSummary(): Summary {
    const summary: Summary = {
      totalProcesses: this.processMetrics.size,
      totalSlots: 0,
      completedSlots: 0,
      failedSlots: 0,
      averageDuration: 0,
      processes: {}
    }

    for (const [processId, metrics] of this.processMetrics) {
      summary.totalSlots += metrics.totalSlots
      summary.completedSlots += metrics.completedSlots
      summary.failedSlots += metrics.failedSlots
      summary.averageDuration += metrics.averageDuration

      summary.processes[processId] = {
        totalSlots: metrics.totalSlots,
        completedSlots: metrics.completedSlots,
        failedSlots: metrics.failedSlots,
        successRate: metrics.totalSlots > 0 ?
          ((metrics.completedSlots / metrics.totalSlots) * 100).toFixed(1) : 0,
        averageDuration: metrics.averageDuration,
        lastSlot: metrics.lastSlot
      }
    }

    if (this.processMetrics.size > 0) {
      summary.averageDuration = summary.averageDuration / this.processMetrics.size
    }

    return summary
  }

  /**
   * Export metrics to JSON
   * @returns JSON string of metrics
   */
  exportMetrics(): string {
    const exportData = {
      timestamp: new Date().toISOString(),
      summary: this.generateSummary(),
      processMetrics: Object.fromEntries(this.processMetrics),
      activeSlots: this.getActiveSlots()
    }

    return JSON.stringify(exportData, null, 2)
  }

  /**
   * Reset all metrics
   */
  reset() {
    this.slotMetrics.clear()
    this.processMetrics.clear()
  }

  /**
   * Close file logger and cleanup resources
   */
  close() {
    if (this.fileLogger && this.fileLogger.close) {
      this.fileLogger.close()
    }
  }
}
