// server/worker.ts
import { parentPort } from "worker_threads";

// server/db.ts
import pg from "pg";
var { Pool } = pg;
var QUERYABLE_HYDRATION_FIELDS = ["url"];
var QUERYABLE_REPUSH_FIELDS = ["messageId"];
var createProcesses = async (pool) => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS processes(
      id SERIAL PRIMARY KEY,
      processId TEXT UNIQUE NOT NULL,
      timestamp BIGINT NOT NULL
    );
  `);
};
var createHydrations = async (pool) => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS hydrations(
      id SERIAL PRIMARY KEY,
      processId TEXT NOT NULL,
      url TEXT NOT NULL,
      status TEXT NOT NULL CHECK(status IN ('INIT', 'REQUESTSENT', 'NOPROGRESS', 'PROGRESS', 'HYDRATED')),
      timestamp BIGINT NOT NULL,
      UNIQUE(processId, url),
      FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
    );
  `);
};
var createRepushes = async (pool) => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS repushes(
      id SERIAL PRIMARY KEY,
      processId TEXT NOT NULL,
      messageId TEXT NOT NULL,
      status TEXT NOT NULL CHECK(status IN ('INIT', 'REPUSHED', 'CANNOTPUSHALL')),
      reasons TEXT,
      timestamp BIGINT NOT NULL,
      UNIQUE(processId, messageId),
      FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
    );
  `);
};
var createAuthTokens = async (pool) => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS auth_token(
      id SERIAL PRIMARY KEY,
      token TEXT NOT NULL,
      type TEXT NOT NULL CHECK(type IN ('ADMIN', 'USER')),
      timestamp BIGINT NOT NULL,
      UNIQUE(token)
    );
  `);
};
var createPermissions = async (pool) => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS permissions(
      id SERIAL PRIMARY KEY,
      token TEXT NOT NULL,
      processId TEXT NOT NULL,
      label TEXT NOT NULL,
      timestamp BIGINT NOT NULL,
      UNIQUE(token, processId),
      FOREIGN KEY (token) REFERENCES auth_token(token) ON DELETE CASCADE,
      FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
    );
  `);
};
async function createPostgresClient({ url }) {
  const pool = new Pool({
    connectionString: url
  });
  try {
    await pool.query("SELECT NOW()");
    console.log("Successfully connected to PostgreSQL database");
  } catch (error) {
    console.error("Failed to connect to PostgreSQL:", error);
    throw error;
  }
  await Promise.resolve().then(() => createProcesses(pool)).then(() => createHydrations(pool)).then(() => createRepushes(pool)).then(() => createAuthTokens(pool)).then(() => createPermissions(pool));
  const client = {
    query: async ({ sql, parameters }) => {
      const result = await pool.query(sql, parameters);
      return result.rows;
    },
    run: async ({ sql, parameters }) => {
      const result = await pool.query(sql, parameters);
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    transaction: async (statements) => {
      const client2 = await pool.connect();
      try {
        await client2.query("BEGIN");
        const results = [];
        for (const { sql, parameters } of statements) {
          const result = await client2.query(sql, parameters);
          results.push({
            changes: result.rowCount || 0,
            lastInsertRowid: result.rows[0]?.id
          });
        }
        await client2.query("COMMIT");
        return results;
      } catch (error) {
        await client2.query("ROLLBACK");
        throw error;
      } finally {
        client2.release();
      }
    },
    saveProcess: async (processId, timestamp = Date.now()) => {
      const result = await pool.query(
        "INSERT INTO processes (processId, timestamp) VALUES ($1, $2) ON CONFLICT (processId) DO NOTHING RETURNING id",
        [processId, timestamp]
      );
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    saveHydration: async (processId, url2, status, timestamp = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO hydrations (processId, url, status, timestamp)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (processId, url)
         DO UPDATE SET status = EXCLUDED.status, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [processId, url2, status, timestamp]
      );
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    saveRepush: async (processId, messageId, status, reason, timestamp = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO repushes (processId, messageId, status, reasons, timestamp)
         VALUES ($1, $2, $3, $4, $5)
         ON CONFLICT (processId, messageId)
         DO UPDATE SET status = EXCLUDED.status, reasons = EXCLUDED.reasons, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [processId, messageId, status, reason, timestamp]
      );
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    saveAuthToken: async (token, type, timestamp = Date.now()) => {
      const result = await pool.query(
        "INSERT INTO auth_token (token, type, timestamp) VALUES ($1, $2, $3) ON CONFLICT (token) DO NOTHING RETURNING id",
        [token, type, timestamp]
      );
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    savePermission: async (token, processId, label, timestamp = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO permissions (token, processId, label, timestamp)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (token, processId)
         DO UPDATE SET label = EXCLUDED.label, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [token, processId, label, timestamp]
      );
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      };
    },
    getAuthToken: async (token) => {
      const result = await pool.query(
        "SELECT id, token, type, timestamp FROM auth_token WHERE token = $1",
        [token]
      );
      return result.rows[0];
    },
    getPermissionsByToken: async (token) => {
      const result = await pool.query(
        "SELECT id, token, processId, label, timestamp FROM permissions WHERE token = $1",
        [token]
      );
      return result.rows;
    },
    getAllProcessIds: async (pagination) => {
      let sql = "SELECT processId FROM processes";
      const params = [];
      if (pagination?.cursor) {
        sql += " WHERE timestamp > $1";
        params.push(pagination.cursor);
      }
      sql += " ORDER BY timestamp";
      if (pagination?.limit !== void 0) {
        const limitIndex = params.length + 1;
        sql += ` LIMIT $${limitIndex}`;
        params.push(pagination.limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => row.processid);
    },
    getProcessIdsByQuery: async (query, pagination) => {
      const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(" OR ");
      let sql = `SELECT DISTINCT p.processId
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`;
      const params = QUERYABLE_HYDRATION_FIELDS.map(() => query);
      if (pagination?.cursor) {
        const cursorIndex = params.length + 1;
        sql += ` AND p.timestamp > $${cursorIndex}`;
        params.push(pagination.cursor);
      }
      sql += " ORDER BY p.timestamp";
      if (pagination?.limit !== void 0) {
        const limitIndex = params.length + 1;
        sql += ` LIMIT $${limitIndex}`;
        params.push(pagination.limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => row.processid);
    },
    getAllProcessesWithTimestamp: async (pagination) => {
      let sql = "SELECT processId, timestamp FROM processes";
      const params = [];
      if (pagination?.cursor) {
        sql += " WHERE timestamp > $1";
        params.push(pagination.cursor);
      }
      sql += " ORDER BY timestamp";
      if (pagination?.limit !== void 0) {
        const limitIndex = params.length + 1;
        sql += ` LIMIT $${limitIndex}`;
        params.push(pagination.limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => ({
        processId: row.processid,
        timestamp: parseInt(row.timestamp)
      }));
    },
    getProcessesByQueryWithTimestamp: async (query, pagination) => {
      const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(" OR ");
      let sql = `SELECT DISTINCT p.processId, p.timestamp
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`;
      const params = QUERYABLE_HYDRATION_FIELDS.map(() => query);
      if (pagination?.cursor) {
        const cursorIndex = params.length + 1;
        sql += ` AND p.timestamp > $${cursorIndex}`;
        params.push(pagination.cursor);
      }
      sql += " ORDER BY p.timestamp";
      if (pagination?.limit !== void 0) {
        const limitIndex = params.length + 1;
        sql += ` LIMIT $${limitIndex}`;
        params.push(pagination.limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => ({
        processId: row.processid,
        timestamp: parseInt(row.timestamp)
      }));
    },
    getHydrationsByProcessId: async (processId, query) => {
      if (query) {
        const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 2}`).join(" OR ");
        const sql = `SELECT url, status FROM hydrations WHERE processId = $1 AND (${conditions})`;
        const params = [processId, ...QUERYABLE_HYDRATION_FIELDS.map(() => query)];
        const result2 = await pool.query(sql, params);
        return result2.rows;
      }
      const result = await pool.query("SELECT url, status FROM hydrations WHERE processId = $1", [processId]);
      return result.rows;
    },
    getHydrationsByStatus: async (status, limit) => {
      let sql = "SELECT processId, url, status FROM hydrations WHERE status = $1 ORDER BY timestamp";
      const params = [status];
      if (limit) {
        sql += " LIMIT $2";
        params.push(limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => ({
        processId: row.processid,
        url: row.url,
        status: row.status
      }));
    },
    getRepushes: async (query, pagination) => {
      let sql = "SELECT processId, messageId, status, reasons, timestamp FROM repushes";
      const params = [];
      if (query) {
        const conditions = QUERYABLE_REPUSH_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(" OR ");
        sql += ` WHERE ${conditions}`;
        params.push(...QUERYABLE_REPUSH_FIELDS.map(() => query));
      }
      if (pagination?.cursor) {
        const cursorIndex = params.length + 1;
        sql += query ? ` AND timestamp > $${cursorIndex}` : ` WHERE timestamp > $${cursorIndex}`;
        params.push(pagination.cursor);
      }
      sql += " ORDER BY timestamp";
      if (pagination?.limit !== void 0) {
        const limitIndex = params.length + 1;
        sql += ` LIMIT $${limitIndex}`;
        params.push(pagination.limit);
      }
      const result = await pool.query(sql, params);
      return result.rows.map((row) => ({
        processId: row.processid,
        messageId: row.messageid,
        status: row.status,
        reasons: row.reasons,
        timestamp: parseInt(row.timestamp)
      }));
    },
    getStatusCounts: async () => {
      const result = await pool.query("SELECT status, COUNT(*) as count FROM hydrations GROUP BY status");
      return result.rows.reduce((acc, row) => {
        acc[row.status] = parseInt(row.count);
        return acc;
      }, {});
    },
    getRepushStatusCounts: async () => {
      const result = await pool.query("SELECT status, COUNT(*) as count FROM repushes GROUP BY status");
      return result.rows.reduce((acc, row) => {
        acc[row.status] = parseInt(row.count);
        return acc;
      }, {});
    },
    deleteProcess: async (processId) => {
      const result = await pool.query("DELETE FROM processes WHERE processId = $1", [processId]);
      return {
        changes: result.rowCount || 0
      };
    },
    pool
  };
  if (process.env.ADMIN_TOKEN) {
    await client.saveAuthToken(process.env.ADMIN_TOKEN, "ADMIN");
    console.log("Admin token initialized");
  }
  if (process.env.USER_PERMISSIONS) {
    try {
      const userPermissions = JSON.parse(process.env.USER_PERMISSIONS);
      for (const [token, processIds] of Object.entries(userPermissions)) {
        await client.saveAuthToken(token, "USER");
        for (const processId of processIds) {
          await client.saveProcess(processId);
          await client.savePermission(token, processId, "RESOLVE_UNPUSHED");
        }
        console.log(`User token initialized with ${processIds.length} process permissions`);
      }
    } catch (error) {
      console.error("Failed to parse USER_PERMISSIONS:", error);
    }
  }
  return client;
}

// server/hydrator/errors.ts
var HydrateTokensError = class extends Error {
  constructor(message, code, details = {}) {
    super(message);
    this.name = "HydrateTokensError";
    this.code = code;
    this.details = details;
    this.timestamp = (/* @__PURE__ */ new Date()).toISOString();
  }
  toJSON() {
    return {
      name: this.name,
      message: this.message,
      code: this.code,
      details: this.details,
      timestamp: this.timestamp,
      stack: this.stack
    };
  }
};
var ConfigurationError = class extends HydrateTokensError {
  constructor(message, details = {}) {
    super(message, "CONFIG_ERROR", details);
    this.name = "ConfigurationError";
  }
};
var NetworkError = class extends HydrateTokensError {
  constructor(message, details = {}) {
    super(message, "NETWORK_ERROR", details);
    this.name = "NetworkError";
  }
};
var ValidationError = class extends HydrateTokensError {
  constructor(message, details = {}) {
    super(message, "VALIDATION_ERROR", details);
    this.name = "ValidationError";
  }
};
var ProcessError = class extends HydrateTokensError {
  constructor(message, processId, details = {}) {
    super(message, "PROCESS_ERROR", { processId, ...details });
    this.name = "ProcessError";
    this.processId = processId;
  }
};
var ErrorHandler = class {
  constructor(logger2) {
    this.logger = logger2;
    this.errorLog = [];
  }
  /**
   * Handle an error
   * @param error - Error to handle
   * @param context - Additional context
   */
  handle(error, context = {}) {
    this.errorLog.push({
      error: error.toJSON ? error.toJSON() : {
        name: error.name,
        message: error.message,
        stack: error.stack
      },
      context,
      timestamp: (/* @__PURE__ */ new Date()).toISOString()
    });
    if (error instanceof HydrateTokensError) {
      this.logger.error(`${error.name}: ${error.message}`, {
        code: error.code,
        details: error.details,
        ...context
      });
    } else {
      this.logger.error(`Unexpected error: ${error.message}`, {
        name: error.name,
        stack: error.stack,
        ...context
      });
    }
    if (error instanceof ConfigurationError) {
      this.handleConfigurationError(error);
    } else if (error instanceof NetworkError) {
      this.handleNetworkError(error);
    } else if (error instanceof ValidationError) {
      this.handleValidationError(error);
    } else if (error instanceof ProcessError) {
      this.handleProcessError(error);
    }
  }
  /**
   * Handle configuration errors
   * @param error - Configuration error
   */
  handleConfigurationError(error) {
    this.logger.error("Configuration error occurred. Please check your configuration.");
    if (error.details.missingFile) {
      this.logger.info("Tip: Create a configuration file or use command-line arguments.");
    } else if (error.details.validationErrors) {
      this.logger.info("Tip: Fix the validation errors in your configuration.");
    }
  }
  /**
   * Handle network errors
   * @param error - Network error
   */
  handleNetworkError(error) {
    this.logger.error("Network error occurred. Please check your connection and node URL.");
    if (error.details.statusCode) {
      this.logger.info(`Tip: HTTP ${error.details.statusCode} - Check if the node is running and accessible.`);
    } else if (error.details.timeout) {
      this.logger.info("Tip: Connection timeout - The node might be overloaded or unreachable.");
    }
  }
  /**
   * Handle validation errors
   * @param error - Validation error
   */
  handleValidationError(error) {
    this.logger.error("Validation error occurred. Please check your input data.");
    if (error.details.field) {
      this.logger.info(`Tip: Invalid value for field '${error.details.field}'.`);
    }
  }
  /**
   * Handle process-specific errors
   * @param error - Process error
   */
  handleProcessError(error) {
    this.logger.error(`Process ${error.processId} encountered an error.`);
    if (error.details.importFailed) {
      this.logger.info("Tip: Process import failed - Check if the process ID is valid.");
    } else if (error.details.cronFailed) {
      this.logger.info("Tip: Cron setup failed - The process might not support cron operations.");
    }
  }
  /**
   * Get error summary
   * @returns Error summary
   */
  getErrorSummary() {
    const totalErrors = this.errorLog.length;
    const errorsByType = {};
    const errorsByCode = {};
    this.errorLog.forEach((entry) => {
      const error = entry.error;
      const type = error.name || "UnknownError";
      const code = error.code || "UNKNOWN";
      errorsByType[type] = (errorsByType[type] || 0) + 1;
      errorsByCode[code] = (errorsByCode[code] || 0) + 1;
    });
    return {
      totalErrors,
      errorsByType,
      errorsByCode,
      recentErrors: this.errorLog.slice(-5)
    };
  }
  /**
   * Export error log
   * @param format - Export format (json, csv)
   * @returns Formatted error log
   */
  exportErrorLog(format = "json") {
    switch (format) {
      case "json":
        return JSON.stringify(this.errorLog, null, 2);
      case "csv":
        const headers = ["Timestamp", "Error Type", "Error Code", "Message", "Details"];
        const rows = this.errorLog.map((entry) => [
          entry.timestamp,
          entry.error.name || "Unknown",
          entry.error.code || "UNKNOWN",
          entry.error.message,
          JSON.stringify(entry.error.details || {})
        ]);
        return [headers, ...rows].map((row) => row.join(",")).join("\n");
      default:
        throw new Error(`Unsupported export format: ${format}`);
    }
  }
  /**
   * Clear error log
   */
  clear() {
    this.errorLog = [];
  }
};
async function retryWithBackoff(fn, options = {}) {
  const {
    maxAttempts = 3,
    backoff = 1e3,
    maxBackoff = 3e4,
    onRetry = null,
    shouldRetry = () => true
  } = options;
  let lastError;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;
      if (attempt === maxAttempts || !shouldRetry(lastError)) {
        throw error;
      }
      const delay = Math.min(backoff * Math.pow(2, attempt - 1), maxBackoff);
      if (onRetry) {
        onRetry(lastError, attempt, delay);
      }
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
  }
  throw lastError;
}

// server/hydrator/slotLogger.ts
var SlotLogger = class {
  constructor(options = {}) {
    this.logger = options.logger || console;
    this.enabled = options.enabled !== false;
    this.logFile = options.logFile || null;
    this.format = options.format || "json";
    this.includeMetrics = options.includeMetrics !== false;
    this.slotMetrics = /* @__PURE__ */ new Map();
    this.processMetrics = /* @__PURE__ */ new Map();
    if (this.logFile) {
    }
  }
  /**
   * Start tracking a slot
   * @param processId - Process ID
   * @param slot - Slot number
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  startSlot(processId, slot, target, meta = {}) {
    if (!this.enabled) return;
    const startTime = Date.now();
    const slotKey = `${processId}-${slot}`;
    this.slotMetrics.set(slotKey, {
      processId,
      slot,
      target,
      startTime,
      retryCount: 0,
      ...meta
    });
    if (!this.processMetrics.has(processId)) {
      this.processMetrics.set(processId, {
        totalSlots: 0,
        completedSlots: 0,
        failedSlots: 0,
        totalDuration: 0,
        averageDuration: 0,
        lastSlot: null
      });
    }
    const processStats = this.processMetrics.get(processId);
    processStats.totalSlots++;
    processStats.lastSlot = slot;
    const logData = {
      event: "slot_start",
      processId,
      slot,
      target,
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      ...meta
    };
    if (this.logger.slotStart) {
      this.logger.slotStart(processId, slot, target, meta);
    }
    if (this.fileLogger) {
      this.fileLogger.info("Slot started", logData);
    }
  }
  /**
   * Update slot progress
   * @param processId - Process ID
   * @param current - Current slot
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  updateProgress(processId, current, target, meta = {}) {
    if (!this.enabled) return;
    const progress = (current / target * 100).toFixed(1);
    const logData = {
      event: "slot_progress",
      processId,
      current,
      target,
      progress: parseFloat(progress),
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      ...meta
    };
    if (this.logger.slotProgress) {
      this.logger.slotProgress(processId, current, target, meta);
    }
    if (this.fileLogger) {
      this.fileLogger.debug("Slot progress updated", logData);
    }
  }
  /**
   * Complete a slot
   * @param processId - Process ID
   * @param slot - Slot number
   * @param meta - Additional metadata
   */
  completeSlot(processId, slot, meta = {}) {
    if (!this.enabled) return;
    const slotKey = `${processId}-${slot}`;
    const slotData = this.slotMetrics.get(slotKey);
    if (!slotData) {
      this.logger.warn(`No start data found for slot ${slot} of process ${processId}`);
      return;
    }
    const duration = Date.now() - slotData.startTime;
    const processStats = this.processMetrics.get(processId);
    if (processStats) {
      processStats.completedSlots++;
      processStats.totalDuration += duration;
      processStats.averageDuration = processStats.totalDuration / processStats.completedSlots;
    }
    const logData = {
      event: "slot_complete",
      processId,
      slot,
      duration,
      retryCount: slotData.retryCount || 0,
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      ...meta
    };
    if (this.logger.slotComplete) {
      this.logger.slotComplete(processId, slot, duration, meta);
    }
    if (this.fileLogger) {
      this.fileLogger.info("Slot completed", logData);
    }
    this.slotMetrics.delete(slotKey);
  }
  /**
   * Log slot error
   * @param processId - Process ID
   * @param slot - Slot number
   * @param error - Error object
   * @param meta - Additional metadata
   */
  logSlotError(processId, slot, error, meta = {}) {
    if (!this.enabled) return;
    const slotKey = `${processId}-${slot}`;
    const slotData = this.slotMetrics.get(slotKey);
    if (slotData) {
      slotData.retryCount = (slotData.retryCount || 0) + 1;
    }
    if (!this.processMetrics.has(processId)) {
      this.processMetrics.set(processId, {
        totalSlots: 0,
        completedSlots: 0,
        failedSlots: 0,
        totalDuration: 0,
        averageDuration: 0,
        lastSlot: null
      });
    }
    const processStats = this.processMetrics.get(processId);
    processStats.failedSlots++;
    const logData = {
      event: "slot_error",
      processId,
      slot,
      error: error.message,
      errorType: error.constructor.name,
      retryCount: slotData ? slotData.retryCount : 0,
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      ...meta
    };
    if (this.logger.slotError) {
      this.logger.slotError(processId, slot, error, meta);
    }
    if (this.fileLogger) {
      this.fileLogger.error("Slot error occurred", logData);
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
  logRetry(processId, slot, attempt, delay, meta = {}) {
    if (!this.enabled) return;
    const slotKey = `${processId}-${slot}`;
    const slotData = this.slotMetrics.get(slotKey);
    if (slotData) {
      slotData.retryCount = attempt;
    }
    const logData = {
      event: "slot_retry",
      processId,
      slot,
      attempt,
      delay,
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      ...meta
    };
    this.logger.warn(`Retrying slot ${slot} for process ${processId} (attempt ${attempt}, delay ${delay}ms)`, {
      processId,
      slot,
      attempt,
      delay,
      ...meta
    });
    if (this.fileLogger) {
      this.fileLogger.warn("Slot retry attempt", logData);
    }
  }
  /**
   * Get metrics for a specific process
   * @param processId - Process ID
   * @returns Process metrics
   */
  getProcessMetrics(processId) {
    return this.processMetrics.get(processId) || null;
  }
  /**
   * Get all process metrics
   * @returns Map of process metrics
   */
  getAllMetrics() {
    return new Map(this.processMetrics);
  }
  /**
   * Get active slot metrics
   * @returns Array of active slot metrics
   */
  getActiveSlots() {
    return Array.from(this.slotMetrics.values());
  }
  /**
   * Generate summary report
   * @returns Summary statistics
   */
  generateSummary() {
    const summary = {
      totalProcesses: this.processMetrics.size,
      totalSlots: 0,
      completedSlots: 0,
      failedSlots: 0,
      averageDuration: 0,
      processes: {}
    };
    for (const [processId, metrics] of this.processMetrics) {
      summary.totalSlots += metrics.totalSlots;
      summary.completedSlots += metrics.completedSlots;
      summary.failedSlots += metrics.failedSlots;
      summary.averageDuration += metrics.averageDuration;
      summary.processes[processId] = {
        totalSlots: metrics.totalSlots,
        completedSlots: metrics.completedSlots,
        failedSlots: metrics.failedSlots,
        successRate: metrics.totalSlots > 0 ? (metrics.completedSlots / metrics.totalSlots * 100).toFixed(1) : 0,
        averageDuration: metrics.averageDuration,
        lastSlot: metrics.lastSlot
      };
    }
    if (this.processMetrics.size > 0) {
      summary.averageDuration = summary.averageDuration / this.processMetrics.size;
    }
    return summary;
  }
  /**
   * Export metrics to JSON
   * @returns JSON string of metrics
   */
  exportMetrics() {
    const exportData = {
      timestamp: (/* @__PURE__ */ new Date()).toISOString(),
      summary: this.generateSummary(),
      processMetrics: Object.fromEntries(this.processMetrics),
      activeSlots: this.getActiveSlots()
    };
    return JSON.stringify(exportData, null, 2);
  }
  /**
   * Reset all metrics
   */
  reset() {
    this.slotMetrics.clear();
    this.processMetrics.clear();
  }
  /**
   * Close file logger and cleanup resources
   */
  close() {
    if (this.fileLogger && this.fileLogger.close) {
      this.fileLogger.close();
    }
  }
};

// server/hydrator/hydrator.ts
var TokenHydrator = class {
  constructor(config, logger2, errorHandler2) {
    this.config = config;
    this.logger = logger2;
    this.errorHandler = errorHandler2;
    this.abortControllers = /* @__PURE__ */ new Map();
    this.slotLogger = null;
    if (config.slotLogging && config.slotLogging.enabled) {
      this.slotLogger = new SlotLogger({
        logger: this.logger,
        enabled: true,
        logFile: config.slotLogging.logFile || null,
        format: config.slotLogging.format || "json",
        includeMetrics: config.slotLogging.includeMetrics !== false
      });
    }
  }
  /**
   * Fetch token information from SU router
   * @param token - Token process ID
   * @returns Token information
   */
  async fetchTokenInfo(token) {
    const controller = new AbortController();
    this.abortControllers.set(`info-${token}`, controller);
    try {
      const response = await retryWithBackoff(
        async () => {
          const res = await fetch(`https://su-router.ao-testnet.xyz/${token}/latest`, {
            signal: controller.signal
          });
          if (!res.ok) {
            throw new NetworkError(`Failed to fetch token info: HTTP ${res.status}`, {
              statusCode: res.status,
              token
            });
          }
          return res;
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying token info fetch for ${token} (attempt ${attempt}, delay ${delay}ms)`, {
              token,
              attempt,
              delay
            });
            if (this.slotLogger) {
              this.slotLogger.logRetry(token, 0, attempt, delay, {
                operation: "fetchTokenInfo",
                error: error.message
              });
            }
          },
          shouldRetry: (error) => error instanceof NetworkError && error.details.statusCode >= 500
        }
      );
      const data = await response.json();
      const nonce = data?.assignment?.tags?.find((t) => t.name === "Nonce")?.value || 0;
      const name = data?.assignment?.tags?.find((t) => t.name === "Name")?.value || token;
      return { nonce: parseInt(nonce), name };
    } catch (error) {
      if (error.name === "AbortError") {
        throw new NetworkError(`Token info fetch aborted for ${token}`, { token });
      }
      throw error;
    } finally {
      this.abortControllers.delete(`info-${token}`);
    }
  }
  /**
   * Import process to the node
   * @param token - Token process ID
   * @param name - Token name
   * @param nonce - Target nonce
   * @param nodeUrl - Node URL to use
   */
  async importProcess(token, name, nonce, nodeUrl) {
    const threshold = this.config.slotThreshold || 3e5;
    if (nonce <= threshold) {
      this.logger.debug(`Skipping import for ${token} - nonce ${nonce} <= ${threshold}`);
      return;
    }
    const controller = new AbortController();
    this.abortControllers.set(`import-${token}`, controller);
    try {
      this.logger.info(`Importing process ${token} (${name}), target: ${nonce}`);
      await retryWithBackoff(
        async () => {
          const response = await fetch(
            `${nodeUrl}/${token}~genesis-wasm@1.0/import&process-id=${token}`,
            {
              signal: controller.signal
            }
          );
          if (!response.ok) {
            throw new NetworkError(`Process import failed: HTTP ${response.status}`, {
              statusCode: response.status,
              token,
              importFailed: true
            });
          }
          return response;
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying process import for ${token} (attempt ${attempt})`, {
              token,
              attempt,
              delay
            });
          }
        }
      );
      if (this.logger.success) {
        this.logger.success(`Process ${token} imported successfully`);
      }
    } catch (error) {
      throw new ProcessError(`Failed to import process ${token}`, token, {
        importFailed: true,
        originalError: error.message
      });
    } finally {
      this.abortControllers.delete(`import-${token}`);
    }
  }
  /**
   * Trigger cron for the process
   * @param token - Token process ID
   * @param cronType - Type of cron operation ('once' or 'every')
   * @param nodeUrl - Node URL to use
   */
  async triggerCron(token, cronType, nodeUrl) {
    const controller = new AbortController();
    this.abortControllers.set(`cron-${token}`, controller);
    try {
      const cronPath = `${token}~process@1.0/now`;
      const url = cronType === "once" ? `${nodeUrl}/${token}~cron@1.0/once?cron-path=${cronPath}` : `${nodeUrl}/${token}~cron@1.0/every?interval=5-minutes&cron-path=${cronPath}`;
      console.log(url);
      this.logger.debug(`Triggering ${cronType} cron for ${token}`);
      await retryWithBackoff(
        async () => {
          const response = await fetch(url, {
            signal: controller.signal
          });
          if (!response.ok) {
            throw new NetworkError(`Cron trigger failed: HTTP ${response.status}`, {
              statusCode: response.status,
              token,
              cronFailed: true
            });
          }
          return response;
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          onRetry: (error, attempt, delay) => {
            this.logger.warn(`Retrying cron trigger for ${token} (attempt ${attempt})`, {
              token,
              attempt,
              delay
            });
          }
        }
      );
      this.logger.debug(`Cron ${cronType} triggered successfully for ${token}`);
    } catch (error) {
      throw new ProcessError(`Failed to trigger cron for process ${token}`, token, {
        cronFailed: true,
        cronType,
        originalError: error.message
      });
    } finally {
      this.abortControllers.delete(`cron-${token}`);
    }
  }
  /**
   * Get current slot for a process
   * @param token - Token process ID
   * @param nodeUrl - Node URL to use
   * @returns Current slot number
   */
  async getCurrentSlot(token, nodeUrl) {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 3e4);
    this.abortControllers.set(`slot-${token}`, controller);
    try {
      const response = await retryWithBackoff(
        async () => {
          const res = await fetch(
            `${nodeUrl}/${token}~process@1.0/compute/at-slot`,
            {
              signal: controller.signal
            }
          );
          if (!res.ok) {
            throw new NetworkError(`Failed to get current slot: HTTP ${res.status}`, {
              statusCode: res.status,
              token
            });
          }
          return res;
        },
        {
          maxAttempts: this.config.retry.maxAttempts,
          backoff: this.config.retry.backoff,
          shouldRetry: (error) => error instanceof NetworkError && error.details.statusCode >= 500
        }
      );
      const slotText = await response.text();
      return parseInt(slotText);
    } catch (error) {
      if (error.name === "AbortError") {
        throw new NetworkError(`Slot check aborted for ${token}`, { token });
      }
      throw error;
    } finally {
      this.abortControllers.delete(`slot-${token}`);
    }
  }
  /**
   * Hydrate a single token process
   * @param token - Token process ID
   * @param nodeUrl - Node URL to use
   * @returns Hydration result
   */
  async hydrateToken(token, nodeUrl) {
    const startTime = Date.now();
    const processLogger = this.logger.child({ processId: token });
    try {
      processLogger.info(`Starting hydration for process ${token}`);
      if (this.slotLogger) {
        this.slotLogger.startSlot(token, 0, 0, {
          operation: "hydration_start",
          timestamp: (/* @__PURE__ */ new Date()).toISOString()
        });
      }
      processLogger.debug("Fetching token information");
      const { nonce, name } = await this.fetchTokenInfo(token);
      const target = parseInt(String(nonce));
      processLogger.info(`Token ${token} (${name}) - Target: ${target}`);
      await this.importProcess(token, name, target, nodeUrl);
      await this.triggerCron(token, "once", nodeUrl);
      let currentSlot = 0;
      let iterations = 0;
      let lastLoggedSlot = 0;
      while (currentSlot < target) {
        try {
          iterations++;
          currentSlot = await this.getCurrentSlot(token, nodeUrl);
          if (currentSlot !== lastLoggedSlot && this.slotLogger) {
            this.slotLogger.updateProgress(token, currentSlot, target, {
              iteration: iterations,
              interval: this.config.waiting.minInterval
            });
            lastLoggedSlot = currentSlot;
          }
          if (currentSlot >= target) {
            break;
          }
          const nextInterval = this.config.waiting.minInterval;
          processLogger.debug(`Waiting ${nextInterval}ms before next check`);
          await new Promise((resolve) => setTimeout(resolve, nextInterval));
        } catch (error) {
          if (error instanceof NetworkError) {
            processLogger.warn(`Network error during slot check: ${error.message}`);
            if (this.slotLogger) {
              this.slotLogger.logSlotError(token, currentSlot, error, {
                iteration: iterations,
                operation: "slot_check"
              });
            }
            if (iterations > 5) {
              throw error;
            }
            await new Promise((resolve) => setTimeout(resolve, this.config.waiting.maxInterval));
          } else {
            throw error;
          }
        }
      }
      processLogger.info(`Reached target for process ${token} (${name}), setting up recurring cron`);
      if (this.slotLogger) {
        this.slotLogger.completeSlot(token, target, {
          totalIterations: iterations,
          operation: "target_reached"
        });
      }
      await new Promise((resolve) => setTimeout(resolve, 1e4));
      await this.triggerCron(token, "every", nodeUrl);
      const duration = Date.now() - startTime;
      if (processLogger.success) {
        processLogger.success(`Hydration completed for process ${token} (${name}) in ${duration}ms`);
      }
      return {
        success: true,
        token,
        name,
        target,
        iterations,
        duration
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      if (this.slotLogger) {
        this.slotLogger.logSlotError(token, 0, error, {
          duration,
          operation: "hydration_failed"
        });
      }
      this.errorHandler.handle(error, {
        processId: token,
        duration,
        operation: "hydrateToken"
      });
      return {
        success: false,
        token,
        error: error.message,
        duration
      };
    }
  }
  /**
   * Hydrate multiple token processes
   * @param tokens - Array of token process IDs
   * @param nodeUrl - Node URL to use
   * @returns Array of hydration results
   */
  async hydrateTokens(tokens, nodeUrl) {
    this.logger.info(`Starting hydration for ${tokens.length} processes`);
    const results = [];
    for (const token of tokens) {
      const result = await this.hydrateToken(token, nodeUrl);
      results.push(result);
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    return results;
  }
  /**
   * Abort all ongoing operations
   */
  abort() {
    this.logger.info("Aborting all operations...");
    for (const [key, controller] of this.abortControllers) {
      controller.abort();
      this.logger.debug(`Aborted operation: ${key}`);
    }
    this.abortControllers.clear();
  }
  /**
   * Cleanup resources including slot logger
   */
  cleanup() {
    this.logger.info("Cleaning up hydrator resources...");
    if (this.slotLogger) {
      this.slotLogger.close();
      this.logger.debug("Slot logger closed");
    }
    if (this.logger && this.logger.close) {
      this.logger.close();
      this.logger.debug("Main logger file stream closed");
    }
  }
};

// server/hydrator/logger.ts
import { EventEmitter } from "events";
import { createWriteStream, existsSync, mkdirSync, statSync, renameSync, unlinkSync } from "fs";
import { dirname } from "path";
var LOG_LEVELS = {
  debug: 0,
  info: 1,
  warn: 2,
  error: 3
};
var Logger = class _Logger extends EventEmitter {
  constructor(options = {}) {
    super();
    this.level = options.level || "info";
    this.format = options.format || "simple";
    this.colors = options.colors !== false;
    this.showTimestamp = options.showTimestamp !== false;
    this.showProcessId = options.showProcessId || false;
    this.fileOutput = options.fileOutput || null;
    this.fileStream = null;
    this.maxFileSize = options.maxFileSize || 50 * 1024 * 1024;
    this.maxFiles = options.maxFiles || 5;
    this.currentFileSize = 0;
    this.fileRotationEnabled = options.fileRotation !== false;
    if (this.fileOutput) {
      this._initializeFileStream();
    }
  }
  /**
   * Check if a log level should be displayed
   * @param level - Log level to check
   * @returns Whether to display the log
   */
  shouldLog(level) {
    return LOG_LEVELS[level] >= LOG_LEVELS[this.level];
  }
  /**
   * Initialize file stream for logging
   * @private
   */
  _initializeFileStream() {
    try {
      const logDir = dirname(this.fileOutput);
      if (!existsSync(logDir)) {
        mkdirSync(logDir, { recursive: true });
      }
      if (existsSync(this.fileOutput)) {
        const stats = statSync(this.fileOutput);
        this.currentFileSize = stats.size;
      }
      this.fileStream = createWriteStream(this.fileOutput, { flags: "a" });
      this.fileStream.on("error", (error) => {
        this.emit("fileError", error);
        console.error("Logger file stream error:", error.message);
      });
    } catch (error) {
      this.emit("fileError", error);
      console.error("Failed to initialize file stream:", error.message);
    }
  }
  /**
   * Rotate log files when size limit is reached
   * @private
   */
  _rotateFiles() {
    if (!this.fileRotationEnabled || !this.fileOutput) return;
    try {
      if (this.fileStream) {
        this.fileStream.end();
        this.fileStream = null;
      }
      for (let i = this.maxFiles - 1; i > 0; i--) {
        const oldFile = `${this.fileOutput}.${i}`;
        const newFile = `${this.fileOutput}.${i + 1}`;
        if (existsSync(oldFile)) {
          if (i === this.maxFiles - 1) {
            unlinkSync(oldFile);
          } else {
            renameSync(oldFile, newFile);
          }
        }
      }
      if (existsSync(this.fileOutput)) {
        renameSync(this.fileOutput, `${this.fileOutput}.1`);
      }
      this.currentFileSize = 0;
      this._initializeFileStream();
    } catch (error) {
      this.emit("fileError", error);
      console.error("File rotation failed:", error.message);
      try {
        this._initializeFileStream();
      } catch (reinitError) {
        console.error("Stream reinitialization failed:", reinitError.message);
      }
    }
  }
  /**
   * Write log entry to file
   * @private
   * @param logEntry - Formatted log entry
   */
  _writeToFile(logEntry) {
    if (!this.fileStream) return;
    const logLine = logEntry + "\n";
    const logSize = Buffer.byteLength(logLine, "utf8");
    if (this.fileRotationEnabled && this.currentFileSize + logSize > this.maxFileSize) {
      this._rotateFiles();
    }
    try {
      this.fileStream.write(logLine);
      this.currentFileSize += logSize;
    } catch (error) {
      this.emit("fileError", error);
    }
  }
  /**
   * Format timestamp
   * @returns Formatted timestamp
   */
  formatTimestamp() {
    if (!this.showTimestamp) return "";
    const now = /* @__PURE__ */ new Date();
    return now.toISOString().replace("T", " ").substring(0, 19);
  }
  /**
   * Get colored level string
   * @param level - Log level
   * @returns Colored level string
   */
  getColoredLevel(level) {
    if (!this.colors) return level.toUpperCase();
    const colors = {
      debug: "\x1B[90m",
      // gray
      info: "\x1B[34m",
      // blue
      warn: "\x1B[33m",
      // yellow
      error: "\x1B[31m"
      // red
    };
    const reset = "\x1B[0m";
    return `${colors[level]}${level.toUpperCase()}${reset}`;
  }
  /**
   * Format log message based on output format
   * @param level - Log level
   * @param message - Log message
   * @param meta - Additional metadata
   * @returns Formatted log message
   */
  formatMessage(level, message, meta = {}) {
    const timestamp = this.formatTimestamp();
    const levelStr = this.getColoredLevel(level);
    switch (this.format) {
      case "json":
        return JSON.stringify({
          timestamp: timestamp || (/* @__PURE__ */ new Date()).toISOString(),
          level,
          message,
          ...meta
        });
      case "slot":
        const slotData = {
          timestamp: timestamp || (/* @__PURE__ */ new Date()).toISOString(),
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
        };
        return JSON.stringify(slotData);
      case "simple":
        const parts = [];
        if (timestamp) {
          parts.push(`[${timestamp}]`);
        }
        parts.push(levelStr);
        const metaKeys = Object.keys(meta);
        for (const key of metaKeys) {
          if (!["slot", "processId", "target", "current", "progress", "duration", "retryCount"].includes(key)) {
            parts.push(`[${key}:${meta[key]}]`);
          }
        }
        if (this.showProcessId && meta.processId) {
          parts.push(`[${meta.processId}]`);
        }
        if (meta.slot) {
          parts.push(`[Slot:${meta.slot}]`);
        }
        parts.push(message);
        return parts.join(" ");
      default:
        return message;
    }
  }
  /**
   * Log a debug message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  debug(message, meta = {}) {
    if (this.shouldLog("debug")) {
      const formattedMessage = this.formatMessage("debug", message, meta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log an info message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  info(message, meta = {}) {
    if (this.shouldLog("info")) {
      const formattedMessage = this.formatMessage("info", message, meta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log a warning message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  warn(message, meta = {}) {
    if (this.shouldLog("warn")) {
      const formattedMessage = this.formatMessage("warn", message, meta);
      console.warn(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log an error message
   * @param message - Log message
   * @param meta - Additional metadata
   */
  error(message, meta = {}) {
    if (this.shouldLog("error")) {
      const formattedMessage = this.formatMessage("error", message, meta);
      console.error(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log a success message
   * @param message - Success message
   * @param meta - Additional metadata
   */
  success(message, meta = {}) {
    if (this.shouldLog("info")) {
      const successMessage = `\u2713 ${message}`;
      const formattedMessage = this.formatMessage("info", successMessage, meta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log a step in a process
   * @param step - Step description
   * @param current - Current step number
   * @param total - Total number of steps
   * @param meta - Additional metadata
   */
  step(step, current, total, meta = {}) {
    if (this.shouldLog("info")) {
      const stepMessage = `[${current}/${total}] ${step}`;
      const formattedMessage = this.formatMessage("info", stepMessage, { ...meta, current, total });
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log slot start event
   * @param processId - Process ID
   * @param slot - Slot number
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  slotStart(processId, slot, target, meta = {}) {
    if (this.shouldLog("info")) {
      const message = `Slot ${slot} started for process ${processId}, target: ${target}`;
      const slotMeta = {
        processId,
        slot,
        target,
        event: "slot_start",
        ...meta
      };
      const formattedMessage = this.formatMessage("info", message, slotMeta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log slot progress update
   * @param processId - Process ID
   * @param current - Current slot
   * @param target - Target slot
   * @param meta - Additional metadata
   */
  slotProgress(processId, current, target, meta = {}) {
    if (this.shouldLog("debug")) {
      const progress = (current / target * 100).toFixed(1);
      const message = `Slot progress: ${current}/${target} (${progress}%) for process ${processId}`;
      const slotMeta = {
        processId,
        current,
        target,
        progress: parseFloat(progress),
        event: "slot_progress",
        ...meta
      };
      const formattedMessage = this.formatMessage("debug", message, slotMeta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log slot completion event
   * @param processId - Process ID
   * @param slot - Completed slot
   * @param duration - Duration in milliseconds
   * @param meta - Additional metadata
   */
  slotComplete(processId, slot, duration, meta = {}) {
    if (this.shouldLog("info")) {
      const message = `Slot ${slot} completed for process ${processId} in ${duration}ms`;
      const slotMeta = {
        processId,
        slot,
        duration,
        event: "slot_complete",
        ...meta
      };
      const formattedMessage = this.formatMessage("info", message, slotMeta);
      console.log(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Log slot error event
   * @param processId - Process ID
   * @param slot - Slot number
   * @param error - Error object
   * @param meta - Additional metadata
   */
  slotError(processId, slot, error, meta = {}) {
    if (this.shouldLog("error")) {
      const message = `Slot ${slot} error for process ${processId}: ${error.message}`;
      const slotMeta = {
        processId,
        slot,
        error: error.message,
        errorType: error.constructor.name,
        event: "slot_error",
        ...meta
      };
      const formattedMessage = this.formatMessage("error", message, slotMeta);
      console.error(formattedMessage);
      this._writeToFile(formattedMessage);
    }
  }
  /**
   * Create a child logger with additional context
   * @param context - Additional context to include in all logs
   * @returns Child logger instance
   */
  child(context) {
    const childLogger = new _Logger({
      level: this.level,
      format: this.format,
      colors: this.colors,
      showTimestamp: this.showTimestamp,
      showProcessId: this.showProcessId,
      fileOutput: this.fileOutput,
      maxFileSize: this.maxFileSize,
      maxFiles: this.maxFiles,
      fileRotation: this.fileRotationEnabled
    });
    childLogger.context = { ...context };
    const originalFormat = childLogger.formatMessage.bind(childLogger);
    childLogger.formatMessage = (level, message, meta = {}) => {
      const mergedMeta = { ...childLogger.context, ...meta };
      return originalFormat(level, message, mergedMeta);
    };
    return childLogger;
  }
  /**
   * Close file stream and cleanup resources
   */
  close() {
    if (this.fileStream) {
      this.fileStream.end();
      this.fileStream = null;
    }
  }
  /**
   * Get current file size and rotation info
   * @returns File statistics
   */
  getFileStats() {
    if (!this.fileOutput) return null;
    return {
      currentFile: this.fileOutput,
      currentSize: this.currentFileSize,
      maxSize: this.maxFileSize,
      rotationEnabled: this.fileRotationEnabled,
      maxFiles: this.maxFiles
    };
  }
};
var defaultLogger = new Logger();

// server/fn.ts
var logger = new Logger({
  level: "debug",
  format: "simple",
  colors: false,
  showTimestamp: true,
  fileOutput: "~/hydrator.log"
});
var errorHandler = new ErrorHandler(logger);
var hydrator = new TokenHydrator(
  {
    slotThreshold: 200,
    retry: {
      maxAttempts: 3,
      backoff: 1e3
    },
    waiting: {
      adaptive: true,
      minInterval: 1e3,
      maxInterval: 3e4
    },
    output: {
      format: "simple"
    },
    slotLogging: {
      enabled: false
    }
  },
  logger,
  errorHandler
);
var readProcessesWith = ({ db: db2 }) => {
  return async ({ processes, query, pagination } = {}) => {
    let processesWithTimestamp;
    if (processes && processes.length > 0) {
      const hydrations2 = {};
      const filteredProcesses2 = [];
      for (const processId of processes) {
        const rows = await db2.getHydrationsByProcessId(processId, query);
        if (rows.length > 0) {
          hydrations2[processId] = rows;
          filteredProcesses2.push(processId);
        }
      }
      return {
        processes: filteredProcesses2,
        hydrations: Object.keys(hydrations2).length > 0 ? hydrations2 : void 0
      };
    } else if (query) {
      processesWithTimestamp = await db2.getProcessesByQueryWithTimestamp(query, pagination);
    } else {
      processesWithTimestamp = await db2.getAllProcessesWithTimestamp(pagination);
    }
    const hydrations = {};
    const filteredProcesses = [];
    for (const process2 of processesWithTimestamp) {
      const rows = await db2.getHydrationsByProcessId(process2.processId, query);
      if (rows.length > 0) {
        hydrations[process2.processId] = rows;
        filteredProcesses.push(process2.processId);
      }
    }
    const result = {
      processes: filteredProcesses,
      hydrations: Object.keys(hydrations).length > 0 ? hydrations : void 0
    };
    if (pagination?.limit !== void 0 && processesWithTimestamp.length > 0) {
      if (processesWithTimestamp.length === pagination.limit) {
        const lastProcess = processesWithTimestamp[processesWithTimestamp.length - 1];
        result.nextCursor = lastProcess.timestamp;
      }
    }
    return result;
  };
};
var loadProcessesWith = ({ db: db2 }) => {
  return async ({ processes, hydrations }) => {
    for (const processId of processes) {
      await db2.saveProcess(processId);
    }
    if (hydrations) {
      for (const [processId, hydrationList] of Object.entries(hydrations)) {
        for (const hydration of hydrationList) {
          await db2.saveHydration(processId, hydration.url, hydration.status);
        }
      }
    }
  };
};
var startHydration = async ({ id, url, db: db2 }) => {
  await db2.saveHydration(id, url, "REQUESTSENT");
  await hydrator.hydrateTokens([id], url).then(async (res) => {
    if (res[0].success) {
      await db2.saveHydration(id, url, "HYDRATED");
    } else {
      const newStatus = await checkStatus({ id, url });
      await db2.saveHydration(id, url, newStatus);
    }
    return res;
  }).catch(async (e) => {
    const newStatus = await checkStatus({ id, url });
    await db2.saveHydration(id, url, newStatus);
    return null;
  });
};
var startCron = async ({ id, url, db: db2 }) => {
  await hydrator.triggerCron(id, "every", url).then(async (res) => {
    return res;
  }).catch(async (e) => {
    return null;
  });
};
var hydrateWith = ({ db: db2 }) => {
  return async ({ processes }) => {
    for (const processId of processes) {
      const hydrations = await db2.getHydrationsByProcessId(processId);
      if (hydrations.length > 0) {
        for (const hydration of hydrations) {
          if (!["INIT", "NOPROGRESS", "PROGRESS"].includes(hydration.status)) {
            continue;
          }
          startHydration({ id: processId, url: hydration.url, db: db2 }).then(() => {
          });
        }
      }
    }
  };
};
var cronWith = ({ db: db2 }) => {
  return async ({ processes }) => {
    for (const processId of processes) {
      const hydrations = await db2.getHydrationsByProcessId(processId);
      if (hydrations.length > 0) {
        for (const hydration of hydrations) {
          startCron({ id: processId, url: hydration.url, db: db2 }).then(() => {
          });
        }
      }
    }
  };
};
var checkStatus = async ({ id, url }) => {
  const slot = await hydrator.getCurrentSlot(id, url).then((res) => res).catch((_) => 0);
  const nonce = await hydrator.fetchTokenInfo(id).then((res) => res.nonce).catch((_) => 0);
  if (slot > 0 && nonce > 0) {
    if (slot === nonce) {
      return "HYDRATED";
    } else if (nonce - slot < 100) {
      return "HYDRATED";
    } else if (slot > 0) {
      return "PROGRESS";
    }
  }
  return "NOPROGRESS";
};
var refreshStatusWith = ({ db: db2 }) => {
  return async ({ processes }) => {
    const tasks = [];
    for (const processId of processes) {
      const hydrations = await db2.getHydrationsByProcessId(processId);
      for (const hydration of hydrations) {
        tasks.push({ processId, url: hydration.url });
      }
    }
    const concurrency = 10;
    for (let i = 0; i < tasks.length; i += concurrency) {
      const batch = tasks.slice(i, i + concurrency);
      await Promise.all(
        batch.map(async ({ processId, url }) => {
          const newStatus = await checkStatus({ id: processId, url });
          await new Promise((resolve) => setTimeout(resolve, 50));
          console.log(processId, url, newStatus);
          await db2.saveHydration(processId, url, newStatus);
        })
      );
    }
  };
};

// server/worker.ts
if (!parentPort) {
  throw new Error("This file must be run as a Worker thread");
}
if (!process.env.DATABASE_PATH) {
  throw new Error("DATABASE_PATH environment variable is required");
}
var db = await createPostgresClient({
  url: process.env.DATABASE_PATH
});
var loadProcesses = loadProcessesWith({ db });
var hydrate = hydrateWith({ db });
var cron = cronWith({ db });
var refreshStatus = refreshStatusWith({ db });
var readProcesses = readProcessesWith({ db });
console.log("[Worker] Worker thread initialized");
var isRunning = false;
var port = parentPort;
port.on("message", async (message) => {
  try {
    console.log("[Worker] Received message:", message);
    if (typeof message === "string") {
      if (message === "start-hydrations") {
        isRunning = true;
        console.log("[Worker] Hydrations started");
        const response = { type: "status", running: true };
        port.postMessage(response);
      } else if (message === "stop-hydrations") {
        isRunning = false;
        console.log("[Worker] Hydrations stopped");
        const response = { type: "status", running: false };
        port.postMessage(response);
      }
      return;
    }
    if (message.type === "queue-hydrations") {
      saveHydrations(message);
    }
  } catch (error) {
    console.log("[Worker] error:", error);
  }
});
async function saveHydrations(message) {
  const { processes, hydrations } = message.payload;
  for (const processId of processes) {
    await db.saveProcess(processId);
  }
  if (hydrations && Array.isArray(hydrations)) {
    for (const hydration of hydrations) {
      const { processId, url, status } = hydration;
      const existingHydrations = await db.getHydrationsByProcessId(
        processId,
        url
      );
      if (existingHydrations.length === 0) {
        await db.saveHydration(processId, url, status);
      }
    }
  }
  console.log(`[Worker] Queued hydration job with ${processes.length} processes.`);
}
var runningPromises = /* @__PURE__ */ new Set();
var MAX_CONCURRENT = 100;
var queueHydration = async (processId, url) => {
  while (runningPromises.size >= MAX_CONCURRENT) {
    if (runningPromises.size > 0) {
      await Promise.race(runningPromises);
    }
  }
  const promise = startHydration({ id: processId, url, db }).then(() => {
    runningPromises.delete(promise);
    console.log(`Hydration completed for ${processId}. Queue size: ${runningPromises.size}/${MAX_CONCURRENT}`);
  }).catch((err) => {
    console.error(`Hydration error for ${processId}:`, err);
    runningPromises.delete(promise);
    console.log(`Queue size after error: ${runningPromises.size}/${MAX_CONCURRENT}`);
  });
  runningPromises.add(promise);
  console.log(`Started hydration for ${processId}. Queue size: ${runningPromises.size}/${MAX_CONCURRENT}`);
};
while (true) {
  if (!isRunning) {
    await new Promise((resolve) => setTimeout(resolve, 1e3));
    continue;
  }
  const initHydrations = await db.getHydrationsByStatus("INIT", 200);
  if (initHydrations.length === 0) {
    console.log("[Worker] No INIT hydrations found, waiting 5 seconds before next check...");
    await new Promise((resolve) => setTimeout(resolve, 5e3));
    continue;
  }
  console.log(`[Worker] Found ${initHydrations.length} INIT hydrations to process`);
  for (const hydration of initHydrations) {
    await queueHydration(hydration.processId, hydration.url);
  }
  await new Promise((resolve) => setTimeout(resolve, 100));
}
