import Database from 'better-sqlite3'
import { existsSync, mkdirSync } from 'fs'
import { dirname } from 'path'

type SqliteDatabase = Database.Database

interface SqlStatement {
  sql: string
  parameters: any[]
}

interface SqliteClientConfig {
  url: string
}

export interface Hydration {
  url: string
  status: string
}

// Define queryable fields for filtering hydrations
export const QUERYABLE_HYDRATION_FIELDS = ['url'] as const

// Define queryable fields for filtering repushes
export const QUERYABLE_REPUSH_FIELDS = ['messageId'] as const

export interface Repush {
  processId: string
  messageId: string
  status: string
  reasons?: string
  timestamp: number
}

export interface AuthToken {
  id?: number
  token: string
  type: string
  timestamp: number
}

export interface Permission {
  id?: number
  token: string
  processId: string
  label: string
  timestamp: number
}

export interface PaginationOptions {
  limit?: number
  cursor?: number  // timestamp cursor
}

export interface ProcessWithTimestamp {
  processId: string
  timestamp: number
}

export interface SqliteClient {
  query: (statement: SqlStatement) => Promise<any[]>
  run: (statement: SqlStatement) => Promise<Database.RunResult>
  transaction: (statements: SqlStatement[]) => Promise<Database.RunResult[]>
  pragma: (value: string, options?: Database.PragmaOptions) => Promise<any>
  saveProcess: (processId: string, timestamp?: number) => Promise<Database.RunResult>
  saveHydration: (processId: string, url: string, status: string, timestamp?: number) => Promise<Database.RunResult>
  saveRepush: (processId: string, messageId: string, status: string, reason?: string, timestamp?: number) => Promise<Database.RunResult>
  saveAuthToken: (token: string, type: string, timestamp?: number) => Promise<Database.RunResult>
  savePermission: (token: string, processId: string, label: string, timestamp?: number) => Promise<Database.RunResult>
  getAuthToken: (token: string) => Promise<AuthToken | undefined>
  getPermissionsByToken: (token: string) => Promise<Permission[]>
  getAllProcessIds: (pagination?: PaginationOptions) => Promise<string[]>
  getProcessIdsByQuery: (query: string, pagination?: PaginationOptions) => Promise<string[]>
  getAllProcessesWithTimestamp: (pagination?: PaginationOptions) => Promise<ProcessWithTimestamp[]>
  getProcessesByQueryWithTimestamp: (query: string, pagination?: PaginationOptions) => Promise<ProcessWithTimestamp[]>
  getHydrationsByProcessId: (processId: string, query?: string) => Promise<Hydration[]>
  getRepushes: (query?: string, pagination?: PaginationOptions) => Promise<Repush[]>
  getStatusCounts: () => Promise<Record<string, number>>
  getRepushStatusCounts: () => Promise<Record<string, number>>
  deleteProcess: (processId: string) => Promise<Database.RunResult>
  db: SqliteDatabase
}

const createProcesses = async (db: SqliteDatabase): Promise<Database.RunResult> => db.prepare(
  `CREATE TABLE IF NOT EXISTS processes(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    processId TEXT UNIQUE NOT NULL,
    timestamp INTEGER NOT NULL
  );`
).run()

const createHydrations = async (db: SqliteDatabase): Promise<Database.RunResult> => db.prepare(
  `CREATE TABLE IF NOT EXISTS hydrations(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    processId TEXT NOT NULL,
    url TEXT NOT NULL,
    status TEXT NOT NULL CHECK(status IN ('INIT', 'REQUESTSENT', 'NOPROGRESS', 'PROGRESS', 'HYDRATED')),
    timestamp INTEGER NOT NULL,
    UNIQUE(processId, url),
    FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
  );`
).run()

const createRepushes = async (db: SqliteDatabase): Promise<Database.RunResult> => db.prepare(
  `CREATE TABLE IF NOT EXISTS repushes(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    processId TEXT NOT NULL,
    messageId TEXT NOT NULL,
    status TEXT NOT NULL CHECK(status IN ('INIT', 'REPUSHED', 'CANNOTPUSHALL')),
    reasons TEXT,
    timestamp INTEGER NOT NULL,
    UNIQUE(processId, messageId),
    FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
  );`
).run()

const createAuthTokens = async (db: SqliteDatabase): Promise<Database.RunResult> => db.prepare(
  `CREATE TABLE IF NOT EXISTS auth_token(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    token TEXT NOT NULL,
    type TEXT NOT NULL CHECK(type IN ('ADMIN', 'USER')),
    timestamp INTEGER NOT NULL,
    UNIQUE(token)
  );`
).run()

const createPermissions = async (db: SqliteDatabase): Promise<Database.RunResult> => db.prepare(
  `CREATE TABLE IF NOT EXISTS permissions(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    token TEXT NOT NULL,
    processId TEXT NOT NULL,
    label TEXT NOT NULL,
    timestamp INTEGER NOT NULL,
    UNIQUE(token, processId),
    FOREIGN KEY (token) REFERENCES auth_token(token) ON DELETE CASCADE,
    FOREIGN KEY (processId) REFERENCES processes(processId) ON DELETE CASCADE
  );`
).run()

export async function createSqliteClient ({ url }: SqliteClientConfig): Promise<SqliteClient> {
  // Create directory if it doesn't exist
  const dbDir = dirname(url)
  if (!existsSync(dbDir)) {
    console.log(`Creating directory: ${dbDir}`)
    mkdirSync(dbDir, { recursive: true })
  }

  const dbExists = existsSync(url)
  const db: SqliteDatabase = new Database(url)

  if (!dbExists) {
    console.log('Database file does not exist, creating tables...')
    db.pragma('encoding = "UTF-8"')
    db.pragma('journal_mode = WAL')

    await Promise.resolve()
      .then(() => createProcesses(db))
      .then(() => createHydrations(db))
      .then(() => createRepushes(db))
      .then(() => createAuthTokens(db))
      .then(() => createPermissions(db))
  } else {
    await Promise.resolve()
      .then(() => createProcesses(db))
      .then(() => createHydrations(db))
      .then(() => createRepushes(db))
      .then(() => createAuthTokens(db))
      .then(() => createPermissions(db))
  }

  const client = {
    query: async ({ sql, parameters }: SqlStatement) => db.prepare(sql).all(...parameters),
    run: async ({ sql, parameters }: SqlStatement) => db.prepare(sql).run(...parameters),
    transaction: async (statements: SqlStatement[]) => db.transaction(
      (stmts: SqlStatement[]) => stmts.map(({ sql, parameters }) => db.prepare(sql).run(...parameters))
    )(statements),
    pragma: async (value: string, options?: Database.PragmaOptions) => db.pragma(value, options),
    saveProcess: async (processId: string, timestamp: number = Date.now()) => {
      return db.prepare(
        'INSERT OR IGNORE INTO processes (processId, timestamp) VALUES (?, ?)'
      ).run(processId, timestamp)
    },
    saveHydration: async (processId: string, url: string, status: string, timestamp: number = Date.now()) => {
      return db.prepare(
        'INSERT OR REPLACE INTO hydrations (processId, url, status, timestamp) VALUES (?, ?, ?, ?)'
      ).run(processId, url, status, timestamp)
    },
    saveRepush: async (processId: string, messageId: string, status: string, reason?: string, timestamp: number = Date.now()) => {
      return db.prepare(
        'INSERT OR REPLACE INTO repushes (processId, messageId, status, reasons, timestamp) VALUES (?, ?, ?, ?, ?)'
      ).run(processId, messageId, status, reason, timestamp)
    },
    saveAuthToken: async (token: string, type: string, timestamp: number = Date.now()) => {
      return db.prepare(
        'INSERT OR IGNORE INTO auth_token (token, type, timestamp) VALUES (?, ?, ?)'
      ).run(token, type, timestamp)
    },
    savePermission: async (token: string, processId: string, label: string, timestamp: number = Date.now()) => {
      return db.prepare(
        'INSERT OR REPLACE INTO permissions (token, processId, label, timestamp) VALUES (?, ?, ?, ?)'
      ).run(token, processId, label, timestamp)
    },
    getAuthToken: async (token: string) => {
      const result = db.prepare(
        'SELECT id, token, type, timestamp FROM auth_token WHERE token = ?'
      ).get(token) as AuthToken | undefined
      return result
    },
    getPermissionsByToken: async (token: string) => {
      const results = db.prepare(
        'SELECT id, token, processId, label, timestamp FROM permissions WHERE token = ?'
      ).all(token) as Permission[]
      return results
    },
    getAllProcessIds: async (pagination?: PaginationOptions) => {
      let sql = 'SELECT processId FROM processes'
      const params: any[] = []

      if (pagination?.cursor) {
        sql += ' WHERE timestamp > ?'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        sql += ` LIMIT ${pagination.limit}`
      }

      const rows = db.prepare(sql).all(...params) as { processId: string }[]
      return rows.map(row => row.processId)
    },
    getProcessIdsByQuery: async (query: string, pagination?: PaginationOptions) => {
      // Build OR clause for all queryable fields
      const conditions = QUERYABLE_HYDRATION_FIELDS.map(field => `${field} = ?`).join(' OR ')
      let sql = `SELECT DISTINCT p.processId
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`
      const params: any[] = QUERYABLE_HYDRATION_FIELDS.map(() => query)

      if (pagination?.cursor) {
        sql += ' AND p.timestamp > ?'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY p.timestamp'

      if (pagination?.limit !== undefined) {
        sql += ` LIMIT ${pagination.limit}`
      }

      const rows = db.prepare(sql).all(...params) as { processId: string }[]
      return rows.map(row => row.processId)
    },
    getAllProcessesWithTimestamp: async (pagination?: PaginationOptions) => {
      let sql = 'SELECT processId, timestamp FROM processes'
      const params: any[] = []

      if (pagination?.cursor) {
        sql += ' WHERE timestamp > ?'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        sql += ` LIMIT ${pagination.limit}`
      }

      return db.prepare(sql).all(...params) as ProcessWithTimestamp[]
    },
    getProcessesByQueryWithTimestamp: async (query: string, pagination?: PaginationOptions) => {
      // Build OR clause for all queryable fields
      const conditions = QUERYABLE_HYDRATION_FIELDS.map(field => `${field} = ?`).join(' OR ')
      let sql = `SELECT DISTINCT p.processId, p.timestamp
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`
      const params: any[] = QUERYABLE_HYDRATION_FIELDS.map(() => query)

      if (pagination?.cursor) {
        sql += ' AND p.timestamp > ?'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY p.timestamp'

      if (pagination?.limit !== undefined) {
        sql += ` LIMIT ${pagination.limit}`
      }

      return db.prepare(sql).all(...params) as ProcessWithTimestamp[]
    },
    getHydrationsByProcessId: async (processId: string, query?: string) => {
      if (query) {
        // Build OR clause for all queryable fields
        const conditions = QUERYABLE_HYDRATION_FIELDS.map(field => `${field} = ?`).join(' OR ')
        const sql = `SELECT url, status FROM hydrations WHERE processId = ? AND (${conditions})`
        const params = [processId, ...QUERYABLE_HYDRATION_FIELDS.map(() => query)]
        const rows = db.prepare(sql).all(...params) as Hydration[]
        return rows
      }
      const rows = db.prepare('SELECT url, status FROM hydrations WHERE processId = ?').all(processId) as Hydration[]
      return rows
    },
    getRepushes: async (query?: string, pagination?: PaginationOptions) => {
      let sql = 'SELECT processId, messageId, status, reasons, timestamp FROM repushes'
      const params: any[] = []

      if (query) {
        // Build OR clause for all queryable fields
        const conditions = QUERYABLE_REPUSH_FIELDS.map(field => `${field} = ?`).join(' OR ')
        sql += ` WHERE ${conditions}`
        params.push(...QUERYABLE_REPUSH_FIELDS.map(() => query))
      }

      if (pagination?.cursor) {
        sql += query ? ' AND timestamp > ?' : ' WHERE timestamp > ?'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        sql += ` LIMIT ${pagination.limit}`
      }

      return db.prepare(sql).all(...params) as Repush[]
    },
    getStatusCounts: async () => {
      const rows = db.prepare('SELECT status, COUNT(*) as count FROM hydrations GROUP BY status').all() as Array<{ status: string, count: number }>
      return rows.reduce((acc, { status, count }) => {
        acc[status] = count
        return acc
      }, {} as Record<string, number>)
    },
    getRepushStatusCounts: async () => {
      const rows = db.prepare('SELECT status, COUNT(*) as count FROM repushes GROUP BY status').all() as Array<{ status: string, count: number }>
      return rows.reduce((acc, { status, count }) => {
        acc[status] = count
        return acc
      }, {} as Record<string, number>)
    },
    deleteProcess: async (processId: string) => {
      return db.prepare('DELETE FROM processes WHERE processId = ?').run(processId)
    },
    db
  }

  if (process.env.ADMIN_TOKEN) {
    await client.saveAuthToken(process.env.ADMIN_TOKEN, 'ADMIN')
    console.log('Admin token initialized')
  }

  if (process.env.USER_PERMISSIONS) {
    try {
      const userPermissions = JSON.parse(process.env.USER_PERMISSIONS) as Record<string, string[]>

      for (const [token, processIds] of Object.entries(userPermissions)) {
        await client.saveAuthToken(token, 'USER')

        for (const processId of processIds) {
          await client.saveProcess(processId)
          await client.savePermission(token, processId, 'RESOLVE_UNPUSHED')
        }

        console.log(`User token initialized with ${processIds.length} process permissions`)
      }
    } catch (error) {
      console.error('Failed to parse USER_PERMISSIONS:', error)
    }
  }

  return client
}
