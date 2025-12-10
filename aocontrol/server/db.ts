import pg from 'pg'

const { Pool } = pg

type PostgresPool = pg.Pool

interface SqlStatement {
  sql: string
  parameters: any[]
}

interface PostgresClientConfig {
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

export interface DbClient {
  query: (statement: SqlStatement) => Promise<any[]>
  run: (statement: SqlStatement) => Promise<{ changes: number, lastInsertRowid?: number }>
  transaction: (statements: SqlStatement[]) => Promise<{ changes: number, lastInsertRowid?: number }[]>
  saveProcess: (processId: string, timestamp?: number) => Promise<{ changes: number, lastInsertRowid?: number }>
  saveHydration: (processId: string, url: string, status: string, timestamp?: number) => Promise<{ changes: number, lastInsertRowid?: number }>
  saveRepush: (processId: string, messageId: string, status: string, reason?: string, timestamp?: number) => Promise<{ changes: number, lastInsertRowid?: number }>
  saveAuthToken: (token: string, type: string, timestamp?: number) => Promise<{ changes: number, lastInsertRowid?: number }>
  savePermission: (token: string, processId: string, label: string, timestamp?: number) => Promise<{ changes: number, lastInsertRowid?: number }>
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
  deleteProcess: (processId: string) => Promise<{ changes: number, lastInsertRowid?: number }>
  pool: PostgresPool
}

const createProcesses = async (pool: PostgresPool): Promise<void> => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS processes(
      id SERIAL PRIMARY KEY,
      processId TEXT UNIQUE NOT NULL,
      timestamp BIGINT NOT NULL
    );
  `)
}

const createHydrations = async (pool: PostgresPool): Promise<void> => {
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
  `)
}

const createRepushes = async (pool: PostgresPool): Promise<void> => {
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
  `)
}

const createAuthTokens = async (pool: PostgresPool): Promise<void> => {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS auth_token(
      id SERIAL PRIMARY KEY,
      token TEXT NOT NULL,
      type TEXT NOT NULL CHECK(type IN ('ADMIN', 'USER')),
      timestamp BIGINT NOT NULL,
      UNIQUE(token)
    );
  `)
}

const createPermissions = async (pool: PostgresPool): Promise<void> => {
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
  `)
}

export async function createPostgresClient ({ url }: PostgresClientConfig): Promise<DbClient> {
  const pool = new Pool({
    connectionString: url
  })

  // Test connection
  try {
    await pool.query('SELECT NOW()')
    console.log('Successfully connected to PostgreSQL database')
  } catch (error) {
    console.error('Failed to connect to PostgreSQL:', error)
    throw error
  }

  // Create tables
  await Promise.resolve()
    .then(() => createProcesses(pool))
    .then(() => createHydrations(pool))
    .then(() => createRepushes(pool))
    .then(() => createAuthTokens(pool))
    .then(() => createPermissions(pool))

  const client: DbClient = {
    query: async ({ sql, parameters }: SqlStatement) => {
      const result = await pool.query(sql, parameters)
      return result.rows
    },
    run: async ({ sql, parameters }: SqlStatement) => {
      const result = await pool.query(sql, parameters)
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    transaction: async (statements: SqlStatement[]) => {
      const client = await pool.connect()
      try {
        await client.query('BEGIN')
        const results = []
        for (const { sql, parameters } of statements) {
          const result = await client.query(sql, parameters)
          results.push({
            changes: result.rowCount || 0,
            lastInsertRowid: result.rows[0]?.id
          })
        }
        await client.query('COMMIT')
        return results
      } catch (error) {
        await client.query('ROLLBACK')
        throw error
      } finally {
        client.release()
      }
    },
    saveProcess: async (processId: string, timestamp: number = Date.now()) => {
      const result = await pool.query(
        'INSERT INTO processes (processId, timestamp) VALUES ($1, $2) ON CONFLICT (processId) DO NOTHING RETURNING id',
        [processId, timestamp]
      )
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    saveHydration: async (processId: string, url: string, status: string, timestamp: number = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO hydrations (processId, url, status, timestamp)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (processId, url)
         DO UPDATE SET status = EXCLUDED.status, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [processId, url, status, timestamp]
      )
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    saveRepush: async (processId: string, messageId: string, status: string, reason?: string, timestamp: number = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO repushes (processId, messageId, status, reasons, timestamp)
         VALUES ($1, $2, $3, $4, $5)
         ON CONFLICT (processId, messageId)
         DO UPDATE SET status = EXCLUDED.status, reasons = EXCLUDED.reasons, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [processId, messageId, status, reason, timestamp]
      )
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    saveAuthToken: async (token: string, type: string, timestamp: number = Date.now()) => {
      const result = await pool.query(
        'INSERT INTO auth_token (token, type, timestamp) VALUES ($1, $2, $3) ON CONFLICT (token) DO NOTHING RETURNING id',
        [token, type, timestamp]
      )
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    savePermission: async (token: string, processId: string, label: string, timestamp: number = Date.now()) => {
      const result = await pool.query(
        `INSERT INTO permissions (token, processId, label, timestamp)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (token, processId)
         DO UPDATE SET label = EXCLUDED.label, timestamp = EXCLUDED.timestamp
         RETURNING id`,
        [token, processId, label, timestamp]
      )
      return {
        changes: result.rowCount || 0,
        lastInsertRowid: result.rows[0]?.id
      }
    },
    getAuthToken: async (token: string) => {
      const result = await pool.query(
        'SELECT id, token, type, timestamp FROM auth_token WHERE token = $1',
        [token]
      )
      return result.rows[0] as AuthToken | undefined
    },
    getPermissionsByToken: async (token: string) => {
      const result = await pool.query(
        'SELECT id, token, processId, label, timestamp FROM permissions WHERE token = $1',
        [token]
      )
      return result.rows as Permission[]
    },
    getAllProcessIds: async (pagination?: PaginationOptions) => {
      let sql = 'SELECT processId FROM processes'
      const params: any[] = []

      if (pagination?.cursor) {
        sql += ' WHERE timestamp > $1'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        const limitIndex = params.length + 1
        sql += ` LIMIT $${limitIndex}`
        params.push(pagination.limit)
      }

      const result = await pool.query(sql, params)
      return result.rows.map((row: any) => row.processid)
    },
    getProcessIdsByQuery: async (query: string, pagination?: PaginationOptions) => {
      // Build OR clause for all queryable fields
      const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(' OR ')
      let sql = `SELECT DISTINCT p.processId
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`
      const params: any[] = QUERYABLE_HYDRATION_FIELDS.map(() => query)

      if (pagination?.cursor) {
        const cursorIndex = params.length + 1
        sql += ` AND p.timestamp > $${cursorIndex}`
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY p.timestamp'

      if (pagination?.limit !== undefined) {
        const limitIndex = params.length + 1
        sql += ` LIMIT $${limitIndex}`
        params.push(pagination.limit)
      }

      const result = await pool.query(sql, params)
      return result.rows.map((row: any) => row.processid)
    },
    getAllProcessesWithTimestamp: async (pagination?: PaginationOptions) => {
      let sql = 'SELECT processId, timestamp FROM processes'
      const params: any[] = []

      if (pagination?.cursor) {
        sql += ' WHERE timestamp > $1'
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        const limitIndex = params.length + 1
        sql += ` LIMIT $${limitIndex}`
        params.push(pagination.limit)
      }

      const result = await pool.query(sql, params)
      return result.rows.map((row: any) => ({
        processId: row.processid,
        timestamp: parseInt(row.timestamp)
      }))
    },
    getProcessesByQueryWithTimestamp: async (query: string, pagination?: PaginationOptions) => {
      // Build OR clause for all queryable fields
      const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(' OR ')
      let sql = `SELECT DISTINCT p.processId, p.timestamp
                 FROM hydrations h
                 JOIN processes p ON h.processId = p.processId
                 WHERE ${conditions}`
      const params: any[] = QUERYABLE_HYDRATION_FIELDS.map(() => query)

      if (pagination?.cursor) {
        const cursorIndex = params.length + 1
        sql += ` AND p.timestamp > $${cursorIndex}`
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY p.timestamp'

      if (pagination?.limit !== undefined) {
        const limitIndex = params.length + 1
        sql += ` LIMIT $${limitIndex}`
        params.push(pagination.limit)
      }

      const result = await pool.query(sql, params)
      return result.rows.map((row: any) => ({
        processId: row.processid,
        timestamp: parseInt(row.timestamp)
      }))
    },
    getHydrationsByProcessId: async (processId: string, query?: string) => {
      if (query) {
        // Build OR clause for all queryable fields
        const conditions = QUERYABLE_HYDRATION_FIELDS.map((field, index) => `${field} = $${index + 2}`).join(' OR ')
        const sql = `SELECT url, status FROM hydrations WHERE processId = $1 AND (${conditions})`
        const params = [processId, ...QUERYABLE_HYDRATION_FIELDS.map(() => query)]
        const result = await pool.query(sql, params)
        return result.rows as Hydration[]
      }
      const result = await pool.query('SELECT url, status FROM hydrations WHERE processId = $1', [processId])
      return result.rows as Hydration[]
    },
    getRepushes: async (query?: string, pagination?: PaginationOptions) => {
      let sql = 'SELECT processId, messageId, status, reasons, timestamp FROM repushes'
      const params: any[] = []

      if (query) {
        // Build OR clause for all queryable fields
        const conditions = QUERYABLE_REPUSH_FIELDS.map((field, index) => `${field} = $${index + 1}`).join(' OR ')
        sql += ` WHERE ${conditions}`
        params.push(...QUERYABLE_REPUSH_FIELDS.map(() => query))
      }

      if (pagination?.cursor) {
        const cursorIndex = params.length + 1
        sql += query ? ` AND timestamp > $${cursorIndex}` : ` WHERE timestamp > $${cursorIndex}`
        params.push(pagination.cursor)
      }

      sql += ' ORDER BY timestamp'

      if (pagination?.limit !== undefined) {
        const limitIndex = params.length + 1
        sql += ` LIMIT $${limitIndex}`
        params.push(pagination.limit)
      }

      const result = await pool.query(sql, params)
      return result.rows.map((row: any) => ({
        processId: row.processid,
        messageId: row.messageid,
        status: row.status,
        reasons: row.reasons,
        timestamp: parseInt(row.timestamp)
      }))
    },
    getStatusCounts: async () => {
      const result = await pool.query('SELECT status, COUNT(*) as count FROM hydrations GROUP BY status')
      return result.rows.reduce((acc: Record<string, number>, row: any) => {
        acc[row.status] = parseInt(row.count)
        return acc
      }, {} as Record<string, number>)
    },
    getRepushStatusCounts: async () => {
      const result = await pool.query('SELECT status, COUNT(*) as count FROM repushes GROUP BY status')
      return result.rows.reduce((acc: Record<string, number>, row: any) => {
        acc[row.status] = parseInt(row.count)
        return acc
      }, {} as Record<string, number>)
    },
    deleteProcess: async (processId: string) => {
      const result = await pool.query('DELETE FROM processes WHERE processId = $1', [processId])
      return {
        changes: result.rowCount || 0
      }
    },
    pool
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
