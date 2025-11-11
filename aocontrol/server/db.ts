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

export interface SqliteClient {
  query: (statement: SqlStatement) => Promise<any[]>
  run: (statement: SqlStatement) => Promise<Database.RunResult>
  transaction: (statements: SqlStatement[]) => Promise<Database.RunResult[]>
  pragma: (value: string, options?: Database.PragmaOptions) => Promise<any>
  saveProcess: (processId: string, timestamp?: number) => Promise<Database.RunResult>
  saveHydration: (processId: string, url: string, status: string, timestamp?: number) => Promise<Database.RunResult>
  getAllProcessIds: () => Promise<string[]>
  getHydrationsByProcessId: (processId: string) => Promise<Hydration[]>
  getStatusCounts: () => Promise<Record<string, number>>
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
  }

  return {
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
    getAllProcessIds: async () => {
      const rows = db.prepare('SELECT processId FROM processes').all() as { processId: string }[]
      return rows.map(row => row.processId)
    },
    getHydrationsByProcessId: async (processId: string) => {
      const rows = db.prepare('SELECT url, status FROM hydrations WHERE processId = ?').all(processId) as Hydration[]
      return rows
    },
    getStatusCounts: async () => {
      const rows = db.prepare('SELECT status, COUNT(*) as count FROM hydrations GROUP BY status').all() as Array<{ status: string, count: number }>
      return rows.reduce((acc, { status, count }) => {
        acc[status] = count
        return acc
      }, {} as Record<string, number>)
    },
    db
  }
}
