import express from 'express';
import { Deps } from './fn.js';

const LABEL_PATH_MAP: Record<string, string> = {
  'LOAD_PROCESSES': '/api/load',
  'HYDRATE': '/api/hydrate',
  'CRON': '/api/cron',
  'REFRESH_STATUS': '/api/refresh-status',
  'CLEAN_BAD_PROCS': '/api/clean-bad-procs',
  'ROLLING_HYDRATION': '/api/rolling-hydration',
  'RESOLVE_UNPUSHED': '/api/resolve-unpushed'
}

function getLabelForPath(path: string): string | undefined {
  const entry = Object.entries(LABEL_PATH_MAP).find(([_, p]) => p === path)
  return entry?.[0]
}

export const authRequestWith = ({ db }: Deps) => {
  return async (req: express.Request): Promise<any> => {
    const authHeader = req.headers.authorization

    if (!authHeader) {
        return { authorized: false, error: 'No authorization header provided' }
    }

    const token = authHeader.startsWith('Bearer ')
        ? authHeader.substring(7)
        : authHeader

    if (!token) {
        return { authorized: false, error: 'No token provided' }
    }

    const authToken = await db.getAuthToken(token)

    if (!authToken) {
        return { authorized: false, error: 'Invalid token' }
    }

    if (authToken.type === 'ADMIN') {
        return { authorized: true, isAdmin: true }
    }

    /**
     * Right now this only works for hte resolve-unpushed endpoint
     */
    if (authToken.type === 'USER') {
        const permissions = await db.getPermissionsByToken(token)
        const path = req.path
        const body = req.body

        const requiredLabel = getLabelForPath(path)

        if (!requiredLabel) {
            return { authorized: false, error: `Unknown endpoint: ${path}` }
        }

        const requestedProcesses = body?.processes as string[] | undefined

        if (!requestedProcesses || !Array.isArray(requestedProcesses)) {
            return { authorized: false, error: 'Processes array is required' }
        }

        const unauthorizedProcesses: string[] = []

        for (const processId of requestedProcesses) {
            const hasPermission = permissions.some(
                p => p.processId === processId && p.label === requiredLabel
            )

            if (!hasPermission) {
                unauthorizedProcesses.push(processId)
            }
        }

        if (unauthorizedProcesses.length > 0) {
            return {
                authorized: false,
                error: `No ${requiredLabel} permission for processes: ${unauthorizedProcesses.join(', ')}`
            }
        }

        return { authorized: true }
    }

    return { authorized: false, error: 'Unknown token type' }
  }
}