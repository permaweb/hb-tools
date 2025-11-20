export function parseArgs(name) {
    const args = process.argv.slice(2);
    const flags = {};
    let group = null;

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg.startsWith('--')) {
            const [key, value] = arg.slice(2).split('=');
            if (value !== undefined) {
                flags[key] = value;
            } else if (i + 1 < args.length && !args[i + 1].startsWith('--')) {
                flags[key] = args[++i];
            } else {
                flags[key] = true;
            }
        } else if (!group) {
            group = arg;
        }
    }

    if (!group) {
        console.error(`Usage: node ${name} <group> [--url <url>] [--scheduler <address>] [--config <path>]`);
        process.exit(1);
    }

    return { group, flags };
}

function getType(v) {
    if (v === null) return 'null';
    if (Array.isArray(v)) return 'array';
    const tag = Object.prototype.toString.call(v).slice(8, -1).toLowerCase();
    const prim = typeof v;
    if (['number', 'string', 'boolean', 'undefined', 'function', 'bigint', 'symbol'].includes(prim)) return prim;
    return tag;
}

function normalizeExpectedType(expected) {
    if (typeof expected === 'string') return expected.toLowerCase();
    if (typeof expected === 'function') {
        // Map common constructors to type strings
        const ctorMap = new Map([
            [Number, 'number'], [String, 'string'], [Boolean, 'boolean'],
            [Array, 'array'], [Object, 'object'], [Function, 'function'],
            [Date, 'date'], [Map, 'map'], [Set, 'set'], [RegExp, 'regexp']
        ]);
        return ctorMap.get(expected) || 'object';
    }
    return getType(expected);
}

export function createTestRunner() {
    let passed = 0;
    let failed = 0;
    let startTime = null;

    return {
        start() {
            startTime = Date.now();
        },

        async test(testFn) {
            const start = Date.now();
            try {
                const result = await testFn();
                passed++;
                const duration = ((Date.now() - start) / 1000).toFixed(2);
                return { result, duration };
            } catch (error) {
                console.error(error);
                failed++;
                const duration = ((Date.now() - start) / 1000).toFixed(2);
                throw error;
            }
        },

        getResults() {
            return { passed, failed };
        },

        getSummary(testSuiteName = 'Tests') {
            const exitCode = failed > 0 ? 1 : 0;

            if (failed > 0) {
                console.log(`\x1b[31m${testSuiteName} Failed\x1b[0m`);
            } else {
                console.log(`\x1b[32mAll ${testSuiteName} Passed\x1b[0m`);
            }

            console.log(`${testSuiteName} Summary:`);
            console.log(`\x1b[32mPassed\x1b[0m: ${passed}`);
            console.log(`\x1b[${exitCode === 1 ? '31' : '90'}mFailed\x1b[0m: ${failed}`);

            if (startTime !== null) {
                const totalDuration = ((Date.now() - startTime) / 1000).toFixed(2);
                console.log(`\x1b[1mTotal duration: ${totalDuration}s\x1b[0m`);
            }

            // Output parseable test counts for run-group.js
            console.log(`TEST_RESULTS: passed=${passed} failed=${failed} total=${passed + failed}`);

            return exitCode;
        }
    };
}

export function expect(actual) {
    return {
        toBeDefined: () => {
            console.log('\x1b[90m%s\x1b[0m', `Checking if value is defined: ${JSON.stringify(actual)}`);
            if (actual === undefined) {
                throw new Error(`Expected value to be defined, but it was undefined`);
            }
            console.log('\x1b[32m%s\x1b[0m', 'Success: Value is defined');
        },
        toHaveProperty: (prop) => {
            console.log('\x1b[90m%s\x1b[0m', `Checking if object ${JSON.stringify(actual)} has property '${prop}'`);
            if (!(prop in actual)) {
                throw new Error(`Expected object to have property '${prop}', but it was not found`);
            }
            console.log('\x1b[32m%s\x1b[0m', `Success: Object has property '${prop}'`);
        },
        toEqualType: (expected) => {
            const actualType = getType(actual);
            const expectedType = normalizeExpectedType(expected);

            console.log('\x1b[90m%s\x1b[0m', `Checking type, actual: ${actualType}, expected: ${expectedType}`);

            if (actualType !== expectedType) {
                throw new Error(`Type mismatch: expected ${expectedType}, but got ${actualType}`);
            }
            console.log('\x1b[32m%s\x1b[0m', `Success: Types match (${actualType})`);
        },
        toEqualLength: (expected) => {
            console.log('\x1b[90m%s\x1b[0m', `Checking length, actual: ${actual.length}, expected: ${expected}`);
            if (actual.length !== expected) {
                throw new Error(`Array length mismatch: expected length ${expected}, but got ${actual.length}`);
            }
            console.log('\x1b[32m%s\x1b[0m', `Success: Array length is equal (${actual.length})`);
        },
        toEqual: (expected) => {
            console.log(
                '\x1b[90m%s\x1b[0m',
                `Checking equality, actual: ${JSON.stringify(actual)}, expected: ${JSON.stringify(expected)}`,
            );
            const actualType = typeof actual;
            const expectedType = typeof expected;
            if (actualType !== expectedType) {
                throw new Error(`Type mismatch: expected ${expectedType}, but got ${actualType}`);
            }

            if (actualType === 'object' && actual !== null && expected !== null) {
                const actualKeys = Object.keys(actual);
                const expectedKeys = Object.keys(expected);
                console.log(
                    '\x1b[90m%s\x1b[0m',
                    `Checking object keys, actual keys: ${JSON.stringify(actualKeys)}, expected keys: ${JSON.stringify(expectedKeys)}`,
                );
                if (actualKeys.length !== expectedKeys.length) {
                    throw new Error(`Object key count mismatch: expected ${expectedKeys.length}, but got ${actualKeys.length}`);
                }

                for (const key of actualKeys) {
                    if (!(key in expected)) {
                        throw new Error(`Expected object is missing key: ${key}`);
                    }
                    expect(actual[key]).toEqual(expected[key]);
                }
            } else if (actual !== expected) {
                throw new Error(`Value mismatch: expected ${expected}, but got ${actual}`);
            }
            console.log('\x1b[32m%s\x1b[0m', 'Success: Values are equal');
        },
    };
}