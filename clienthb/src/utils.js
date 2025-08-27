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