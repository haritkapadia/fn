const path = require("path")
const fs = require("fs")
const stream = require("stream")
const streamPromises = require("stream/promises")

// Downloads file
// Optionally, skips file if it exists
// Optionally, a filename can be given. Otherwise, the filename is derived from the URL.
const download = async (url, skipExists, filename) => {
    if (filename === undefined) {
        filename = path.basename(new URL(url).pathname)
    }
    if (skipExists && fs.existsSync(filename)) {
        return
    }
    const res = await fetch(url)
    const buf = await res.arrayBuffer()
    return fs.promises.writeFile(filename, Buffer.from(new Uint8Array(buf)))
}
