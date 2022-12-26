export class ShellQuote {
    constructor() {
        let promise = fetch("https://cdn.jsdelivr.net/npm/shell-quote@1.7.4/index.js");
        promise.then(resp => setup(resp));

        this.promise = function() { return promise; }

        let __loaded = false;

        let obj = this;

        async function setup(resp) {
            if(!resp.ok)
                return;
            let text = await resp.text();
            let exports = {};
            eval(text);
            Object.assign(obj, exports);
            __loaded = true;
        }

        this.loaded = function() {
            return this.__loaded;
        }
    }
}