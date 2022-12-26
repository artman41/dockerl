const DEBUG = false;

import { Docker } from "./docker.js";
import { ShellQuote } from './shell-quote.js';

let shellQuote = new ShellQuote();

if(DEBUG) {
    function createScript(src) {
        let script = document.createElement("script");
        script.src = src;
        return script;
    }
    document.currentScript.before(createScript("https://cdn.jsdelivr.net/bluebird/latest/bluebird.js"));

    Promise.longStackTraces();
}

(function() {
    function onload(_event) {
        let buttons = document.querySelectorAll("#popup input[type][role].btn");
        for(let i=0; i<buttons.length; i++)
            buttons[i].addEventListener("click", event => popup.events.raise(buttons[i].getAttribute("role"), event));

        ["light", "dark"].forEach(klass => {
            let elem = `<template class="${klass}"></template`.toDOMElement();
            document.body.appendChild(elem);
            let style = window.getComputedStyle(elem);
            let bgColour = style.getPropertyValue("--docker-secondary").trim();

            function colourToHex(str){ 
                let ctx = document.createElement('canvas').getContext('2d'); 
                ctx.fillStyle = str; 
                return ctx.fillStyle; 
            }

            monaco.editor.defineTheme(`custom-${klass}`, {
                base: `vs${klass === "light" ? "" : "-" + klass}`,
                inherit: true,
                rules: [],
                colors: {
                    "editor.background": colourToHex(bgColour),
                },
            });
            elem.remove();
        });
        if(document.body.classList.contains("dark"))
            monaco.editor.setTheme("custom-dark");
        else
            monaco.editor.setTheme("custom-light");
    }

    window.toasts = (function() {
        const severities = {
            DEBUG: "debug",
            INFO: "info",
            WARN: "warn",
            ERROR: "error"
        };
        let obj = {
            severities: severities
        };

        function add(message, description, options) {
            const toastContainer = document.getElementById("toasts");
            options = options === undefined ? {} : options;
            const defaults = {
                severity: severities.INFO
            }
            options.getValue = (function(key) {
                return this[key] === undefined ? defaults[key] : this[key];
            }).bind(options);
            let toast = `<div class="toast" role="alert" aria-live="assertive" aria-atomic="true" data-animation="true" data-autohide="false" data-severity="${options.getValue("severity")}">
                <div class="toast-header">
                    <!-- <img src="..." class="rounded mr-2" alt="..."> -->
                    <strong class="mr-auto">${message}</strong>
                    <!-- <small>11 mins ago</small> -->
                    <button type="button" class="ml-2 mb-1 close" data-dismiss="toast" aria-label="Close">
                        <span aria-hidden="true">&times;</span>
                    </button>
                </div>
                <div class="toast-body">${description instanceof HTMLElement ? description.outerHTML : description}</div>
            </div>`.toDOMElement();
            toastContainer.appendChild(toast);
            $(toast).toast('show');
        }

        obj.add = add.bind(obj);

        return obj;
    })();

    window.dockerCard = (function() {
        let obj = {};

        function create(opts) {
            if(!opts.title || !opts.subtitle || !(opts.data && opts.data instanceof Array && opts.data.length > 0))
                throw {message: "Missing options!", args: [opts]}

            let dockerCard = `<div class="docker-card" draggable>
                <div class="docker-card-gripper pb-2"></div>
                <div class="docker-card-overlay" notGrabbable></div>
                <div class="docker-card-header" notGrabbable>
                    <span class="container-icons">
                        <span class="container-refresh material-symbols-outlined">refresh</span>
                        <span class="container-remove material-symbols-outlined">close</span>
                    </span>
                    <h4 name="title"></h4>
                    <h5 name="subtitle"></h5>
                </div>
                <div class="docker-card-info" notGrabbable>
                    <table>
                        <tbody></tbody>
                    </table>
                </div>
                <div class="docker-card-status" notGrabbable>
                    <span>Status: </span>
                    <span class="data"></span>
                    <select>
                        <option value="" disabled selected hidden>Actions</option>
                    </select>
                </div>
            </div>`.toDOMElement();

            const title = dockerCard.querySelector(".docker-card-header>h4[name='title']");
            function setTitle(text) {
                title.textContent = text;
            }
            dockerCard.setTitle = setTitle.bind(dockerCard);
            dockerCard.setTitle(opts.title ? opts.title : "");

            const subtitle = dockerCard.querySelector(".docker-card-header>h5[name='subtitle']");
            function setSubtitle(text) {
                subtitle.textContent = text;
            }
            dockerCard.setSubtitle = setSubtitle.bind(dockerCard);
            dockerCard.setSubtitle(opts.subtitle ? opts.subtitle : "");

            const spanRefresh = dockerCard.querySelector(".docker-card-header span.container-refresh");
            function timedPromise(eventType, func_finally) {
                let promise = new Promise((resolve, reject) => {
                    function eventf() {
                        let timeoutFunc = (delay) => setTimeout(() => reject(new Error("timeout")), delay);
                        const delay = 5000;
                        let _now = new Date().getTime();
                        let _triggerTime = _now + delay;
                        let _paused_timeLeft = null;
                        let _timeout = null;

                        let _resolved = false;
                        let _rejected = false;
                        let _fulfilled = false;

                        function getTimeLeft(){
                            var now = new Date();
                            return _triggerTime - now;
                        }
                    
                        function pause(){
                            if(_timeout === null)
                                return;
                            _paused_timeLeft = getTimeLeft();
                    
                            clearTimeout(_timeout);
                            _timeout = null;
                        }
                    
                        function resume(){
                            if (_timeout !== null)
                                return;
                            _timeout = timeoutFunc(_paused_timeLeft === null ? delay : _paused_timeLeft);
                        }

                        function clear() {
                            if(_timeout === null)
                                return;
                            _paused_timeLeft = 0;
                            clearTimeout(_timeout);
                        }

                        let promiseObj = {
                            resolve: (...args) => {resolve(...args); _resolved = true; _fulfilled = true;}, 
                            reject: (...args) => {reject(...args); _rejected = true; _fulfilled = true;},
                            isResolved: () => _resolved,
                            isRejected: () => _rejected,
                            isFulfilled: () => _fulfilled,
                        }

                        let obj = {promise: promiseObj};
                        obj.pauseTimeout = pause.bind(obj);
                        obj.resumeTimeout = resume.bind(obj);
                        obj.clearTimeout = clear.bind(obj);
                        obj.resumeTimeout();
                        return new CustomEvent(eventType, {
                            bubbles: true, 
                            detail: obj
                        });
                    };
                    dockerCard.dispatchEvent(eventf());
                });
                if(func_finally !== undefined && func_finally instanceof Function)
                    return promise.finally(func_finally);
                else
                    return promise;
            }
            spanRefresh.addEventListener("click", _event => {
                if(dockerCard.getAttribute("refreshing") === "true")
                    return;
                    dockerCard.setAttribute("refreshing", true);
                timedPromise("refresh", () => dockerCard.setAttribute("refreshing", false));
            });

            const spanRemove = dockerCard.querySelector(".docker-card-header span.container-remove");
            spanRemove.addEventListener("click", _event => {
                if(dockerCard.getAttribute("removing") === "true")
                    return;
                dockerCard.setAttribute("removing", true);
                timedPromise("remove", () => dockerCard.setAttribute("removing", false));
            });

            const table = dockerCard.querySelector('.docker-card-info>table');
            const templateDataRow = `<tr active="false"><td name="key"></td><td name="value"></td></tr>`;

            function setData(data) {
                if(!(data instanceof Array))
                    throw {message: "Data should be an array!", args: [data]};

                table.tBodies[0].remove();
                let tbody = table.createTBody();
                for(let i=0; i<data.length; i++) {
                    let row = templateDataRow.toDOMElement();
                    row.setAttribute("key", data[i].name);
                    row.querySelector("[name='key']").innerText = data[i].name;
                    row.querySelector("[name='value']").innerText = data[i].value;
                    tbody.appendChild(row);
                }
            }

            dockerCard.setData = setData.bind(dockerCard);
            dockerCard.setData(opts.data ? opts.data : []);

            function updateData(data) {
                if(!(data instanceof Array))
                    throw {message: "Data should be an array!", args: [data]};

                const tbody = table.tBodies[0];
                for(let i=0; i<data.length; i++) {
                    let row = table.querySelector(`[key='${data[i].name}']`);
                    if(row !== null) {
                        row.querySelector("[name='value']").innerText = data[i].value;
                        continue;
                    }
                    row = templateDataRow.toDOMElement();
                    row.querySelector("[name='key']").innerText = data[i].name;
                    row.querySelector("[name='value']").innerText = data[i].value;
                    if(tbody.rows.length <= 3)
                        row.setAttribute("active", "true");
                    tbody.appendChild(row);
                }
            }
            dockerCard.updateData = updateData.bind(dockerCard);

            const spanData = dockerCard.querySelector('.docker-card-status>span.data');

            function setStatus(text) {
                spanData.innerText = text;
            }
            dockerCard.setStatus = setStatus.bind(dockerCard);
            if(opts.status)
                dockerCard.setStatus(opts.status);

            const actions = dockerCard.querySelector('.docker-card-status>select');
            if(opts.actions && opts.actions instanceof Array)
                for(let i=0; i< opts.actions.length; i++) {
                    let {value: v, func: f} = opts.actions[i];
                    if(!v || !f || !(f instanceof Function))
                        continue;
                    let option = `<option>${v}</option>`.toDOMElement();
                    actions.addEventListener("change", event => {
                        const selected = event.target.selectedIndex;
                        if(selected !== i+1 && event.target.options[selected] !== option)
                            return;
                        event.target.selectedIndex = 0;
                        f(option);
                    });
                    actions.appendChild(option);
                }

            return dockerCard;
        }

        obj.create = create.bind(obj);

        obj.popup = {};
        
        async function inspect(imageId) {
            let resp = await docker.getImage(imageId);
            let jsonString = JSON.stringify(resp.data, null, 4);
            if(!resp.ok){
                toasts.add(`${imageId.slice(0, 8)}: ${resp.statusText}`, `<pre>${jsonString}</pre>`.toDOMElement(), {severity: toasts.severities.ERROR});
                return;
            }
            let container = `<div monaco-container></div>`.toDOMElement();
            let opts = {
                value: jsonString,
                language: "json",
                automaticLayout: true,
                scrollBeyondLastLine: false
            };
            document.body.addEventListener("onThemeTransitionStart", _event => opts.theme = document.body.classList.contains("dark") ? "custom-dark" : "custom-light");
            let _editor = monaco.editor.create(container, opts);
            popup.show("Image " + imageId.slice(0, 8), false, container);
            container.parentElement.classList.add("ignore-theme");
            popup.events.addListener("ok", () => popup.hide(), {executions: 1});
        }

        obj.popup.inspect = inspect.bind(obj.popup);

        function containerPrompt(header) {
            let portCode = 
                "if(this.value < this.min) this.value = this.min;" +
                "if(this.value > this.max) this.value = this.max;";
            let row = `<div class="row ml-0 mr-0" style="align-content: center;">
                <input placeholder="" name="port-from" type="number" min="1025" max="65536" class="form-control col-3" onchange="${portCode}">
                <span style="margin-top: -0.2em;">
                    <span class="material-symbols-outlined d-block position-relative">arrow_right_alt</span>
                    <span class="material-symbols-outlined d-block position-relative">arrow_right_alt</span>
                    <span class="material-symbols-outlined d-block position-relative">arrow_right_alt</span>
                </span>
                <input placeholder="" name="port-to" type="number" min="1025" max="65536" class="form-control col-3" onchange="${portCode}">
                <select>
                    <option>tcp</option>
                    <option>udp</option>
                    <option>sctp</option>
                </select>
            </div>`;
            const elem = `<form class="was-validated">
                <div class="mb-3">
                    <label class="form-label">Container Name</label>
                    <input name="container-name" type="text" class="form-control" required>
                </div>
                <div class="mb-3">
                    <label class="form-label">Ports <span name="add_input" class="material-symbols-outlined align-text-bottom font-weight-bold">Add</span></label>
                    <!-- ${row} -->
                </div>
                <div class="mb-3">
                    <label class="form-label">Command</label>
                    <input name="container-command" type="text" class="form-control" value="/bin/echo 'Hello World!'">
                </div>
            </form>`.toDOMElement();

            function toData() {
                let entries = new FormData(this).entries();
                let obj = {
                    containerName: null,
                    ports: null,
                    command: null
                }
                while(true) {
                    let next = entries.next();
                    if(next.done)
                        break;
                    let [key, value] = next.value;
                    switch (key) {
                        case "container-name":
                            obj.containerName = value.replace(/\s/g, "_");
                            break;
                        case "port-from":
                            if(obj.ports === null)
                                obj.ports = [];
                            obj.ports.push({from: value});
                            break;
                        case "port-to":
                            if(obj.ports === null || obj.ports.length === 0)
                                continue;
                            obj.ports[obj.ports.length-1].to = value;
                            break;
                        case "container-command":
                            obj.command = shellQuote.parse(value);
                            break;
                    }
                };
                return obj;
            }
            elem.toData = toData.bind(elem);

            const addInput = elem.querySelector("span[name='add_input']");
            addInput.addEventListener("click", event => {
                let elem = row.toDOMElement();
                let inputs = elem.querySelectorAll('input');
                inputs.forEach(input => input.addEventListener("focusout", _ => {
                    setTimeout(() => {
                        if(inputs.all(input => input.value === "") && document.activeElement.parentElement !== elem) 
                            elem.remove(); 
                    }, 1);
                }));
                event.target.parentElement.parentElement.appendChild(elem);
                inputs[0].focus();
            });
            popup.show(header, true, elem);

            return elem;
        }

        obj.popup.containerPrompt = containerPrompt.bind(obj.popup)

        return obj;
    })();

    window.popup = (function() {
        let obj = {};

        let dispatcher = (function() {
            let obj = {
                listeners: {},
            };

            function addListener(type, listener, opts = {}) {
                if(!(type !== undefined && type.toString().length > 0))
                    throw {message: "type is not a string!", args: [type, listener]};
                if(!(listener instanceof Function))
                    throw {message: "Listener is not a function!", args: [type, listener]};
                if(this.listeners[type] === undefined)
                    this.listeners[type] = [];

                let obj = {
                    executions: opts["executions"] === undefined ? -1 : opts["executions"],
                    ignoreExecution: false
                };

                let functions = {
                    ignoreExecution: function() {
                        obj.ignoreExecution = true;
                    },
                    setExecutions: function(number) {
                        if (number !== undefined && typeof(number) != "number" && number < 1)
                            return false;
                        obj.executions = number === undefined ? -1 : number;
                    }
                }
                
                obj.func = listener.bind(functions);
                
                this.listeners[type].push(obj);
            }

            function removeListener(type, listener) {
                if(!(type !== undefined && type.toString().length > 0))
                    throw {message: "type is not a string!", args: [type, listener]};
                if(!(listener instanceof Function))
                    throw {message: "Listener is not a function!", args: [type, listener]};
                if(this.listeners[type] === undefined || this.listeners[type].length === 0)
                    return;
                this.listeners[type] = this.listeners[type].filter(o => o.func !== listener);
            }

            async function raise(type, data) {
                if(!(type !== undefined && type.toString().length > 0))
                    throw {message: "type is not a string!", args: [type, data]};
                if(!(data instanceof Object))
                    throw {message: "data is not an object!", args: [type, data]};
                const listeners = this.listeners[type];
                if(listeners === undefined || listeners.length === 0)
                    return;
                let removals = [];
                let after = {resolve: undefined, reject: undefined};
                data.$after = new Promise((resolve, reject) => after = {resolve: resolve, reject: reject});
                data.$after.catch(_ => {});
                
                let promises = [];
                let success = true;
                for(let i=0; i<listeners.length; i++){
                    let ret = undefined;
                    try {
                        ret = listeners[i].func(data);
                    } catch(ex) {
                        ret = false;
                    }
                    if(ret instanceof Promise)
                        promises.push(ret);
                    else if(success && !ret)
                        success = false;
                    if(!listeners[i].ignoreExecution)
                        if(listeners[i].executions -1 == 0)
                            removals.push({type: type, listener: listeners[i]});
                        else if(listeners[i].executions > 0)
                            listeners[i].executions-=1
                    else
                        listeners[i].ignoreExecution = false;
                }
                for(let i=0; i<promises.length && success; i++) {
                    let ret = await promises[i];
                    if(!ret)
                        success = false;
                }
                if(success)
                    after.resolve();
                else
                    after.reject(new Error("1 or more EventHandlers returned false"));
                for(let i=0; i<removals.length; i++){
                    let {type: type, listener: listener} = removals[i];
                    this.removeListener(type, listener.func);
                }
            }

            obj.addListener = addListener.bind(obj);
            obj.removeListener = removeListener.bind(obj);
            obj.raise = raise.bind(obj);

            return obj;
        })();
        obj.events = dispatcher;

        let domElem = null;
        function getElement() {
            if(domElem !== null)
                return domElem;
            domElem = document.getElementById("popup");
            return domElem;
        }

        function showPopup(name, isForm, ...elems) {
            if(name === undefined || isForm === undefined || !(elems instanceof Array && elems.length > 0)) 
                throw {message: "Bad args", args: [name, isForm, elems]};
            if(document.body.getAttribute("data-popup") === "true")
                return;
            const divPopup = this.getElement();
            divPopup.setAttribute("isForm", isForm === true);
            const divTitle = divPopup.querySelector('[name="title"]');
            divTitle.innerText = name;
            const divElems = divPopup.querySelector('[name="elems"]');
            for(let i=0; i<divElems.children.length; i++)
                divElems.removeChild(divElems.children[i]);
            divElems.append(...elems);
            document.body.setAttribute("data-popup", true);
            this.events.raise("show", {name: name, isForm: isForm});
        }

        function hidePopup() {
            if(document.body.getAttribute("data-popup") === "false")
                return;
            document.body.setAttribute("data-popup", false);
            this.events.raise("hide", {});
        }

        obj.getElement = getElement.bind(obj);
        obj.show = showPopup.bind(obj);
        obj.hide = hidePopup.bind(obj);

        let hideFunc = event => {event.$after.then(() => obj.hide(), () => {});};
        obj.events.addListener("submit", hideFunc);
        obj.events.addListener("cancel", () => obj.hide());
        obj.events.addListener("ok", hideFunc);

        return obj;
    })();

    window.addEventListener("load", onload);
})();

function replaceArrow(arrow) {
    let spanArrow = document.createElement("span");
    spanArrow.classList.add("pointer");

    function arrowClick(event) {
        let direction = event.target.getAttribute("direction");
        if(direction == null)
            return;
        const parent = event.target.parentNode;
        const actives = parent.querySelectorAll("[active]");
        for(let i=0; i<actives.length; i++) {
            const node = actives[i];
            if(node.getAttribute("active") === "false")
                continue;
            let nextIndex = direction === "left" ? (i-1 < 0 ? i : i-1) : (i+1 > (actives.length-1) ? i : i+1);
            if(nextIndex !== i) {
                node.setAttribute("active", false);
                actives[nextIndex].setAttribute("active", true);
            }
            break;
        }
    }
    
    let leftArrow = spanArrow.cloneNode(true)
    leftArrow.innerText = "<";
    leftArrow.setAttribute("direction", "left");
    let rightArrow = spanArrow.cloneNode(true)
    rightArrow.innerText = ">";
    rightArrow.setAttribute("direction", "right");

    const childNodes = arrow.childNodes;
    let anyActive = false;
    let firstChild = null;
    for(let i=0; i< childNodes.length; i++) {
        const node = childNodes[i];
        if(!(node instanceof HTMLElement))
            continue;
        if (firstChild === null)
            firstChild = node;
        let active = node.getAttribute("active");
        if(active === null) {
            node.setAttribute("active", false);
            continue;
        }
        if(!active)
            continue;
        anyActive = true;
    }
    if(!anyActive)
        firstChild.setAttribute("active", true);

    let clonedLeftArrow = leftArrow.cloneNode(true);
    clonedLeftArrow.addEventListener("click", arrowClick)
    childNodes[0].before(clonedLeftArrow);

    let clonedRightArrow = rightArrow.cloneNode(true);
    clonedRightArrow.addEventListener("click", arrowClick)
    arrow.appendChild(clonedRightArrow);
    arrow.setAttribute("active", true);
}

(function replaceArrows() {
    let arrows = document.querySelectorAll("arrows");
    
    for(let i=0; i<arrows.length; i++) {
        const arrow = arrows[i];
        replaceArrow(arrow);
    }
})();

(function() {
    let transitionTimer = undefined;
    function humanToMillis(str) {
        return str.match(/\d+\s?\w/g)
            .reduce((acc, cur, i) => {
                let multiplier = 1000;
                switch (cur.slice(-1)) {
                    case 'h':
                        multiplier *= 60;
                    case 'm':
                        multiplier *= 60;
                    case 's':
                        return ((parseInt(cur)?parseInt(cur):0) * multiplier) + acc;
                }
                return acc;
            }, 0);
    }
    function replaceTheme(classList) {
        if(classList.contains("light")) {
            classList.replace("light", "dark");
            monaco.editor.setTheme("custom-dark");
            return "dark";
        }
        classList.replace("dark", "light");
        monaco.editor.setTheme("custom-light");
        return "light";
    }
    function toggleTheme() {
        const classList = document.body.classList;
        if(transitionTimer !== undefined) {
            clearTimeout(transitionTimer);
            classList.remove("theme-transition");
            transitionTimer = undefined;
            setTimeout(this.toggleTheme, 1);
            return;
        }
        document.body.dispatchEvent(new CustomEvent("onThemeTransitionStart", {}));
        classList.add("theme-transition");
        let time = window.getComputedStyle(document.body).getPropertyValue("--theme-transition-time");
        transitionTimer = setTimeout(() => {
            classList.remove("theme-transition");
            document.body.dispatchEvent(new CustomEvent("onThemeTransitionStop", {}));
        }, humanToMillis(time));
        localStorage.setItem("wasDarkMode", replaceTheme(classList) === "dark")
    }
    document.body.toggleTheme = toggleTheme.bind(document.body);
    if(localStorage.getItem("wasDarkMode") === "true"){
        document.body.classList.add("no-transition");
        replaceTheme(document.body.classList);
        setTimeout(() => document.body.classList.remove("no-transition"), 1);
    }
})();