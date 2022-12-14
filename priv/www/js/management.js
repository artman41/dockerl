(function() {

    /**
     * @param {String} HTML representing a single element
     * @return {Element}
     */
    function htmlToElement(html) {
        var template = document.createElement('template');
        html = html.trim(); // Never return a text node of whitespace as the result
        template.innerHTML = html;
        return template.content.firstChild;
    }
    
    function onload(_event) {
        let buttons = document.querySelectorAll("#popup input[type='button'][role]");
        for(let i=0; i<buttons.length; i++)
            buttons[i].addEventListener("click", event => popup.events.raise(buttons[i].getAttribute("role"), event));

        ["light", "dark"].forEach(klass => {
            let elem = htmlToElement(`<template class="${klass}"></template`);
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

    window.dockerCard = (function() {
        let obj = {};

        function create(opts) {
            if(!opts.title || !opts.subtitle || !(opts.data && opts.data instanceof Array && opts.data.length > 0))
                throw {message: "Missing options!", args: [opts]}

            let dockerCard = htmlToElement(`<div class="docker-card" draggable>
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
            </div>`);

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
                        let timeoutFunc = (delay) => setTimeout(() => reject("timeout"), delay);
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
                    let row = htmlToElement(templateDataRow);
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
                    row = htmlToElement(templateDataRow);
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
                    let option = htmlToElement(`<option>${v}</option>`);
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
            let resp = await fetch("/api/image/" + imageId);
            if(!resp.ok)
                throw "failed to inspect image " + imageId;
            let obj = JSON.parse(await resp.text());
            let container = htmlToElement(`<div monaco-container></div>`);
            let opts = {
                value: JSON.stringify(obj, null, 4),
                language: "json",
                automaticLayout: true,
                scrollBeyondLastLine: false
            };
            document.body.addEventListener("onThemeTransitionStart", _event => opts.theme = document.body.classList.contains("dark") ? "custom-dark" : "custom-light");
            let _editor = monaco.editor.create(container, opts);
            popup.show("Image " + imageId.slice(0, 8), false, container);
            container.parentElement.classList.add("ignore-theme");
        }

        obj.popup.inspect = inspect.bind(obj.popup);

        function createContainer(imageId, imageName) {
            const elem = htmlToElement(`<form>
                <div class="mb-3">
                    <label for="exampleInputEmail1" class="form-label">Email address</label>
                    <input type="email" class="form-control" id="exampleInputEmail1" aria-describedby="emailHelp">
                    <div id="emailHelp" class="form-text">We'll never share your email with anyone else.</div>
                </div>
                <div class="mb-3">
                    <label for="exampleInputPassword1" class="form-label">Password</label>
                    <input type="password" class="form-control" id="exampleInputPassword1">
                </div>
                <div class="mb-3 form-check">
                    <input type="checkbox" class="form-check-input" id="exampleCheck1">
                    <label class="form-check-label" for="exampleCheck1">Check me out</label>
                </div>
                <button type="submit" class="btn btn-primary">Submit</button>
            </form>`);
            popup.show(`Creating a container of ${imageName} (${imageId.slice(0, 8)})`, true, elem);
        }

        obj.popup.createContainer = createContainer.bind(obj.popup)

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
                
                this.listeners[type].push({
                    func: listener,
                    executions: opts["executions"] === undefined ? -1 : opts["executions"]
                });
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

            function raise(type, data) {
                if(!(type !== undefined && type.toString().length > 0))
                    throw {message: "type is not a string!", args: [type, data]};
                if(!(data instanceof Object))
                    throw {message: "data is not an object!", args: [type, data]};
                const listeners = this.listeners[type];
                if(listeners === undefined || listeners.length === 0)
                    return;
                let removals = [];
                for(let i=0; i<listeners.length; i++){
                    listeners[i].func(data);
                    if(listeners[i].executions -1 == 0)
                        removals.push({type: type, listener: listeners[i]});
                    else
                    listeners[i].executions-=1
                }
                for(let i=0; i<removals.length; i++){
                    let {type: type, listener: listener} = removals[i];
                    removeListener(type, listener);
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

        obj.events.addListener("submit", () => obj.hide());
        obj.events.addListener("cancel", () => obj.hide());
        obj.events.addListener("ok", () => obj.hide());

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