(function() {
    const DEBUG = false;

    const currentScript = document.currentScript;

    const EVENT_OnDraggablePickup = "draggablePickup";
    const EVENT_OnDraggableDrop = "draggableDrop";
    const EVENT_OnDraggableMove = "draggableMove";

    const ATTR_IsDragging = "draggable-isDragging";
    const ATTR_DraggableArea = "draggableArea";
    const ATTR_Draggable = "draggable";
    const ATTR_Snappable = "snappable";
    const ATTR_IsSnapped = "snappable-isSnapped";

    const ELEM_DraggableSpacer = "DRAGGABLE_SPACER";

    function logTable(params, properties = undefined) {
        if(!DEBUG)
            return;
        let x = params;
        Object.entries(x).forEach(([key, value]) => x[key] = value.constructor === Object ? value : JSON.parse(JSON.stringify(value)))
        console.table(x, properties);
    }

    let draggable = (function() {
        function maybeAddCSS() {
            if(document.querySelector(`link[src*='draggable.css']`) !== null)
                return;
            let link = document.createElement("link");
            let url = new URL(currentScript.src);
            let cssPath = [...(url.pathname.split("/").slice(0, url.pathname.lastIndexOf("/") - 1)), "../css/draggable.css"].join("/");
            link.href = cssPath;
            link.rel = "stylesheet";
            currentScript.before(link);
        }

        return function(_) {
            maybeAddCSS();

            let deltaX = 0, deltaY = 0;
            let isDraggingSomething = false;
            let dragTarget = undefined;

            let posX = 0, posY = 0;

            function toDraggable(elem) {
                [
                    ["setIsDragging", function(b) {this.setAttribute(ATTR_IsDragging, b === true)}],
                    ["isDragging", function() {return this.getAttribute(ATTR_IsDragging) === "true"}],
                    ["setPosition", function(x, y) {
                        normaliseX = (n) => {
                            let computedStyleMap = window.getComputedStyle(this);
                            let thisRect = this.getBoundingClientRect();
                            let parentRect = this.parentElement.getBoundingClientRect();
                            let paddingLeft = computedStyleMap["padding-left"];
                            let borderWidth = (computedStyleMap["border-top-left-radius"] ?? computedStyleMap["border-bottom-left-radius"]) / 100;
                            if ((n - paddingLeft - borderWidth) < 0)
                                return paddingLeft + borderWidth;
                            else if((n+ thisRect.width + borderWidth) > parentRect.right) 
                                return parentRect.right - thisRect.width - borderWidth;
                            else
                                return n;
                        };
                        normaliseY = (n) => {
                            let computedStyleMap = window.getComputedStyle(this);
                            let thisRect = this.getBoundingClientRect();
                            let parentRect = this.parentElement.getBoundingClientRect();
                            let paddingTop = computedStyleMap["padding-top"];
                            let paddingBottom = computedStyleMap["padding-bottom"];
                            let borderWidth = (computedStyleMap["border-top-left-radius"] ?? computedStyleMap["border-top-right-radius"]) / 100;
                            if ((n - paddingTop - borderWidth) < 0)
                                return paddingTop + borderWidth;
                            else if((n+paddingBottom + borderWidth) > parentRect.bottom)
                                return parentRect.bottom - thisRect.height - borderWidth;
                            else
                                return n;
                        };

                        posX = normaliseX(x - deltaX);
                        posY = normaliseY(y - deltaY);
                        this.style.left = `${window.scrollX + posX}px`;
                        this.style.top = `${window.scrollY + posY}px`;
                    }],
                    ["resetPosition", function() {
                        this.style.left = `${window.scrollX + posX}px`;
                        this.style.top = `${window.scrollY + posY}px`;
                    }],
                ].forEach(([funcName, func]) => elem[funcName] = func.bind(elem));
                return elem;
            }

            const WHICH_LEFT = 1;
            const WHICH_MIDDLE = 2;
            const WHICH_RIGHT = 3;
            const WHICH_BACK = 4;
            const WHICH_FORWARD = 5;

            function onPointerDown(event) {
                if(event.which !== WHICH_LEFT)
                    return;
                const draggable = toDraggable(event.target);
                const draggableArea = draggable.parentElement;
                let computedStyleMap = window.getComputedStyle(draggable);
                let {left: left, top: top, width: width, height: height} = draggable.getBoundingClientRect();

                let spacer = document.createElement(ELEM_DraggableSpacer);
                spacer.style.left = `${left}px`;
                spacer.style.top = `${top}px`;
                spacer.style.width = `${width}px`;
                spacer.style.height = `${height - 2}px`;
                spacer.style.display = computedStyleMap["display"];

                draggableArea.appendChild(spacer);
                draggable.before(spacer);

                function diffNums(A, B) {
                    return A > B ? A - B : B - A;
                }

                deltaX = event.clientX - left;
                deltaY = event.clientY - top;
                draggable.setIsDragging(true);
                isDraggingSomething = true;
                dragTarget = this;
                draggableArea.lastElementChild.after(draggable);
                draggable.setPosition(event.clientX, event.clientY);

                let curr = [posX, posY];

                let f = (event_) => {
                    let xDiff = diffNums(event_.detail.x, curr[0]);
                    let yDiff = diffNums(event_.detail.y, curr[1]);
                    if(xDiff < 20 && yDiff < 20) 
                        return;
                    if(spacer.parentElement === null)
                        return;
                    spacer.parentElement.removeChild(spacer);
                    draggable.removeEventListener(EVENT_OnDraggableMove, f);
                };
                draggable.addEventListener(EVENT_OnDraggableMove, f);
                draggable.addEventListener(EVENT_OnDraggableDrop, _event => {
                    if(spacer.parentElement === null) 
                        return;
                    spacer.parentElement.removeChild(spacer);
                }, {once: true});

                this.dispatchEvent(new CustomEvent(EVENT_OnDraggablePickup, {bubbles: true, detail: {x: posX, y: posY} }));
                if(DEBUG)
                    console.log("[%O] <%O> Event: %O, %O", event.type, event.target, event, [left, top]);
            }
            function onPointerUp(event) {
                if(event.which !== WHICH_LEFT)
                    return;
                const draggable = toDraggable(event.target);
                draggable.setIsDragging(false);
                isDraggingSomething = false;
                dragTarget = undefined;
                this.dispatchEvent(new CustomEvent(EVENT_OnDraggableDrop, {bubbles: true, detail: {x: posX, y: posY} }));
                if(DEBUG)
                    console.log("[%O] <%O> Event: %O", event.type, event.target, event);
            }
            function onPointerCancel(event) {
                const draggable = toDraggable(event.target);
                if(DEBUG)
                    console.log("[%O] <%O> Event: %O", event.type, event.target, event);
            }
            function onPointerMove(event) {
                if(!isDraggingSomething || dragTarget === undefined){
                    return;
                }
                const draggable = dragTarget;
                draggable.setPosition(event.clientX, event.clientY);
                this.dispatchEvent(new CustomEvent(EVENT_OnDraggableMove, {bubbles: true, detail: {x: posX, y: posY} }));
                if(DEBUG)
                    console.log("[%O] <%O> Event: %O", event.type, event.target, event);
            }

            function onPointerLeave(event) {
                if(!isDraggingSomething || dragTarget === undefined){
                    return;
                }
                const draggable = dragTarget;
                if(DEBUG)
                    console.log("[%O] <%O> Event: %O", event.type, event.target, event);
            }

            let draggableElems = document.querySelectorAll(`[${ATTR_DraggableArea}]>[${ATTR_Draggable}]`);
            draggableElems.forEach(o => {
                o.addEventListener("pointerdown", onPointerDown, {capture: true});
                o.addEventListener("pointerup", onPointerUp, {capture: true});
                o.addEventListener("pointercancel", onPointerCancel, {capture: true});
                o.addEventListener("pointermove", onPointerMove, {capture: true});
                o.addEventListener("pointerleave", onPointerLeave, {capture: true});
            });

            const areas = Array.from(draggableElems).map(o => o.parentElement).filter((o, i, arr) => {
                if(i === 0)
                    this[arr] = []; 
                let ret = false;
                if(o !== null && !this[arr].includes(o)){
                    this[arr].push(o); 
                    ret = true; 
                };
                if(i === arr.length-1)
                    delete this[arr];
                return ret;
            });
            areas.forEach(area => {
                area.addEventListener("pointermove", onPointerMove, {capture: true});
                area.addEventListener("pointerleave", onPointerLeave, {capture: true});
            });

            window.addEventListener("onselectstart", (_event) => !isDraggingSomething);
            window.addEventListener("scroll", (_event) => { if(isDraggingSomething && dragTarget !== undefined) dragTarget.resetPosition(); });
        }
    })();

    let snappable = (function() {

        function getRect(elem) {
            if(!(elem instanceof HTMLElement))
                return null;
            const computedStyle = window.getComputedStyle(elem);
            let boundingRect = elem.getBoundingClientRect();
            let rect = {
                x: Number.parseFloat(computedStyle.left),
                y: Number.parseFloat(computedStyle.top),
                left: Number.parseFloat(computedStyle.left),
                top: Number.parseFloat(computedStyle.top),
                right: Number.parseFloat(computedStyle.left) + Number.parseFloat(computedStyle.width),
                bottom: Number.parseFloat(computedStyle.top) + Number.parseFloat(computedStyle.height)
            }
            Object.keys(rect).forEach(key => rect[key] = isNaN(rect[key]) ? boundingRect[key] : rect[key]);
            return rect;
        }
    
        function overlaps(elem, area) {
            const elemRect = getRect(elem);
            const areaRect = getRect(area);
    
            logTable({elem: elemRect, area: areaRect});
    
            gte = (a, b, text = null) => { 
                if(DEBUG)
                    console.log("%O%O >= %O", text == null ? "" : text + " ", a, b); 
                return a >= b; 
            };
            lte = (a, b, text = null) => { 
                if(DEBUG)
                    console.log("%O%O <= %O", text == null ? "" : text + " ", a, b); 
                return a <= b; 
            };
    
            return gte(elemRect.top, areaRect.top, "[top]") &&
                   gte(elemRect.left, areaRect.left, "[left]") &&
                   lte(elemRect.right, areaRect.right, "[bottom]") &&
                   lte(elemRect.bottom, areaRect.bottom, "[left]");
        }
    
        function onDraggableDrop(event) {
            const elem = event.target;
    
            const snapholders = document.querySelectorAll(`[${ATTR_DraggableArea}]>[${ATTR_Snappable}]`);
    
            let snapholder = undefined;
            for(let i=0; i<snapholders.length; i++) {
                if(!overlaps(elem, snapholders[i]))
                    continue;
                snapholder = snapholders[i];
                break;
            }
            let foundSnapholder = snapholder !== undefined;
            elem.setAttribute(ATTR_IsSnapped, foundSnapholder);
            if(!foundSnapholder){
                if(elem.parentElement.getAttribute(ATTR_Snappable) !== null)
                    elem.parentElement.after(elem);
                return;
            }
    
            const children = snapholder.childNodes;
            let target = undefined;
            const elemRect = getRect(elem);
            for(let i=0; i<children.length; i++) {
                const child = children[i];
                if(child.tagName === ELEM_DraggableSpacer)
                    continue;
                const childRect = getRect(child);
                logTable({elem_text: elem.innerText, elem: elemRect, child_text: child.innerText, child: childRect});
                if(elemRect.left > childRect.left)
                    continue;
                target = child;
                break;
            }
    
            if(target === undefined)
                snapholder.appendChild(elem);
            else
                target.before(elem);
    
            elem.style.left = "";
            elem.style.top = "";
        }

        return function(_) {
            document.addEventListener(EVENT_OnDraggableDrop, onDraggableDrop);
        }
    })();

    function onload(_) {
        draggable();
        snappable();
    }

    if(document.readyState === "complete" || document.readyState === "interactive")
        onload();
    else
        window.addEventListener("load", onload);
})();