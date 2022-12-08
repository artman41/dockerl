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
    }

    window.dockerCard = (function() {
        let obj = {};

        function create(opts) {
            if(!opts.title || !opts.subtitle || !(opts.data && opts.data instanceof Array && opts.data.length > 0))
                throw {message: "Missing options!", args: [opts]}

            let dockerCard = htmlToElement(`<div class="docker-card">
                <div class="docker-card-header">
                    <h4 name="title"></h4>
                    <h5 name="subtitle"></h5>
                    <span class="close"></span>
                </div>
                <div class="docker-card-info">
                    <table>
                        <tbody></tbody>
                    </table>
                </div>
                <div class="docker-card-status">
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

            // const spanClose = dockerCard.querySelector(".docker-card-header>span.close");

            const table = dockerCard.querySelector('.docker-card-info>table');
            const templateDataRow = `<tr active="false"><td name="key"></td><td name="value"></td></tr>`;

            function setData(data) {
                if(!(data instanceof Array))
                    throw {message: "Data should be an array!", args: [data]};

                console.log("table: %O", table);
                console.log("table.body: %O", table.body);
                table.tBodies[0].remove();
                let tbody = table.createTBody();
                for(let i=0; i<data.length; i++) {
                    let row = htmlToElement(templateDataRow);
                    row.setAttribute("key", data[i].name);
                    console.log("row: %O", row);
                    console.log("key: %O", row.querySelector("[name='key']"))
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
})()
