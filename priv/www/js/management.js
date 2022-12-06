(function() {
    
    function onload(_event) {
    }

    window.dockerCard = (function() {
        let obj = {};

        function create(opts) {
            if(!opts.title || !opts.subtitle || !(opts.data && opts.data instanceof Array && opts.data.length > 0))
                throw {message: "Missing options!", args: [opts]}

            const klasses = ["docker-card", "docker-card-header", "docker-card-info", "docker-card-status"];
            let [divCard, divCardHeader, divCardInfo, divCardStatus] = 
                klasses.map(klass => {
                    let div = document.createElement("div");
                    div.classList.add(klass);
                    return div;
                });

            let title = document.createElement("h4");
            function setTitle(text) {
                title.textContent = text;
            }
            divCard.setTitle = setTitle.bind(divCard);
            divCard.setTitle(opts.title ? opts.title : "");

            let subtitle = document.createElement("h5");
            function setSubtitle(text) {
                subtitle.textContent = text;
            }
            divCard.setSubtitle = setSubtitle.bind(divCard);
            divCard.setSubtitle(opts.subtitle ? opts.subtitle : "");

            divCardHeader.append(title, subtitle);
            divCard.appendChild(divCardHeader);

            let table = document.createElement("table");
            table.classList.add("table", "table-sm");

            function setData(data) {
                if(!(data instanceof Array))
                    throw {message: "Data should be an array!", args: [data]};
                for(let tableN=0; tableN<data.length/3; tableN++) {
                    let from = tableN*3;
                    let to = from+3;
                    table.tBodies.length = 0;
                    let tbody = table.createTBody();
                    tbody.setAttribute("active", tableN === 0);
    
                    if(to > data.length-1)
                        to = data.length;
                    for(let i=from; i<to; i++) {
                        const {name: name, value: value} = data[i];
                        let row = tbody.insertRow();
                        
                        row.insertCell().innerText = name;
                        let rowVal = row.insertCell();
                        if(!(value instanceof Array)) {
                            rowVal.innerText = value;
                            continue;
                        }
                        let arrows = document.createElement("arrows");
                        arrows.append(...value.map(o => {
                            let span = document.createElement("span");
                            span.innerText = o;
                            return span;
                        }));
                        replaceArrow(arrows);
                        rowVal.appendChild(arrows);
                    }
                }
            }

            divCard.setData = setData.bind(divCard);

            divCard.setData(opts.data ? opts.data : []);

            divCardInfo.appendChild(table);
            divCard.appendChild(divCardInfo);

            let spanStatus = document.createElement("span");
            spanStatus.innerText = "Status: ";
            let spanData = document.createElement("span");
            spanData.classList.add("data");

            function setStatus(text) {
                spanData.innerText = text;
            }
            divCard.setStatus = setStatus.bind(divCard);
            if(opts.status)
                divCard.setStatus(opts.status);

            let spanCarousel = document.createElement("span");
            spanCarousel.classList.add("carousel");

            let buttonLT = document.createElement("button");
            buttonLT.innerText = "<";
            buttonLT.addEventListener("click", _event => {
                const tbodies = table.tBodies;
                for(let i=0; i<tbodies.length; i++) {
                    const tbody = tbodies[i];
                    if(tbody.getAttribute("active") !== "true")
                        continue;
                    if(i !== 0){
                        tbody.setAttribute("active", "false");
                        tbodies[i-1].setAttribute("active", "true");
                    }
                    break;
                }
            })
            let buttonGT = document.createElement("button");
            buttonGT.addEventListener("click", _event => {
                const tbodies = table.tBodies;
                for(let i=0; i<tbodies.length; i++) {
                    const tbody = tbodies[i];
                    if(tbody.getAttribute("active") !== "true")
                        continue;
                    if(i !== tbodies.length-1){
                        tbody.setAttribute("active", "false");
                        tbodies[i+1].setAttribute("active", "true");
                    }
                    break;
                }
            })
            buttonGT.innerText = ">";

            spanCarousel.append(buttonLT, buttonGT);
            divCardStatus.append(spanStatus, spanData, spanCarousel)

            divCard.appendChild(divCardStatus);

            return divCard;
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
