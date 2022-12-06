(function() {
    
    function onload(_event) {
    }

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
