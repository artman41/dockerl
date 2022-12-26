window.util = (function() {
    let obj = {};

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
    function any(arr, lambda) {
        for(let i=0; i<arr.length; i++)
            if(lambda(arr[i]))
                return true;
        return false;
    }
    function all(arr, lambda) {
        for(let i=0; i<arr.length; i++)
            if(!lambda(arr[i]))
                return false;
        return true;
    }
    function toNiceString(obj) {
        if(obj === undefined)
            return "undefined";
        if(obj === null)
            return "null";
        if(obj === NaN)
            return "NaN";
        if(obj instanceof Array)
            return "[" + obj.map(o => toNiceString(o)).join(", ") + "]";
        else if(obj instanceof Object)
            return "{" + Object.entries(obj).map(([key, value]) => key + ": " + toNiceString(value)).join(", ") + "}";
        return obj.toString();
    }

    obj.htmlToElement = htmlToElement.bind(obj);
    obj.any = any.bind(obj);
    obj.all = all.bind(obj);
    obj.toNiceString = toNiceString.bind(obj);

    return obj;
})();

(function() {
    String.prototype.toDOMElement = function() { return util.htmlToElement(this); }
    Array.prototype.any = function(lambda) { return util.any(this, lambda); }
    Array.prototype.all = function(lambda) { return util.all(this, lambda); }
    Object.defineProperty(Object.prototype, 'toNiceString', {value: function() { return util.toNiceString(this); }});
})();