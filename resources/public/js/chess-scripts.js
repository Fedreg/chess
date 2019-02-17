function loadPage() {
    let resText = this.responseText;
    let body = document.getElementById("body");
    body.innerHTML = resText;
}

const get = (url) => {
    let req = new XMLHttpRequest();
    req.addEventListener("load", loadPage);
    req.open("GET", url);
    req.send();
    let res = req.responseText;
    return res;
}

const sendMove = () => {
    let moveFrom = document.getElementById("move-start").value;
    let moveTo = document.getElementById("move-end").value;
    let url = "http://localhost:9000/move?xy=" + moveFrom + moveTo
    get(url);
}

