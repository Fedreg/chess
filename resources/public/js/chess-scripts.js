// This could be much nicer JS!

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

function selectSquare(el) {
    let div = document.getElementById(el.id);
    let url = "http://localhost:9000/move?xy=";
    let id = el.id;
    get(url + id);
}
