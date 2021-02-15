// This could be much nicer JS!

function loadPage() {
    let resText = this.responseText;
    let body = document.getElementById("body");
    body.innerHTML = resText;
}

const baseUrl = "http://localhost:9000/";

const get = (url) => {
    let req = new XMLHttpRequest();
    req.addEventListener("load", loadPage);
    req.open("GET", url);
    req.send();
    let res = req.responseText;
    return res;
}

function randomMove() {
    get(baseUrl + "random-move");
}

function autoMatch () {
    console.log("auto");
    get(baseUrl + "random-move?auto=true");
    setTimeout("autoMatch()", 1000);
}

function undo() {
    let url = baseUrl + "undo";
    get(url);
}
    
function redo() {
    let url = baseUrl + "redo";
    get(url);
}

function selectSquare(el) {
    let div = document.getElementById(el.id);
    let url = baseUrl + "move?xy=";
    let id = el.id;
    get(url + id);
    setTimeout("randomMove()", 1000);
}
