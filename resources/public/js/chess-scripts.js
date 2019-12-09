// This could be much nicer JS!

function loadPage() {
    let resText = this.responseText;
    console.log("RESTEXT", resText);
    let body = document.getElementById("body");
    if (resText != "illegal") {
        body.innerHTML = resText;
    }
    else {
        console.log("ILLEGAL MOVE");
    }
}

const get = (url) => {
    let req = new XMLHttpRequest();
    req.addEventListener("load", loadPage);
    req.open("GET", url);
    req.send();
    let res = req.responseText;
    return res;
}

const arr = [];

function selectSquare(el) {
    let div = document.getElementById(el.id);
    let url = "http://localhost:9000/move2?xy=";
    let url2 = "http://localhost:9000/move?xy=";
    let id = el.id;
    let bgcol = el.style.backgroundColor;
    let col = el.style.color;

    if (arr.length == 0) {
        get(url + id);
        arr.push([id, bgcol, col]);
    }
    // else if (arr.length == 1 && id != arr[0][0]) {
    else {
        let firstId = arr[0][0];
        document.getElementById(firstId).style.backgroundColor = arr[0][1];
        // div.style.backgroundColor = bgcol;
        get(url2 + arr[0][0] + id);
        arr.pop(firstId);
    }
    // else if (id == arr[0][0]) {
        // arr.pop(id);
    // }
    // else arr.pop(arr[0]);
}
