// This could be much nicer JS!

function loadPage() {
    let resText = this.responseText;
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
    let url = "http://localhost:9000/move?xy=";
    let id = el.id;
    let bgcol = el.style.backgroundColor;
    let col = el.style.color;

    if (arr.length == 0) {
        arr.push([id, bgcol, col]);
        div.style.backgroundColor = "#0c60f0";
        div.style.color = "#000";
    }
    else if (arr.length == 1 && id != arr[0][0]) {
        let firstId = arr[0][0];
        document.getElementById(firstId).style.backgroundColor = arr[0][1];
        div.style.backgroundColor = bgcol;
        get(url + arr[0][0] + id);
        arr.pop(firstId);
    }
    else if (id == arr[0][0]) {
        document.getElementById(arr[0][0]).style.backgroundColor = arr[0][1];
        document.getElementById(arr[0][0]).style.color = arr[0][2];
        arr.pop(id);
    }
    else arr.pop(arr[0]);
}
