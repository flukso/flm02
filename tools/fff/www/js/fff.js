$(function() {
    var rq = new XMLHttpRequest();
    rq.open("GET", "/cgi-bin/helloworld?error=0");
    rq.onprogress = function(e) {
        $("#stdout").text(rq.responseText);
    };

    rq.send(null);
});
