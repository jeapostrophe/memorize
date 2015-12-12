function u(v) {
    return v + "?callback=?";
}

$( document ).ready(function() {
    $(document).keydown(function(e){
        if ((e.keyCode || e.which) == 32) { // space
            showb();
        }
        if ((e.keyCode || e.which) == 37) { // left
            fire("no");
        }
        if ((e.keyCode || e.which) == 39) { // right
            fire("yes");
        }   
    });

    var shown = false;
    var c = {};
    
    function showb() {
        shown = true;
        $("#card").html( c['back'] ).attr('class', 'back');
    }

    function fire(what) {
        if ( shown ) {
            $.getJSON(u("/api/click/" + what), {}, function (r) {
                doNext();
            });
        }
    }
    
    function doNext() {
        $.getJSON(u("/api/next"), {}, function (r) {
            c = r;
            shown = false;
            $("#card").html( c['front'] ).attr('class', 'front');
        }); }

    doNext();
});
