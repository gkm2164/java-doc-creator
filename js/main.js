function highlightBlock(id) {
    var $elem = $('#def-' + id);
    $elem[0].scrollIntoView({
        behavior: 'smooth',
        block: 'nearest'
    });

    $elem.removeClass('animation-focus');
    setTimeout(function () {
        $elem.addClass('animation-focus')
    }, 10);

}

var min = 300;
var max = 3600;
var mainmin = 200;

$(function () {
    $('#split-bar').mousedown(function (e) {
        e.preventDefault();
        $(document).mousemove(function (e) {
            e.preventDefault();
            var x = e.pageX - $('#sidebar').offset().left;
            if (x > min && x < max && e.pageX < ($(window).width() - mainmin)) {
                $('#sidebar').css("width", x);
                $('#split-bar').css('margin-left', x);
                $('#main').css("margin-left", x);
            }
        });
    });
    $(document).mouseup(function (e) {
        $(document).unbind('mousemove');
    });
});