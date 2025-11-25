$(document).ready(function() {
    // Handle internal links
    $('a[href^="#"]').on('click', function(e) {
        e.preventDefault();
        var target = $(this).attr('href');
        var $target = $(target);

        if ($target.length) {
            $('html, body').animate({
                scrollTop: $target.offset().top
            }, 0); // Adjust the duration if needed
        }
    });

    // Handle URL hash on load
    var hash = window.location.hash;
    if (hash) {
        var $target = $(hash);
        if ($target.length) {
            $('html, body').scrollTop($target.offset().top);
        }
    }
});