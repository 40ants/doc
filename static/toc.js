// As the basis I took code from this blog post:
// http://mudox.github.io/post/auto-highlighting-toc/

$(document).ready(function() {
  $(window).scroll(function() {
      $("#page-toc p").removeClass("toc-active");
      currentAnchor().addClass("toc-active");
  })
});

function tocItem(anchor) {
    return $("[href=\"" + anchor + "\"]").parent();
}

function heading(anchor) {
    return $("[id=" + anchor.substr(1) + "]");
}

var _anchors = null;

function anchors() {
    if (!_anchors) {
        _anchors = $("#page-toc a").map(function() {
            return $(this).attr("href");
        })
    }
    return _anchors;
}

function currentAnchor() {
    var winY = window.pageYOffset;
    var currAnchor = null;
    anchors().each(function() {
        var y = heading(this).position().top;
        if (y < winY + window.innerHeight * 0.23) {
            currAnchor = this;
            return;
        }
    })
    return tocItem(currAnchor);
}
