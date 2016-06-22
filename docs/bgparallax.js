if (document.addEventListener && document.getElementById) (function() {
  document.addEventListener("scroll", function() {
    var bg_distance = 5.0;
    var xpos = (-window.pageXOffset / bg_distance);
    var ypos = (-window.pageYOffset / bg_distance);
    document.documentElement.style.backgroundPosition = xpos+"px "+ypos+"px";
  }, false);
})();

