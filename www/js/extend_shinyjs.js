shinyjs.disable_selectize_input = function(params) {
  // make sure that disabling happens just later than updating selectizeInput
  setTimeout(function() {
    id = "select#" + params.id;
    $el = $(id);
    $el[0].selectize.disable();
  }, 1);
};

shinyjs.scroll_trigger = function(params) {
  $(window).scroll(
    function(e) {
      id = "#" + params.container_id;
      $el = $(id);
      $last_image_box = $el.find(".image-box").eq(-1);
      if ($last_image_box.attr("trigger-scroll") !== undefined) return;
      if ($last_image_box.length && inViewport($last_image_box)) {
        $last_image_box.attr("trigger-scroll", "true");
        Shiny.setInputValue(params.scroll_trigger_id, Math.random());
      }
    }
  );
};

function inViewport($el) {
  var elH = $el.outerHeight(),
      H   = $(window).height(),
      r   = $el[0].getBoundingClientRect(), t=r.top, b=r.bottom;
  return Math.max(0, t>0? Math.min(elH, H-t) : Math.min(b, H));
}