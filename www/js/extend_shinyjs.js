shinyjs.disable_selectize_input = function(params) {
  // make sure that disabling happens just later than updating selectizeInput
  setTimeout(function() {
    id = "select#" + params.id;
    $el = $(id);
    $el[0].selectize.disable();
  }, 1);
};

shinyjs.reset_scroll_trigger = function(params) {
  scroll_trigger = $("#" + params.id);
  scroll_trigger.attr("last-height", $(window).scrollTop() + $(window).height());
};