shinyjs.disable_selectize_input = function(params) {
  // make sure that disabling happens just later than updating selectizeInput
  setTimeout(function() {
    id = "select#" + params.id;
    $el = $(id);
    $el[0].selectize.disable();
  }, 1);
};