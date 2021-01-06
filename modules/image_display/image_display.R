image_display_ui <- display_ui_factory(
  header_ui = image_display_header_ui,
  results_number_ui = display_results_number_ui,
  content_ui = image_display_content_ui
)

image_display_server <- display_server_factory(
  header_server= image_display_header_server,
  results_number_server = display_results_number_server,
  content_server = image_display_content_server
)