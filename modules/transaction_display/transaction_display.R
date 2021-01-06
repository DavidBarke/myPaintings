transaction_display_ui <- display_ui_factory(
  header_ui = transaction_display_header_ui,
  results_number_ui = display_results_number_ui,
  content_ui = transaction_display_content_ui
)

transaction_display_server <- display_server_factory(
  header_server= transaction_display_header_server,
  results_number_server = display_results_number_server,
  content_server = transaction_display_content_server
)