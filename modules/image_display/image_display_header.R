image_display_header_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      width = 8,
      solidHeader = TRUE,
      status = "primary",
      title = "Filter",
      filter_table_ui(
        id = ns("filter_table")
      )
    ),
    bs4Dash::box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      title = "Settings",
      shiny::selectInput(
        inputId = ns("display"),
        label = "Display",
        choices = c(
          "Images" = "image",
          "Infos" = "info",
          "List" = "list"
        )
      ),
      shiny::selectInput(
        inputId = ns("width"),
        label = "Number of columns",
        choices = c(
          "1" = 12,
          "2" = 6,
          "3" = 4,
          "4" = 3
        ),
        selected = 3
      )
    )
  )
}

image_display_header_server <- function(id, .values, display_args) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      user_ids_r <- shiny::reactive({
        2L
      })
      
      display_r <- shiny::reactive({
        input$display
      })
      
      width_r <- shiny::reactive({
        as.integer(input$width)
      })
      
      filter_table_return <- filter_table_server(
        id = "filter_table",
        .values = .values,
        type = display_args$type
      )
      
      return_list <- list(
        display_r = display_r,
        images_r = filter_table_return$images_r,
        image_ids_r = filter_table_return$image_ids_r,
        n_r = shiny::reactive(length(filter_table_return$image_ids_r())),
        width_r = width_r
      )
      
      return(return_list)
    }
  )
}