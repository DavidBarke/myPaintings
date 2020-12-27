collection_header_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      bs4Dash::box(
        width = NULL,
        title = "myPaintings",
        status = "primary",
        solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = ns("filter"),
              label = "Filter",
              choices = c(
                "All" = "all",
                "Offered" = "offered",
                "Not offered" = "not_offered"
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = ns("sort"),
              label = "Sort",
              choices = c(
                "By Title" = "title",
                "By Year" = "year",
                "By Painter" = "painter"
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = ns("display"),
              label = "Display",
              choices = c(
                "Details" = "details",
                "List" = "list"
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = ns("width"),
              label = "Item width",
              choices = c(1, 2, 3, 4, 6, 12),
              selected = 3
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = ns("n_entries"),
              label = "Entries per page",
              choices = c(10, 25, 50, 100),
              selected = 50
            )
          )
        )
      )
    )
  )
}

collection_header_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      filter_r <- shiny::reactive({
        input$filter
      })
      
      sort_r <- shiny::reactive({
        input$sort
      })
      
      display_r <- shiny::reactive({
        input$display
      })
      
      width_r <- shiny::reactive({
        as.integer(input$width)
      })
      
      n_entries_r <- shiny::reactive({
        as.integer(input$n_entries)
      })
      
      return_list <- list(
        display_r = display_r,
        filter_r = filter_r,
        n_entries_r = n_entries_r,
        sort_r = sort_r,
        width_r = width_r
      )
      
      return(return_list)
    }
  )
}