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
            width = 4,
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
            width = 4,
            shiny::selectInput(
              inputId = ns("sort"),
              label = "Sort",
              choices = c(
                "By Title" = "title",
                "By Year" = "year",
                "By Painter" = "painter"
              )
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
      
      return_list <- list(
        filter_r = filter_r,
        sort_r = sort_r
      )
      
      return(return_list)
    }
  )
}