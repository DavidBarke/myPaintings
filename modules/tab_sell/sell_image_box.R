sell_image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::bs4Card(
    width = NULL,
    title = "Mona Lisa",
    label = card_label(
      text = shiny::uiOutput(
        outputId = ns("price")
      ),
      status = "danger",
      tooltip = "Price"
    ),
    dropdownMenu = bs4Dash::cardDropdown(
      bs4Dash::cardDropdownItem(
        id = ns("change_price"),
        "Price",
        icon = shiny::icon("edit")
      )
    ),
    solidHeader = TRUE,
    status = "primary",
    maximizable = FALSE,
    shiny::imageOutput(
      outputId = ns("img"),
      height = NULL
    )
  )
}

sell_image_box_server <- function(id, .values, image_id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$img <- shiny::renderImage(deleteFile = FALSE, {
        list(
          src = "./img/mona_lisa.png",
          width = "100%",
          height = "auto"
        )
      })
      
      price_r <- shiny::reactive({
        1000
      })
      
      output$price <- shiny::renderUI({
        paste(price_r(), "$")
      })
      
      shiny::observeEvent(input$change_price, {
        
      })
    }
  )
}