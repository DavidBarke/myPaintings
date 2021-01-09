image_box_ui <- function(id, image) {
  ns <- shiny::NS(id)
  
  status_choices <- c("primary", "orange", "olive", "fuchsia")
  
  status <- sample(status_choices, 1)
  
  htmltools::div(
    class = "image-box",
    bs4Dash::tabBox(
      id = ns("image_tabset"),
      width = NULL,
      side = "right",
      title = image_box_title_ui(
        id = ns("image_box_title"),
        image = image,
        price_badge_id = ns("price_badge")
      ),
      dropdownMenu = bs4Dash::boxDropdown(
        image_box_dropdown_ui(
          id = ns("image_box_dropdown")
        )
      ),
      solidHeader = TRUE,
      status = status,
      maximizable = TRUE,
      collapsible = FALSE,
      shiny::tabPanel(
        title = "Image",
        value = "image",
        icon = shiny::icon("image"),
        image_box_image_ui(
          id = ns("image_box_image"),
          src = image$path
        )
      ),
      shiny::tabPanel(
        title = "Info",
        value = "info",
        icon = shiny::icon("info-circle"),
        image_box_info_ui(
          id = ns("image_box_info"),
          image = image
        )
      )
    )
  )
}

image_box_server <- function(
  id, .values, 
  image_r,
  options # List of reactives. Options from the collection header.
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      shiny::observeEvent(options$display_r(), {
        if (options$display_r() %in% c("image", "info")) {
          shiny::updateTabsetPanel(
            inputId = "image_tabset",
            selected = options$display_r()
          )
        }
      })
      
      dropdown_return <- image_box_dropdown_server(
        id = "image_box_dropdown",
        .values = .values,
        image_r = image_r,
        box_id = ns("image_tabset"),
        price_badge_id = ns("price_badge"),
        click_badge_r = shiny::reactive(shiny::req(input$price_badge))
      )
    }
  )
}