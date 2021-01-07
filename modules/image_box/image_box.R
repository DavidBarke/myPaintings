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
        id = ns("image_box_title")
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
      
      ## Is visible ----
      is_visible_r <- shiny::reactive({
        all(is.na(image_r()))
      })
      
      ## Is offered ----
      is_offered_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(image_r(), {
        is_offered_rv(image_r()$is_offered)
      })
      
      shiny::observeEvent(dropdown_return$offer$is_offered_r(), {
        is_offered_rv(dropdown_return$offer$is_offered_r())
      }, ignoreInit = TRUE)
      
      is_offered_r <- shiny::reactive({
        shiny::req(is_visible_r())
        shiny::req(!is.null(is_offered_rv()))
        is_offered_rv()
      })
      
      ## Price ----
      price_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(c(
        image_r(),
        dropdown_return$offer$is_offered_r()
      ),
      # Make sure that this observeEvent updates last when initialising the
      # server, so that is_offered_r reflects the offered status correctly.
      priority = -1,
      {
        if (!is_offered_r()) return()
        
        price_rv(
          db_get_offered_price(.values$db, image_r()$image_id)
        )
      })
      
      shiny::observeEvent(dropdown_return$price$price_r(), {
        price_rv(dropdown_return$price$price_r())
      }, ignoreInit = TRUE)
      
      price_r <- shiny::reactive({
        if (!is_offered_r()) return(NULL)

        price_rv()
      }) 
      
      ## UI ----
      shiny::observeEvent(options$display_r(), {
        if (options$display_r() %in% c("image", "info")) {
          shiny::updateTabsetPanel(
            inputId = "image_tabset",
            selected = options$display_r()
          )
        }
      })
      
      image_box_title_server(
        id = "image_box_title",
        .values = .values,
        image_r = image_r
      )
      
      dropdown_return <- image_box_dropdown_server(
        id = "image_box_dropdown",
        .values = .values,
        image_r = image_r
      )
      
      return_list <- list(
        is_offered_r = is_offered_r
      )
      
      return(return_list)
    }
  )
}