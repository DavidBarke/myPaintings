image_box_ui <- function(id, src) {
  ns <- shiny::NS(id)
  
  status_choices <- c("primary", "orange", "olive", "fuchsia")
  
  status <- sample(status_choices, 1)
  
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
        src = src
      )
    ),
    shiny::tabPanel(
      title = "Info",
      value = "info",
      icon = shiny::icon("info-circle"),
      image_box_info_ui(
        id = ns("image_box_info")
      )
    )
  )
}

image_box_server <- function(
  id, .values, 
  index, # Index of box.
  result_image_ids_r, # Image ids of all images that match a request.
  result_offered_r, # Is displayed image offered?
  options # List of reactives. Options from the collection header.
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      image_id_r <- shiny::reactive({
        result_image_ids_r()[index]
      })
      
      entry_r <- shiny::reactive({
        db_get_image_entry_by_image_id(
          db = .values$db,
          image_id = image_id_r()
        )
      })
      
      ## Is visible ----
      is_visible_r <- shiny::reactive({
        index <= length(result_image_ids_r())
      })
      
      ## Is offered ----
      is_offered_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(result_offered_r(), {
        is_offered_rv(result_offered_r()[index])
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
        image_id_r(),
        dropdown_return$offer$is_offered_r()
      ),
      # Make sure that this observeEvent updates last when initialising the
      # server, so that is_offered_r reflects the offered status correctly.
      priority = -1,
      {
        if (!is_offered_r()) return()
        
        price_rv(
          db_get_offered_price(.values$db, image_id_r())
        )
      })
      
      shiny::observeEvent(dropdown_return$price$price_r(), {
        price_rv(dropdown_return$price$price_r())
      }, ignoreInit = TRUE)
      
      price_r <- shiny::reactive({
        if (!is_offered_r()) return(NULL)

        price_rv()
      }) 
      
      ## Owner ----
      owner_name_r <- shiny::reactive({
        db_get_image_owner(.values$db, image_id_r())
      })
      
      ## Image ----
      image_r <- shiny::reactive({
        c(
          entry_r(),
          list(
            is_offered = is_offered_r(),
            price = price_r(),
            owner = owner_name_r(),
            index = index
          )
        )
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
      
      image_box_info_server(
        id = "image_box_info",
        .values = .values,
        image_r = image_r,
        status = status
      )
      
      return_list <- list(
        is_offered_r = is_offered_r
      )
      
      return(return_list)
    }
  )
}