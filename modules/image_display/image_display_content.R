image_display_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("images")
    ) %>% shinycssloaders::withSpinner(),
    scroll_trigger(
      inputId = ns("scroll_trigger"),
      containerId = ns("images")
    )
  )
}

image_display_content_server <- function(id, .values, display_args, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      server_start <- 20
      load_offset <- 5
      
      max_loaded_server_rv <- shiny::reactiveVal(server_start)
      
      # This reactiveVal holds the number of currently visible image boxes. It
      # gets incrementey by user scroll and gets reset whenever a new user
      # request (filter) is processed.
      current_visible_index_rv <- shiny::reactiveVal(0)
      last_visible_index_rv <- shiny::reactiveVal(0)
      
      ui <- new.env()
      
      purrr::walk(1:server_start, function(index) {
        image_box_server(
          id = "image_box" %_% index,
          .values = .values,
          image_r = shiny::reactive(options$images_r()[index,]),
          options = options
        )
      })
      
      result_image_ids_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(options$image_ids_r(), {
        result_image_ids_rv(options$image_ids_r())
      })
      
      result_image_ids_r <- shiny::reactive({
        result_image_ids_rv()
      })
      
      result_offered_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(options$image_ids_r(), {
        result_offered_rv(options$images_r()$is_offered)
      })
      
      result_offered_r <- shiny::reactive({
        result_offered_rv()
      })
      
      shiny::observeEvent(result_image_ids_r(), {
        shiny::removeUI(
          selector = paste0("#", ns("images"), " .image-box"),
          multiple = TRUE,
          immediate = TRUE
        )
        
        current_visible_index_rv(5)
        last_visible_index_rv(0)
      }, priority = 1)
      
      ## Visible output ----
      output$images <- shiny::renderUI({
        image_boxes <- ui$boxes
        
        if (options$display_r() %in% c("image", "info")) {
          # Column-based layout
          # distribute_boxes(image_boxes, width = options$width_r())
          n_col <- 12 / options$width_r()
          
          columns <- purrr::map(seq_len(n_col), function(i) {
            # For last element mod is 0
            if (i == n_col) i <- 0
            shiny::column(
              width = options$width_r(),
              id = ns(paste("img-col", i, sep = "-"))
            )
          })
          
          shiny::fluidRow(
            columns
          )
        } else {
          # Row-based layout
          columns <- purrr::map(image_boxes, function(image_box) {
            shiny::column(
              width = options$width_r(),
              image_box
            )
          })
          
          shiny::fluidRow(
            columns
          )
        }
      })
      
      distribute_boxes <- function(boxes, width) {
        indices <- seq_along(boxes)
        
        n <- 12 / width
        
        columns <- purrr::map(seq_len(n), function(i) {
          # For last element mod is 0
          if (i == n) i <- 0
          shiny::column(
            width = width,
            id = ns(paste("img-col", i, sep = "-")),
            boxes[indices %% n == i]
          )
        })
        
        shiny::fluidRow(columns)
      }
      
      shiny::observeEvent(current_visible_index_rv(), {
        vis_index <- current_visible_index_rv()
        last_vis_index <- last_visible_index_rv()
        max_server <- max_loaded_server_rv()
        
        if (vis_index > max_server) {
          new_indices <- (max_server + 1):(vis_index)
          
          purrr::walk(new_indices, function(index) {
            image_box_server(
              id = "image_box" %_% index,
              .values = .values,
              image_r = shiny::reactive(options$images_r()[index,]),
              options = options
            )
          })
          
          max_loaded_server_rv(vis_index)
        }
        
        if (vis_index > last_vis_index) {
          new_indices <- (last_vis_index + 1):vis_index
          
          new_boxes <- purrr::map(new_indices, function(index) {
            image_box_ui(
              id = ns("image_box" %_% index),
              image = options$images_r()[index,]
            )
          })
          
          ui$boxes <- c(ui$boxes, new_boxes)
          
          n_col <- 12 / options$width_r()
          
          purrr::walk2(new_boxes, new_indices, function(box, index) {
            i <- index %% n_col
            if (i == n_col) i <- 0
            shiny::insertUI(
              selector = paste0("#", ns(paste0("img-col-", i))),
              where = "beforeEnd",
              ui = box
            )
          })
          
          last_visible_index_rv(current_visible_index_rv())
        }
      })
      
      scroll_trigger_r <- shiny::throttle(
        millis = 1000,
        shiny::reactive({
          input$scroll_trigger
        })
      )
      
      shiny::observeEvent(scroll_trigger_r(), {
        current_visible_index_rv(current_visible_index_rv() + load_offset)
      })
    }
  )
}