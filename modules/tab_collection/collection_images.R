collection_images_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("images")
    ),
    scroll_trigger(
      inputId = ns("scroll_trigger")
    )
  )
}

collection_images_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      vis_start <- 20
      load_offset <- 5
      
      prepared_step <- 50
      
      # This reactiveVal holds the number of currently visible image boxes. It
      # gets incrementey by user scroll and gets reset whenever a new user
      # request (filter) is processed.
      current_visible_index_r <- shiny::reactiveVal(vis_start)
      current_prepared_index_r <- shiny::reactiveVal(prepared_step)
      
      ui_index_rv <- shiny::reactiveVal(prepared_step)
      server_index_rv <- shiny::reactiveVal(vis_start)
      
      ui_blocked_rv <- shiny::reactiveVal(FALSE)
      server_blocked_rv <- shiny::reactiveVal(FALSE)
      
      ui <- new.env()
      
      purrr::walk(1:vis_start, function(index) {
        image_box_server(
          id = "image_box" %_% index,
          .values = .values,
          # Ever image box uses its index to retrieve its current image_id
          # by indexing result_image_ids_r
          index = index,
          result_image_ids_r = result_image_ids_r,
          options = options
        )
      })
      
      ui$boxes <- purrr::map(1:prepared_step, function(index) {
        image_box_ui(
          id = ns("image_box" %_% index)
        )
      })
      
      ## Visible output ----
      visible_indices_r <- shiny::reactive({
        seq_len(min(
          current_visible_index_r(),
          length(result_image_ids_r())
        ))
      })
      
      output$images <- shiny::renderUI({
        image_boxes <- ui$boxes
        
        if (options$display_r() %in% c("image", "info")) {
          # Column-based layout
          distribute_boxes(image_boxes, width = options$width_r())
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
            id = paste("img-col", i, sep = "-"),
            boxes[indices %% n == i]
          )
        })
        
        shiny::fluidRow(columns)
      }
      
      shiny::observeEvent(
        ignoreInit = TRUE,
        current_visible_index_r(), 
        {
          ui_blocked_rv(TRUE)
          server_blocked_rv(TRUE)
          
          vis_index <- current_visible_index_r()
          
          new_server_indices <- (shiny::isolate(server_index_rv()) + 1):vis_index
          
          purrr::walk(new_server_indices, function(index) {
            image_box_server(
              id = "image_box" %_% index,
              .values = .values,
              # Ever image box uses its index to retrieve its current image_id
              # by indexing result_image_ids_r
              index = index,
              result_image_ids_r = result_image_ids_r,
              options = options
            )
          })
          
          server_index_rv(vis_index)
          ui_blocked_rv(FALSE)
          server_blocked_rv(FALSE)
        }
      )
      
      shiny::observeEvent(current_visible_index_r(), {
        vis_index <- current_visible_index_r()
        prep_index <- current_prepared_index_r()
        
        if (vis_index > prep_index) {
          new_ui_indices <- (prep_index + 1):(prep_index + prepared_step)
          
          new_boxes <- purrr::map(new_ui_indices, function(index) {
            image_box_ui(
              id = ns("image_box" %_% index)
            )
          })
          
          n_col <- 12 / options$width_r()
          
          indices <- seq_along(new_boxes)
          
          purrr::map(seq_len(n_col), function(i) {
            # For last element mod is 0
            if (i == n_col) i <- 0
            shiny::insertUI(
              selector = paste0("#img-col-", i),
              where = "beforeEnd",
              ui = new_boxes[indices %% n_col == i]
            )
          })
          
          ui$boxes <- c(
            ui$boxes,
            new_boxes
          )
          
          current_prepared_index_r(prep_index + prepared_step)
        }
      })
      
      scroll_trigger_r <- shiny::throttle(
        millis = 1000,
        shiny::reactive({
          input$scroll_trigger
        })
      )
      
      shiny::observeEvent(scroll_trigger_r(), {
        current_visible_index_r(current_visible_index_r() + load_offset)
      })
      
      result_image_ids_r <- shiny::reactive({
        db_get_image_ids_by_filter(
          db = .values$db,
          filter = options$filter_r()
        )
      })
    }
  )
}