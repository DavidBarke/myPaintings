collection_images_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # shiny::fluidRow(
    #   shiny::column(
    #     width = 4,
    #     id = "column-1"
    #   ),
    #   shiny::column(
    #     width = 4,
    #     id = "column-2"
    #   ),
    #   shiny::column(
    #     width = 4,
    #     id = "column-3"
    #   )
    # ),
    shiny::uiOutput(
      outputId = ns("images")
    ),
    shiny::actionButton(
      inputId = ns("load_more"),
      label = "Load more",
      width = "100%"
    )
  )
}

collection_images_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      vis_start <- 20
      
      prepared_step <- 50
      
      # This reactiveVal holds the number of currently visible image boxes. It
      # gets incrementey by user scroll and gets reset whenever a new user
      # request (filter) is processed.
      current_visible_index_r <- shiny::reactiveVal(vis_start)
      current_prepared_index_r <- shiny::reactiveVal(prepared_step)
      
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
      
      result_image_ids_r <- shiny::reactive({
        db_get_image_ids_by_filter(
          db = .values$db,
          filter = options$filter_r()
        )
      })
      
      ## Visible output ----
      visible_indices_r <- shiny::reactive({
        seq_len(min(
          current_visible_index_r(),
          length(result_image_ids_r())
        ))
      })
      
      rvs <- shiny::reactiveValues(
        image_boxes = purrr::map(1:prepared_step, function(index) {
          image_box_ui(
            id = ns("image_box" %_% index)
          )
        })
      )
      
      output$images <- shiny::renderUI({
        image_boxes <- rvs$image_boxes
        
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
        
        n <- 12 / as.integer(width)
        
        columns <- purrr::map(seq_len(n), function(i) {
          # For last element mod is 0
          if (i == n) i <- 0
          shiny::column(
            width = width,
            boxes[indices %% n == i]
          )
        })
        
        shiny::fluidRow(columns)
      }
      
      shiny::observeEvent(input$load_more, {
        vis_index <- current_visible_index_r()
        
        offset <- 20
        
        current_visible_index_r(vis_index + offset)
        
        new_server_indices <- (vis_index + 1):(vis_index + offset)
        
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
        
        if (current_visible_index_r() > current_prepared_index_r()) {
          new_ui_indices <- (current_prepared_index_r() + 1):
            (current_prepared_index_r() + prepared_step)
          
          rvs$image_boxes <- c(
            rvs$image_boxes,
            purrr::map(new_ui_indices, function(index) {
              image_box_ui(
                id = ns("image_box" %_% index)
              )
            })
          )
          
          current_prepared_index_r(current_prepared_index_r() + prepared_step)
        }
      })
    }
  )
}