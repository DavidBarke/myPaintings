collection_images_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("images")
  )
}

collection_images_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      called_rvs <- shiny::reactiveValues(
        image_box_server = integer()
      )
      
      image_box_return <- new.env()
      
      all_image_ids_r <- shiny::reactive({
        db_get_column(.values$db, "image", "rowid")
      })
      
      shiny::observe({
        purrr::walk(all_image_ids_r(), function(image_id) {
          if (!as.integer(image_id) %in% called_rvs$image_box_server) {
            called_rvs$image_box_server <- c(
              called_rvs$image_box_server, image_id
            )
            
            image_box_return[[as.character(image_id)]] <- image_box_server(
              id = "image_box" %_% image_id,
              .values = .values,
              image_id = image_id
            )
          }
        })
      })
      
      is_offered_r <- shiny::reactive({
        purrr::map_lgl(all_image_ids_r(), function(image_id) {
          image_box_return[[as.character(image_id)]]
          image_box_return[[as.character(image_id)]]$is_offered_r()
        })
      })
      
      filter_image_ids_r <- shiny::reactive({
        ids <- all_image_ids_r()
        
        switch(
          options$filter_r(),
          "all" = ids,
          "offered" = ids[is_offered_r()],
          "not_offered" = ids[!is_offered_r()]
        )
      })
      
      image_ids_r <- filter_image_ids_r
      
      output$images <- shiny::renderUI({
        image_boxes <- purrr::map(image_ids_r(), function(image_id) {
          image_box_ui(
            id = ns("image_box" %_% image_id)
          )
        })
        
        distribute_boxes(image_boxes, width = options$width_r())
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
    }
  )
}