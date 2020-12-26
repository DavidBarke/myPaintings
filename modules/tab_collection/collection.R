collection_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    collection_header_ui(
      id = ns("collection_header")
    ),
    shiny::uiOutput(
      outputId = ns("images")
    )
  )
}

collection_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      called_rvs <- shiny::reactiveValues(
        image_box_server = integer()
      )
      
      image_box_return <- new.env()
      
      collection_header_return <- collection_header_server(
        id = "collection_header",
        .values = .values
      )
      
      all_image_ids_r <- shiny::reactive({
        db_get_column(.values$db, "image", "rowid")
      })
      
      shiny::observe({
        purrr::walk(all_image_ids_r(), function(image_id) {
          if (!as.integer(image_id) %in% called_rvs$image_box_server) {
            called_rvs$image_box_server <- c(
              called_rvs$image_box_server, image_id
            )
            print(image_id)
            
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
          collection_header_return$filter_r(),
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
        
        indices <- seq_along(image_boxes)
        
        shiny::fluidRow(
          shiny::column(
            width = 3,
            image_boxes[indices %% 4 == 1]
          ),
          shiny::column(
            width = 3,
            image_boxes[indices %% 4 == 2]
          ),
          shiny::column(
            width = 3,
            image_boxes[indices %% 4 == 3]
          ),
          shiny::column(
            width = 3,
            image_boxes[indices %% 4 == 0]
          )
        )
      })
    }
  )
}