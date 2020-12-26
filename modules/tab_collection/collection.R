collection_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bs4Dash::box(
          width = NULL,
          title = "Images",
          status = "primary",
          solidHeader = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(
          outputId = ns("images_1")
        )
      ),
      shiny::column(
        width = 3,
        shiny::uiOutput(
          outputId = ns("images_2")
        )
      ),
      shiny::column(
        width = 3,
        shiny::uiOutput(
          outputId = ns("images_3")
        )
      ),
      shiny::column(
        width = 3,
        shiny::uiOutput(
          outputId = ns("images_4")
        )
      )
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
      
      image_ids_r <- shiny::reactive({
        DBI::dbGetQuery(.values$db, "SELECT rowid FROM image")$rowid
      })
      
      shiny::observe({
        image_ui <- purrr::map(image_ids_r(), function(image_id) {
          if (!as.integer(image_id) %in% called_rvs$image_box_server) {
            called_rvs$image_box_server <- c(
              called_rvs$image_box_server, image_id
            )
            
            image_box_server(
              id = "image_box" %_% image_id,
              .values = .values,
              image_id = image_id
            )
          }
        })
      })
      
      output$images_1 <- shiny::renderUI({
        indices <- seq_along(image_ids_r())
        indices <- indices[indices %% 4 == 1]
        
        image_ui <- purrr::map(indices, function(image_id) {
          image_box_ui(
            id = ns("image_box" %_% image_id)
          )
        }) 
      })
      
      output$images_2 <- shiny::renderUI({
        indices <- seq_along(image_ids_r())
        indices <- indices[indices %% 4 == 2]
        
        image_ui <- purrr::map(indices, function(image_id) {
          image_box_ui(
            id = ns("image_box" %_% image_id)
          )
        }) 
      })
      
      output$images_3 <- shiny::renderUI({
        indices <- seq_along(image_ids_r())
        indices <- indices[indices %% 4 == 3]
        
        image_ui <- purrr::map(indices, function(image_id) {
          image_box_ui(
            id = ns("image_box" %_% image_id)
          )
        }) 
      })
      
      output$images_4 <- shiny::renderUI({
        indices <- seq_along(image_ids_r())
        indices <- indices[indices %% 4 == 0]
        
        image_ui <- purrr::map(indices, function(image_id) {
          image_box_ui(
            id = ns("image_box" %_% image_id)
          )
        }) 
      })
    }
  )
}