image_box_link_ui <- function(id, title) {
  ns <- shiny::NS(id)
  
  as.character(
    shiny::actionLink(
      inputId = ns("link"),
      label = title,
      onclick = glue::glue(
        'Shiny.setInputValue("{inputId}", this.id + Math.random())',
        inputId = ns("link")
      )
    )
  )
}

image_box_link_server <- function(id, .values, image_id, title) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      image_r <- shiny::reactive({
        DBI::dbGetQuery(
          .values$db,
          construct_query_text(
            query_images("browse"),
            "image.rowid = ?"
          ),
          params = list(image_id)
        )
      })
      
      shiny::observeEvent(input$link, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          image_box_ui(
            id = ns("image_box"),
            image = image_r(),
            type = "browse"
          )
        ))
      })
    }
  )
}