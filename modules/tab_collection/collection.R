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
      
      image_tbl_r <- shiny::reactive({
        db_get_table(.values$db, "image")
      })
      
      output$images <- shiny::renderUI({
        image_ui <- purrr::pmap(image_tbl_r(), function(rowid, path, title, user_id) {
          if (!as.integer(rowid) %in% called_rvs$image_box_server) {
            called_rvs$image_box_server <- c(called_rvs$image_box_server, rowid)
            
            image_box_server(
              id = "image_box" %_% rowid,
              .values = .values,
              img = list(
                src = path,
                title = title
              )
            )
          }
          
          image_box_ui(
            id = ns("image_box" %_% rowid)
          )
        }) 
        
        shiny::fluidRow(
          image_ui
        )
      })
      
      image_box_server(
        id = "mona_lisa",
        img = list(
          src = "./img/mona_lisa.png",
          title = "Mona Lisa"
        )
      )
    }
  )
}