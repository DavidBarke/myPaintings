transaction_display_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      title = shiny::uiOutput(outputId = ns("title")),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      DT::dataTableOutput(
        outputId = ns("transaction_table")
      )
    )
  )
}

transaction_display_content_server <- function(id, .values, display_args, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$title <- shiny::renderUI({
        paste(
          "Transactions (from", 
          options$date_start_r(), 
          "until", 
          options$date_end_r(),
          ")"
        )
      })
      
      taken_image_ids_rv <- shiny::reactiveVal(character())
      
      output$transaction_table <- DT::renderDataTable({
        tbl <- options$transactions_r()
        
        tbl$Title <- purrr::map2(tbl$image_id, tbl$Title, function(image_id, title) {
          if (!image_id %in% taken_image_ids_rv()) {
            taken_image_ids_rv(c(
              shiny::isolate(taken_image_ids_rv()),
              image_id
            ))
            
            image_box_link_server(
              id = "image_box_link" %_% image_id,
              .values = .values,
              image_id = image_id,
              title = title
            )
          }
          
          image_box_link_ui(
            id = ns("image_box_link" %_% image_id),
            title = title
          )
        })
        
        tbl$is_sold <- tbl$Seller == .values$user_rvs$name
        
        DT::datatable(
          tbl,
          options = list(
            columnDefs = list(
              list(
                # Hide image_id and is_sold
                visible = FALSE,
                targets = c(1, length(tbl))
              )
            )
          ),
          escape = FALSE
        ) %>%
          DT::formatCurrency(
            columns = "Price"
          ) %>%
          DT::formatStyle(
            columns = "Price",
            valueColumns = "is_sold",
            color = DT::styleEqual(
              levels = c(TRUE, FALSE),
              values = c("green", "red")
            )
          )
      })
    }
  )
}