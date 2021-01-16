filter_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::actionButton(
      inputId = ns("add_condition"),
      label = NULL,
      icon = shiny::icon("plus")
    ),
    shiny::uiOutput(
      outputId = ns("reset"),
      inline = TRUE
    ),
    htmltools::div(
      id = ns("table"),
      class = "filter-table"
    ),
    htmltools::hr(),
    shiny::actionButton(
      inputId = ns("apply"),
      label = "Apply"
    )
  )
}

filter_table_server <- function(id, 
                                .values,
                                type = c("browse", "collection", "buy")
) {
  type <- match.arg(type)
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$reset <- shiny::renderUI({
        if (n_conditions_rv()) {
          htmltools::tagList(
            shiny::actionButton(
              inputId = ns("reset"),
              label = "Reset"
            ),
            htmltools::hr()
          )
        }
      })
      
      # Number of currently active conditions
      n_conditions_rv <- shiny::reactiveVal(0)
      # Maximum number of conditions that has been reached during particular session
      max_conditions_rv <- shiny::reactiveVal(0)
      # Triggered when n_conditions_rv is set to 1
      first_condition_rv <- shiny::reactiveVal(0)
      
      ret <- environment()
      # List of server returns
      ret$condition <- list()
      
      shiny::observeEvent(input$add_condition, {
        print("add_condition")
        n_conditions_rv(n_conditions_rv() + 1)
        
        if (n_conditions_rv() == 1) first_condition_rv(first_condition_rv() + 1)
        
        if (n_conditions_rv() > max_conditions_rv()) {
          index <- n_conditions_rv()
          
          image_ids_in_r <- if (index == 1) {
            image_ids_start_r
          } else {
            ret$condition[[index - 1]]$image_ids_r
          }
          
          ret$condition[[index]] <- filter_table_condition_server(
            id = "filter_table_condition" %_% index,
            .values = .values,
            index = index,
            image_ids_in_r = image_ids_in_r,
            first_condition_r = shiny::reactive(first_condition_rv()),
            n_conditions_r = shiny::reactive(n_conditions_rv()),
            type = type
          )
          
          # Listen on clicking remove button in filter condition
          shiny::observeEvent(ret$condition[[index]]$remove_r(), {
            n_conditions_rv(n_conditions_rv() - 1)
          }, ignoreInit = TRUE)
          
          max_conditions_rv(index)
        }
        
        shiny::insertUI(
          selector = paste0("#", ns("table")),
          where = "beforeEnd",
          ui = filter_table_condition_ui(
            id = ns("filter_table_condition" %_% n_conditions_rv())
          )
        )
      })
      
      shiny::observeEvent(input$reset, {
        n_conditions_rv(0)
        
        shiny::removeUI(
          selector = paste0("#", ns("table"), " .filter-table-row"),
          multiple = TRUE
        )
      })
      
      query_image_ids_start_condition_r <- switch(
        type,
        "browse" = shiny::reactive(character()),
        "collection" = shiny::reactive("user_image.user_id = ?"),
        "buy" = shiny::reactive("user_image.user_id != ?")
      )
      
      query_params_start_r <- shiny::reactive({
        switch(
          type,
          "browse" = NULL,
          "collection" = list(.values$user_rvs$user_id),
          "buy" = list(.values$user_rvs$user_id)
        )
      })
      
      query_text_start_r <- shiny::reactive({
        browse_collection_text <- "
        SELECT 
          image.rowid AS image_id 
        FROM user_image
          INNER JOIN image ON user_image.image_id = image.rowid
          INNER JOIN user ON user_image.user_id = user.rowid
        "
        
        switch(
          type,
          "browse" = browse_collection_text,
          "collection" = browse_collection_text,
          "buy" = "
          SELECT
            image.rowid AS image_id
          FROM offered_images
            LEFT JOIN user_image ON offered_images.image_id = user_image.image_id
            LEFT JOIN image ON offered_images.image_id = image.rowid
            LEFT JOIN user ON user_image.user_id = user.rowid
          "
        )
      })
      
      image_ids_start_r <- shiny::reactive({
        query <- construct_query_text(
          query_text_start_r(),
          query_image_ids_start_condition_r()
        )
        
        DBI::dbGetQuery(
          .values$db,
          query,
          params = query_params_start_r()
        )$image_id
      })
      
      apply_filter_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(input$apply, {
        apply_filter_rv(apply_filter_rv() + 1)
      })
      
      shiny::observeEvent(.values$update$db_user_rv(), {
        apply_filter_rv(apply_filter_rv() + 1)
      })
      
      apply_filter_r <- shiny::reactive({
        apply_filter_rv()
      }) %>% shiny::throttle(1000)
      
      image_ids_r <- shiny::eventReactive(apply_filter_r(), {
        if (n_conditions_rv() == 0) {
          image_ids_start_r()
        } else {
          ret$condition[[n_conditions_rv()]]$image_ids_r()
        }
      })
      
      query_all_images_r <- shiny::reactive({
        query_images(type)
      })
      
      images_r <- shiny::reactive({
        tbl <- DBI::dbGetQuery(
          .values$db,
          query_all_images_r()
        ) 
        
        tbl %>% filter(image_id %in% image_ids_r())
      })
      
      return_list <- list(
        images_r = images_r,
        image_ids_r = image_ids_r
      )
      
      return(return_list)
    }
  )
}