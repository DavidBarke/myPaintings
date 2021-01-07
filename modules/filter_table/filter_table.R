filter_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      id = ns("table"),
      class = "filter-table"
    ),
    shiny::actionButton(
      inputId = ns("add_condition"),
      label = NULL,
      icon = shiny::icon("plus"),
      width = "100%"
    ),
    shiny::actionButton(
      inputId = ns("reset"),
      label = "Reset",
      width = "100%"
    ),
    shiny::actionButton(
      inputId = ns("apply"),
      label = "Apply",
      width = "100%"
    )
  )
}

filter_table_server <- function(id, 
                                .values,
                                tab = c("browse", "collection", "trade")
) {
  tab <- match.arg(tab)
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Number of currently active conditions
      n_conditions_rv <- shiny::reactiveVal(0)
      # Maximum number of conditions that has been reached during particular session
      max_conditions_rv <- shiny::reactiveVal(0)
      # Triggered when n_conditions_rv is set to 1
      first_condition_rv <- shiny::reactiveVal(0)
      
      return <- environment()
      # List of server returns
      return$row <- list()
      
      shiny::observeEvent(input$add_condition, {
        n_conditions_rv(n_conditions_rv() + 1)
        
        if (n_conditions_rv() == 1) first_condition_rv(first_condition_rv() + 1)
        
        if (n_conditions_rv() > max_conditions_rv()) {
          index <- n_conditions_rv()
          
          query_text_in_r <- if (index > 1) {
            return$row[[index - 1]]$query_text_out_r
          } else {
            query_text_in_start_r
          }
          
          query_params_in_r <- if (index > 1) {
            return$row[[index - 1]]$query_params_out_r
          } else {
            query_params_start_r
          }
          
          return$row[[index]] <- filter_table_condition_server(
            id = "filter_table_condition" %_% index,
            .values = .values,
            index = index,
            query_text_start_r = query_text_start_r,
            query_text_in_r = query_text_in_r,
            query_params_in_r = query_params_in_r,
            first_condition_r = shiny::reactive(first_condition_rv()),
            n_conditions_r = shiny::reactive(n_conditions_rv()),
            tab = tab
          )
          
          # Listen on clicking remove button in filter condition
          shiny::observeEvent(return$row[[index]]$remove_r(), {
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
      
      query_text_start_dict <- list(
        browse = "
          SELECT image.rowid AS image_id,
            CASE WHEN offered_images.price NOT NULL THEN 1 ELSE 0 END AS is_offered
          FROM user_image
            INNER JOIN user 
              ON user_image.user_id = user.rowid
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            LEFT JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        ",
        collection = "
          SELECT image.rowid AS image_id,
            1 AS is_offered 
          FROM user_image
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            LEFT JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        ",
        trade = "
          SELECT image.rowid AS image_id,
            1 AS is_offered
          FROM user_image
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            INNER JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        "
      )
      
      query_text_start_r <- shiny::reactive({
        query_text_start_dict[[tab]]
      })
      
      query_text_in_start_r <- switch(
        tab,
        "browse" = shiny::reactive(character()),
        "collection" = shiny::reactive("user_image.user_id = ?"),
        "trade" = shiny::reactive("user_image.user_id != ?")
      )
      
      query_text_result_r <- shiny::reactive({
        if (n_conditions_rv() == 0) {
          construct_query_text(
            query_text_start_r(),
            query_text_in_start_r()
          )
        } else {
          construct_query_text(
            query_text_start_r(),
            return$row[[n_conditions_rv()]]$query_text_out_r()
          )
        }
      })
      
      query_params_start_r <- shiny::reactive({
        switch(
          tab,
          "browse" = list(),
          "collection" = list(.values$user_rv()$user_id),
          "trade" = list(.values$user_rv()$user_id)
        )
      })
      
      query_params_result_r <- shiny::reactive({
        if (n_conditions_rv() == 0) return(query_params_start_r())
        
        return$row[[n_conditions_rv()]]$query_params_out_r()
      })
      
      # shiny::observeEvent(query_params_result_r(), {
      #   cat(query_text_result_r(), "\n")
      #   str(query_params_result_r())
      # })
      
      filter_query_r <- shiny::eventReactive(input$apply, {
        if (length(query_params_result_r())) {
          DBI::dbGetQuery(
            .values$db,
            query_text_result_r(),
            params = query_params_result_r()
          ) 
        } else {
          DBI::dbGetQuery(
            .values$db,
            query_text_result_r()
          ) 
        }
      }) %>%
        shiny::throttle(1000)
      
      # shiny::observeEvent(image_ids_r(), {
      #   print(dplyr::as_tibble(image_ids_r()))
      # })
      
      return_list <- list(
        image_ids_r = shiny::reactive(filter_query_r()$image_id),
        is_offered_r = shiny::reactive(filter_query_r()$is_offered)
      )
      
      return(return_list)
    }
  )
}