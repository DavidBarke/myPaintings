bodyless_card <- function (..., 
                           title = NULL, 
                           status = NULL, 
                           solidHeader = FALSE, 
                           background = NULL, 
                           width = 6, 
                           height = NULL,  
                           icon = NULL, 
                           gradient = FALSE, 
                           boxToolSize = "sm", 
                           elevation = NULL, 
                           headerBorder = TRUE, 
                           label = NULL, 
                           dropdownMenu = NULL, 
                           id = NULL
) {
  
  footer <- NULL
  collapsible <- FALSE
  collapsed <- FALSE
  closable <- FALSE
  maximizable <- FALSE
  gradient <- FALSE
  sidebar <- NULL
  
  bs4Dash:::validateBoxProps(
    title = title, 
    label = label, 
    sidebar = sidebar, 
    dropdownMenu = dropdownMenu, 
    status = status, 
    gradient = gradient, 
    collapsible = collapsible, 
    collapsed = collapsed, 
    solidHeader = solidHeader, 
    background = background, 
    elevation = elevation, 
    width = width
  )
  
  prop_title <- if (!is.null(title)) {
    if (inherits(title, "list")) {
      unlist(bs4Dash:::dropNulls(lapply(title, function(e) {
        if (inherits(e, "shiny.tag.list") || inherits(e, "shiny.tag")) {
          as.character(e)
        }
      })))
    } else {
      as.character(title)
    }
  } else {
    title
  }
  
  props <- bs4Dash:::dropNulls(
    list(
      title = prop_title, 
      status = status, 
      solidHeader = solidHeader, 
      background = background, 
      width = width, 
      height = height, 
      gradient = gradient
    )
  )
  
  cardCl <- bs4Dash:::setBoxClass(
    status, 
    solidHeader, 
    collapsible, 
    collapsed, 
    elevation, 
    gradient, 
    background, 
    sidebar
  )
  
  style <- bs4Dash:::setBoxStyle(
    height, 
    sidebar
  )
  
  cardToolTag <- NULL
  
  if (
    collapsible || 
    closable || 
    maximizable || 
    !is.null(dropdownMenu) || 
    !is.null(sidebar) || 
    !is.null(label)
  ) {
    cardToolTag <- htmltools::tags$div(class = "card-tools float-right")
  }
  
  boxTools <- bs4Dash:::createBoxTools(
    collapsible, 
    collapsed, 
    closable, 
    maximizable, 
    sidebar, 
    dropdownMenu, 
    boxToolSize, 
    status, 
    background, 
    solidHeader
  )
  
  cardToolTag <- htmltools::tagAppendChildren(
    cardToolTag, label, boxTools
  )
  
  if (
    is.null(title) && 
    (
      maximizable || 
      closable || 
      collapsible || 
      !is.null(dropdownMenu) || 
      !is.null(sidebar) || 
      !is.null(label)
    )
  ) {
    title <- "."
  }
  
  
  headerTag <- htmltools::tags$div(
    class = if (headerBorder) "card-header" else "card-header border-0", 
    icon, 
    htmltools::tags$h3(class = "card-title", title)
  )
  
  headerTag <- htmltools::tagAppendChild(headerTag, cardToolTag)
  
  cardTag <- htmltools::tags$div(
    class = cardCl, 
    id = id
  )
  
  cardTag <- htmltools::tagAppendChild(
    cardTag, 
    headerTag
  )
  
  htmltools::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    cardTag, 
    htmltools::tags$script(
      type = "application/json", 
      `data-for` = id, 
      jsonlite::toJSON(x = props, auto_unbox = TRUE, json_verbatim = TRUE)
    )
  )
}