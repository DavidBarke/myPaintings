myPaintings
================

## Overview

This repository contains my contribution to the [Summer 2021 - Shopify
Developer Intern
Challenge](https://docs.google.com/document/d/1ZKRywXQLZWOqVOHC4JkF3LqdpO3Llpfk_CkZPR8bjak/edit).
The task was to build an image repository.

I decided to build a web application with the framework
[Shiny](https://shiny.rstudio.com/) for the R programming language. To
prove the capabilities of my application I crawled over 30,000 images of
historical paintings from the [Web Gallery of Art](https://www.wga.hu/).
Users can collect these paintings and trade them with other users.

## Features

  - **User Management**
    
      - Two statuses: `user` and `admin`
      - Views differentiated by status
      - `admin` may add new users and monitor database
      - `user` may collect and trade paintings

  - **Views**
    
    Users with status `user` have three different views on paintings:
    
      - *Browse* : View all paintings
      - *Collection* : View your own paintings
      - *Buy* : Buy paintings which were offered by other users
    
    Each view consists of a filter, further settings and the paintings
    display. The paintings display consists of one so-called `image_box`
    per painting. `image_box`es are loaded dynamically using custom
    infinite scroll.

  - **Filter**
    
    In each view all applicable paintings can be filtered by the
    following characteristics:
    
      - *Title*
      - *Painter*
      - *School*
      - *Type*
      - *Status* (offered / not offered)
    
    All characteristics but *Status* support three operations:
    
      - `=` : Select exactly one available choice.
      - `IN` : Select multiple available choices.
      - `REGEXP` : Select available choices matching a regular
        expression.
    
    An arbitrary number of filter conditions can be added. Each
    subsequent condition is restricted by the results of the preceding
    condition (e.g. after you selected a particular *Painter*, a
    subsequent *Title* condition will only show the title of paintings
    of this painter as its choices).

  - **Trade Paintings**
    
    In the *Collection* view:
    
      - Offer a painting for an arbitrary price
      - Change the price of an offered painting
      - Withdraw an offered painting
    
    In the *Buy* view:
    
      - Buy an offered painting

  - **Wallet**
    
    Each user with status `user` has an account balance. Selling a
    painting increases the balance whereas buying a painting decreases
    it. In the tab *Wallet* a user can view all past transactions and
    filter them by date.

## Implementation

### Packages

These are the most important R packages, which I used in my application:

  - [shiny](https://github.com/rstudio/shiny) : Reactive web framework
    for R
  - [DBI](https://github.com/r-dbi/DBI) : Database interface definition
    for communication between R and SQLite
  - [bs4Dash](https://github.com/RinteRface/bs4Dash) : Bootstrap 4
    dashboard using AdminLTE3
  - [htmltools](https://github.com/rstudio/htmltools) : Tools for HTML
    generation and output
  - [DT](https://github.com/rstudio/DT) : R interface to the jQuery
    plugin [DataTables](https://datatables.net/)
  - [shinyjs](https://github.com/daattali/shinyjs) : Extending shiny
    with custom JavaScript
  - [bcrypt](https://github.com/jeroen/bcrypt) : Password hashing

### Folder structure

  - `app.R` : Entry point of the app
  - `/data` :
      - `catalog.xlsx` : Catalog of the [Web Gallery of
        Art](https://www.wga.hu/)
      - `images.xlsx` : Extracted table of all paintings in
        `catalog.xlsx`
      - `painters.xlsx` : Extracted table of all painters in
        `catalog.xlsx`
      - `process_catalog.R` : Script for extraction of `images.xlsx` and
        `painters.xlsx` from `catalog.xlsx` and download of all
        paintings from the [Web Gallery of Art](https://www.wga.hu/)
      - `process_catalog_helpers.R` : Helper functions for
        `process_catalog.R`
  - `/db` :
      - `db.sqlite` : SQLite database.
      - `/interface` :
          - `init.R` : Call `db_init()` to initialize and populate
            `db.sqlite`
          - `*.R` : R functions, that provide an API to the database
  - `/img/wga` : All paintings from the [Web Gallery of
    Art](https://www.wga.hu/)
  - `/init/source_directory.R` : R function to source complete directory
  - `/md` : Markdown documents included in the app
  - `/modules` : Core of the functionality. Shiny modules and other R
    functions
  - `/renv` / `renv.lock` : Package management
  - `/www` :
      - `/css/styles.css` : Custom CSS
      - `/js/extend_shinyjs.js` : Custom JavaScript

## Shiny

The framework used in this application is probably not that well-known
outside the R ecosystem, so that I thought it’s best to provide a short
introduction.

Code is split across UI functions and server functions whereas usually
one UI function belongs to one server function. UI functions return
HTML-like R objects. Server functions process input provided by the user
and set outputs accordingly.

[Shiny](https://shiny.rstudio.com/) introduces an reactive programming
concept. Three types of reactive components are distinguished:

  - **Reactive Source**
    
    Reactive sources are read-only and usually represent input elements,
    which require the user to provide a value.

<!-- end list -->

``` r
# UI
textInput(
  inputId = "text",
  label = "I'm a text input"
)

# Server
# Use input$text to read the value of the textInput 
```

  - **Reactive Endpoint**
    
    Reactive endpoints are write-only. Their value is sent to the user.
    Common examples are plots or tables.

<!-- end list -->

``` r
# UI
plotOutput(outputId = "plot")

# Server
output$plot <- renderPlot({
  # code that generates a plot
})
```

  - **Reactive Conductor**
    
    Reactive conductors connect reactive sources with reactive
    endpoints. The following example puts it all together:

<!-- end list -->

``` r
# UI
tagList(
  numericInput(
    inputId = "n",
    label = "Number of bins:",
    value = 30
  ),
  plotOutput(outputId = "plot")
)

# Server
server <- function(input, output) {
  
  x <- rnorm(1000)
  
  # Reactive conductor
  bins_r <- reactive({
    # Read value of reactive source
    n <- input$n
    
    seq(min(x), max(x), length.out = n + 1)
  })
  
  output$plot <- renderPlot({
    # Read value of reactive conductor
    bins <- bins_r()
    
    # Histogram, which gets displayed to the user
    hist(x, breaks = bins)
  })
}
```
