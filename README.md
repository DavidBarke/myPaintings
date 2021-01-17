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
    condition (e.g. after you selected a particular painter, subsequent
    conditions will only allow you to select paintings of this painter).

  - **Trade Paintings**
    
    In the *Collection* view:
    
      - Offer a painting for an arbitrary price
      - Change the price of an offered painting
      - Withdraw an offered painting
    
    In the *Buy* view:
    
      - Buy an offered painting

## Implementation

### Initialisation