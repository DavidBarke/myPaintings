myPaintings
================

<link href="readme/adminlte.min.css" rel="stylesheet">

<script src="https://kit.fontawesome.com/3ded47969a.js" crossorigin="anonymous"></script>

<script src="readme/jquery.min.js"></script>

<script src="readme/bootstrap.bundle.min.js"></script>

<script src="readme/adminlte.min.js"></script>

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

  - **Painting Views**

  - **Filter Paintings**

  - **Trade Paintings**

## Implementation

### Initialisation

``` r
source("./init/source_directory.R")
source_directory("./modules")

set.seed(1)

image_box_ui(
  id = "image",
  image = list(
    owner = "David Barkemeyer",
    painter = "Emanuel Gottlieb Leutze",
    title = "Washington Crossing the Delaware",
    date = "1851",
    location = "Metropolitan Museum of Art, New York",
    type = "historical",
    school = "American",
    url = "https://www.wga.hu/html/l/leutze/delaware.html",
    path = "readme/delaware.jpg",
    is_offered = FALSE
  ),
  type = "browse"
)
```

<div class="image-box">
<div>
<div class="card card-primary card-tabs" id="image-image_tabset_box">
<div class="card-header p-0 pt-1">
<ul class="nav nav-pills shiny-tab-input" id="image-image_tabset" data-tabsetid="8669">
<li class="pt-2 px-3">
<h3 class="card-title">Washington Crossing the Delaware</h3>
</li>
<li class="nav-item">
<a href="#tab-8669-1" data-toggle="tab" data-value="image" class="nav-link active">
<i class=" fa fa-image fa-fw" role="presentation" aria-label=" icon"></i>
Image
</a>
</li>
<li class="nav-item">
<a href="#tab-8669-2" data-toggle="tab" data-value="info" class="nav-link">
<i class=" fa fa-info-circle fa-fw" role="presentation" aria-label=" icon"></i>
Info
</a>
</li>
<li class="ml-auto">
<div class="card-tools float-right">
<button type="button" class="btn btn-tool btn-sm btn-primary" data-card-widget="maximize">
<i class="fa fa-expand" role="presentation" aria-label="expand icon"></i>
</button>
</div>
</li>
</ul>
</div>
<div class="card-body">
<div class="tab-content" data-tabsetid="8669">
<div class="tab-pane active" data-value="image" data-icon-class="fa fa-image" id="tab-8669-1">
<img src="readme/delaware.jpg" width="100%" height="auto"/>
</div>
<div class="tab-pane" data-value="info" data-icon-class="fa fa-info-circle" id="tab-8669-2">
<div class="row">
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-user" role="presentation" aria-label="user icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">Owner</span>
<span class="info-box-number">David Barkemeyer</span>
</div>
</div>
</div>
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-paint-brush" role="presentation" aria-label="paint-brush icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">Painter</span>
<span class="info-box-number">Emanuel Gottlieb Leutze</span>
</div>
</div>
</div>
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-hourglass" role="presentation" aria-label="hourglass icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">Created in</span>
<span class="info-box-number">1851</span>
</div>
</div>
</div>
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-landmark" role="presentation" aria-label="landmark icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">Exhibited at</span>
<span class="info-box-number">Metropolitan Museum of Art, New York</span>
</div>
</div>
</div>
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-school" role="presentation" aria-label="school icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">School</span>
<span class="info-box-number">American</span>
</div>
</div>
</div>
<div class="col-sm-12">
<!--SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<!--/SHINY.SINGLETON[f7ec2b4fe61927287e001df87fb23174d7807398]-->
<div class="info-box">
<span class="info-box-icon">
<i class="fa fa-copyright" role="presentation" aria-label="copyright icon"></i>
</span>
<div class="info-box-content">
<span class="info-box-text">Source</span>
<span class="info-box-number">
<a href="https://www.wga.hu/html/l/leutze/delaware.html" target="_blank">https://www.wga.hu/html/l/leutze/delaware.html</a>
</span>
</div>
</div>
</div>
</div>
</div>
</div>
</div>
</div>
<script type="application/json" data-for="image-image_tabset_box">{"status":"primary","solidHeader":true,"collapsible":false,"closable":false,"maximizable":true,"gradient":false}</script>
</div>
</div>
