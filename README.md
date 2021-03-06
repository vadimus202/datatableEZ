
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/vadimus202/datatableEZ.svg?branch=master)](https://travis-ci.org/vadimus202/datatableEZ) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/vadimus202/datatableEZ?branch=master&svg=true)](https://ci.appveyor.com/project/vadimus202/datatableEZ)

A Friendly Datatable
--------------------

The `DT` package is a wrapper of the JavaScript library 'DataTables', and is a powerful way of rendering HTML tables from R dataframes using JavaScript, usually via Markdown or Shiny. This package is mostly a wrapper around `DT::datatable()` with more convenient defaults and simplified syntax.

The most common elements of the options() list argument are moved to direct arguments of the `datatable_EZ()` function. This avoids the need to include a long list of options and provides additional documentation and examples of their usage.

Read the **Interactive Data Tables** vignette for some examples.

Installation
------------

To install the development version:

``` r
devtools::install_github(repo = "datatableEZ", 
                         username = "vadimus202", 
                         build_vignettes = TRUE)
```

Common Usage
------------

``` r
library(datatableEZ)
```

### Original datatable

-   Includes row numbers by default
-   Less compact
-   Options are not well documented

``` r
datatable(iris)
```

### Easy Datatable

-   By default:
    -   More compact
    -   No row numbers
    -   Search highlights
-   Direct access to options through named arguments:
    -   DOM elements
    -   Column definitions (width, alignment, etc.)
    -   Rows sorting
    -   Page length

``` r
datatable_EZ(
    iris,
    dom="t",
    columnDefs = list(list(width = '50px', targets = c(1, 3))),
    order = list(list(0, 'desc'), list(1, 'asc'))
)
```

### Font Control

the DT package doesn't set the default font family. So the browser will use its default font to display the datatable. That's why you find the font displays differently in different browsers.

``` r
datatable_EZ(iris, font_family = "Courier New", font_size = 9)
```

### Specify Column Widths in Pixels

``` r
datatable_EZ(
    iris,
    col_widths = c(50, 50, 300, 50, 300)
)
```

### Pagination

-   Number of rows per page
-   Page length drop-down options

``` r
datatable_EZ(iris, pageLength = 3, lengthMenu = c(3,5,10))
```

### Static Version

Disables sorting and searching by the user.

``` r
datatable_EZ(iris, ordering = FALSE, dom = "t")
```

### Conditional Databars

A simple way to add conditional formatting databars to one or more columns.

``` r
iris %>% 
    datatable_EZ() %>% 
    format_databars(~Sepal.Length + Sepal.Width + Petal.Length)
```
