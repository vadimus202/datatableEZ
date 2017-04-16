
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/vadimus202/datatableEZ.svg?branch=master)](https://travis-ci.org/vadimus202/datatableEZ) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/vadimus202/datatableEZ?branch=master&svg=true)](https://ci.appveyor.com/project/vadimus202/datatableEZ)

A Friendly Datatable
--------------------

`datatable_EZ()` is a wrapper around `DT::datatable()` with more convenient defaults. Also, most common elements of the `options()` argument are moved to direct arguments list for this function. This removes to necessity to pass on long options list (which is not well documented in the `DT` package documentation). It also makes these options available for auto-complete drop-downs in RStudio.

Read the **Interactive Data Tables** vignette for some examples.

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
