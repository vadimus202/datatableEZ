#' A friendly version of \code{DT::datatable()}
#'
#' This is a wrapper around \code{DT::datatable()} with more convenient
#' defaults. Also, most common elements of \code{options()} argument are moved
#' to direct arguments list for this function. This removes the necessity to pass
#' on a long options list (which is not fully documented in the DT package
#' documentation). It also exposes them auto-complete drop-downs in RStudio IDE.
#'
#' @param data a data object (either a matrix or a data frame)
#' @param caption the table caption; a character vector or a tag object
#' @param rownames \code{TRUE} (show row names) or \code{FALSE} (hide row names)
#'   or a character vector of row names; by default, the row names are not shown
#' @param colnames if missing, the column names of the data; otherwise it can be
#'   an unnamed character vector of names you want to show in the table header
#'   instead of the default data column names; alternatively, you can provide a
#'   \emph{named} numeric or character vector of the form \code{'newName1' = i1,
#'   'newName2' = i2} or \code{c('newName1' = 'oldName1', 'newName2' =
#'   'oldName2', ...)}, where \code{newName} is the new name you want to show in
#'   the table, and \code{i} or \code{oldName} is the index of the current
#'   column name
#' @param filter whether/where to use column filters; \code{none}: no filters;
#'   \code{bottom/top}: put column filters at the bottom/top of the table; range
#'   sliders are used to filter numeric/date/time columns, select lists are used
#'   for factor columns, and text input boxes are used for character columns; if
#'   you want more control over the styles of filters, you can provide a list to
#'   this argument of the form \code{list(position = 'top', clear = TRUE, plain
#'   = FALSE)}, where \code{clear} indicates whether you want the clear buttons
#'   in the input boxes, and \code{plain} means if you want to use Bootstrap
#'   form styles or plain text input styles for the text input boxes
#' @param class the CSS class(es) of the table, defaults to \code{"compact
#'   display"}; see \url{http://datatables.net/manual/styling/classes}
#' @param pageLength a number of records to be displayed per page when the table
#'   loads
#' @param lengthMenu a numeric vector of page length drop-down options
#' @param searchHighlight a logical; should search expression be highlighted?
#'   Defaults to \code{TRUE}
#' @param dom By default, the table has these DOM elements: the length-(l) menu,
#'   the filtering-(f) input box, the table-(t), the information-(i) summary,
#'   and the pagination-(p) control. You can choose to display a subset of these
#'   elements. See \url{https://datatables.net/reference/option/dom} for
#'   details.
#' @param columnDefs list of column definitions; see Examples.
#' @param order list of row sorting options; see examples.
#' @param ordering logical; should the table be sortable?
#' @param options a \code{list()} of all other options.
#' @param ... all other arguments passed to \code{DT::datatable()}
#'
#' @references See \url{http://rstudio.github.io/DT} for the full documentation.
#'
#' @return \code{DT::datatable} object
#'
#' @export
#' @importFrom DT datatable
#'
#' @seealso \code{\link[DT]{datatable}}, \code{\link{format_databars}}
#'
#' @example inst/examples/ex_datatable_EZ.R
#'
datatable_EZ <-
    function(
        data,
        # DT arguments
        colnames,
        filter = "none",
        caption = NULL,
        rownames = FALSE,
        class = "compact display",

        # DT options
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100, 500),
        searchHighlight = TRUE,
        dom="lftip",
        columnDefs = list(),
        ordering = TRUE,
        order = list(),
        options = list(),
        ...
    ){
        # update options
        if(is.null(options$dom))                options <- c(options, list(dom=dom))
        if(is.null(options$columnDefs))         options <- c(options, list(columnDefs=columnDefs))
        if(is.null(options$pageLength))         options <- c(options, list(pageLength=pageLength))
        if(is.null(options$lengthMenu))         options <- c(options, list(lengthMenu=lengthMenu))
        if(is.null(options$searchHighlight))    options <- c(options, list(searchHighlight=searchHighlight))

        if(is.null(options$order))
            options <- c(options, list(order=order))
        if(is.null(options$ordering) & length(order)==0)
            options <- c(options, list(ordering=ordering))

        # attach DT package
        if(!is_attached("DT")) attachNamespace("DT")

        # render datatable
        DT::datatable(data = data,
                      caption = caption,
                      class = class,
                      filter = filter,
                      rownames = rownames,
                      colnames = colnames,
                      options = options,
                      ...)
    }





#' Add Databars to the Datatable
#'
#' Applies conditional formatting databars to one or more columns in a
#' \code{DT::datatable()}.
#'
#' @param table an object from \code{DT::datatable} or
#'   \code{finTrendr::datatable_EZ} output
#' @param columns columns to be formatted (can be character or a formula of the
#'   form \code{~ V1 + V2}, which is equivalent to \code{c('V1', 'V2')})
#' @param color Background colors for each column (recycled if needed). One of:
#'   \itemize{
#'     \item One of \code{RColorBrewer::brewer.pal()} qualitative palettes
#'       ('Accent', 'Dark2','Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'),
#'       or a numeric index 1-8 of those names. The default is "Pastel1"
#'     \item valid R color name(s) - like \code{"red3"}
#'     \item RGB value(s) - like \code{"rgb(255, 0, 0)"}
#'     \item HEX value(s) - like \code{"#ff0000"}
#'   }
#' @param min_quantile Controls the minimum length of the databar. The row with
#'   the maximum value in a column will always have a full bar. The row with the
#'   minimum value will have a zero-length bar, if \code{min_quantile=0}.
#'   Default is 0.10.
#'
#' @return Formatted \code{DT::datatable} object
#' @export
#' @importFrom DT formatStyle styleColorBar
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices col2rgb
#'
#' @seealso \code{\link{datatable_EZ}}, \code{\link[DT]{datatable}},
#'   \code{\link[DT]{formatStyle}}, \code{\link[DT]{styleColorBar}}
#'
#' @examples
#' dt <- iris %>% datatable_EZ()
#'
#' # multiple columns specification
#' # same as c("Sepal.Length","Petal.Length")
#' dt %>% format_databars(~ Sepal.Length + Petal.Length)
#'
#' # Specify R color names
#' dt %>% format_databars(~ Sepal.Length, color=c("red3","grey75"))
#'
#' # Specify RColorBrewer qualitative palettes
#' dt %>% format_databars(~ Sepal.Length, color= "Pastel1") # by name
#' dt %>% format_databars(~ Sepal.Length, color= 2) # by index
#'
format_databars <-
    function(
        table,
        columns,
        color = "Pastel1",
        min_quantile = 0.10
    ){
        # convert formulas
        if (inherits(columns, "formula"))
            columns = all.vars(columns)

        # extract data from table
        df <- table$x$data[,columns, drop=FALSE]

        # define colors
        if(is_brew(color)){
            color <- get_brew(color)
        } else if(are_colors(color)) {
            color <- col2css(color)
        } else {
            stop("Invalid colors: ", paste(color, collapse = " "))
        }
        colors <- rep_len(color, length(columns))

        # loop through each column
        for(i in seq_along(columns)){
            # extract column
            x <- df[,i]
            # define min/max range
            rng <- get_range(x, min_quantile)

            table <-
                DT::formatStyle(
                    table = table,
                    columns = columns[i],
                    background = DT::styleColorBar(data=rng, color = colors[i]),
                    backgroundSize = '90% 75%', backgroundRepeat = 'no-repeat', backgroundPosition = 'right'
                )
        }

        return(table)
    }


are_colors <- function(colors) {
    vec <-
        sapply(colors, function(color) {
            tryCatch(is.matrix(col2rgb(color)),
                     error = function(e) FALSE)
        })

    all(vec)
}


brew_colors <- c('Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3')

is_brew <-
    function(color) length(color)==1 && (color %in% brew_colors || color %in% seq_along(brew_colors))

get_brew <-
    function(color){

        if(color %in% brew_colors){
            # match by name
            pal <- RColorBrewer::brewer.pal(8,color)
        } else if (color %in% seq_along(brew_colors)){
            # match by position
            pal <- RColorBrewer::brewer.pal(8, brew_colors[color])
        }
        return(pal)
    }

col2css <-
    function(color){
        css <- as.data.frame(col2rgb(color))
        css <- sapply(css, paste, collapse=",")
        css <- paste0("rgb(",css,")")
        return(css)
    }

get_range <-
    function(x, min_quantile){
        c(2*min(x, na.rm = T) - stats::quantile(x, min_quantile, na.rm = T),
          max(x, na.rm = T))
    }
