library(DT)

# original DT::datatable
datatable(iris)
# no rownumbers by default
datatable_EZ(iris)

# individual option arguments
datatable_EZ(
    iris,
    # DOM elements (table only)
    dom="t",
    # column definitions
    columnDefs = list(list(width = '50px', targets = c(1, 3))),
    # Row Sorting
    order = list(list(0, 'desc'), list(1, 'asc'))
)

# specify column widths in pixels
datatable_EZ(
    iris,
    col_widths = c(50, 50, 300, 50, 300)
)

# disable sorting and searching by user ("static version")
datatable_EZ(iris, ordering = FALSE, dom = "t")


# Pagination
datatable_EZ(mtcars, pageLength = 3, lengthMenu = c(3,5,10))

