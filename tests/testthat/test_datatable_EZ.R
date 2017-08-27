
test_that("datatable_EZ() returns a datatable object", {
    dt <-
        datatable_EZ(
            iris,
            # DOM elements (table only)
            dom="t",
            # column definitions
            columnDefs = list(list(width = '50px', targets = c(1, 3))),
            # Row Sorting
            order = list(list(0, 'desc'), list(1, 'asc'))
        )

    expect_is(dt,c("datatables", "htmlwidget"))

})

test_that("Column widths argument is working", {
    dt <-
        datatable_EZ(
            iris[, 1:3],
            columnDefs = list(list(width = '20px', targets = 0),
                              list(width = '50px', targets = 1),
                              list(width = '150px', targets = 2))
        )

    dt_col <-
        datatable_EZ(
            iris[, 1:3],
            col_widths = c(20, 50, 150)
        )

    expect_is(dt_col,c("datatables", "htmlwidget"))
    expect_equal(dt, dt_col)
    expect_error(datatable_EZ(iris, col_widths = 12))
    expect_error(datatable_EZ(iris, col_widths = LETTERS[1:5]))

})
