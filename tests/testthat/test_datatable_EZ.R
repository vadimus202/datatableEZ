
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

