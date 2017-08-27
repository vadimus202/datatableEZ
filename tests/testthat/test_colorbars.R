

test_that("format_databars() returns a datatable object",{
    cls <- c("datatables", "htmlwidget")
    dt <- datatable(iris)

    expect_is(format_databars(table = dt, columns = ~Sepal.Length), cls)
    expect_is(format_databars(table = dt, columns = "Sepal.Length"), cls)
    expect_is(format_databars(table = dt, columns = ~Sepal.Length, color = "red3"), cls)
    expect_is(format_databars(table = dt, columns = ~Sepal.Length+Petal.Length, color = 1:2), cls)

    expect_error(format_databars(table = dt, columns = Sepal.Length))
    expect_error(format_databars(table = dt, columns = ~SSepal.Length))
    expect_error(format_databars(table = dt, columns = 1))
    expect_error(format_databars(table = dt, columns = ~Sepal.Length, color = "Redd"))


})



