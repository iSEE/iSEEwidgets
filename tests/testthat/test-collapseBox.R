# library(testthat); source("tests/testthat/test-collapseBox.R")

test_that("collapseBox works", {
    out <- collapseBox(
        id = "ID",
        title = "TITLE",
        shiny::p())
    expect_s3_class(out, "shiny.tag.list")
})
