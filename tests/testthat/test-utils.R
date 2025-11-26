test_that("pass returns TRUE", {
  expect_true(pass())
})
test_that("fail returns FALSE with msg attr", {
  x <- fail("oops")
  expect_false(x)
  expect_equal(attr(x, "msg"), "oops")
})
test_that("is_sas_na works", {
  x <- c("a", NA, "NA", ".", "")
  expect_equal(unname(is_sas_na(x)), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})
test_that("impute_day01 adds day", {
  expect_equal(impute_day01(c("2020-05", "2020-05-02")), c("2020-05-01", "2020-05-02"))
})
test_that("missing_month detects ---", {
  expect_true(missing_month("2020---20"))
  expect_false(missing_month("2020-01-20"))
})
