# Tests for extended utility functions in utils.R
library(testthat)
library(Emei)
library(dplyr)

# Tests for %lacks_all% operator ----
test_that("%lacks_all% returns TRUE when dataframe lacks all specified variables", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %lacks_all% c("x", "y", "z")
  expect_true(result)
})

test_that("%lacks_all% returns FALSE when dataframe contains at least one specified variable", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %lacks_all% c("a", "x", "y")
  expect_false(result)
})

test_that("%lacks_all% returns FALSE when dataframe contains all specified variables", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %lacks_all% c("a", "b", "c")
  expect_false(result)
})

test_that("%lacks_all% handles empty variable vector", {
  df <- data.frame(a = 1, b = 2)
  result <- df %lacks_all% character(0)
  expect_true(result)
})

test_that("%lacks_all% works with single variable name", {
  df <- data.frame(a = 1, b = 2)
  result_missing <- df %lacks_all% "x"
  result_present <- df %lacks_all% "a"
  expect_true(result_missing)
  expect_false(result_present)
})

# Tests for %lacks_any% operator ----
test_that("%lacks_any% returns TRUE when dataframe lacks any specified variable", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %lacks_any% c("a", "x", "y")
  expect_true(result)
})

test_that("%lacks_any% returns FALSE when dataframe contains all specified variables", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %lacks_any% c("a", "b", "c")
  expect_false(result)
})

test_that("%lacks_any% handles empty variable vector", {
  df <- data.frame(a = 1, b = 2)
  result <- df %lacks_any% character(0)
  expect_false(result)
})

test_that("%lacks_any% works with single variable name", {
  df <- data.frame(a = 1, b = 2)
  result_missing <- df %lacks_any% "x"
  result_present <- df %lacks_any% "a"
  expect_true(result_missing)
  expect_false(result_present)
})

# Tests for lacks_msg() function ----
test_that("lacks_msg returns correct message when no variables are missing", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- lacks_msg(df, c("a", "b", "c"))
  expect_match(result, "is not missing any variable")
})

test_that("lacks_msg returns correct message when single variable is missing", {
  df <- data.frame(a = 1, b = 2)
  result <- lacks_msg(df, c("a", "b", "x"))
  expect_match(result, "is missing the variable: x")
})

test_that("lacks_msg returns correct message when multiple variables are missing", {
  df <- data.frame(a = 1, b = 2)
  result <- lacks_msg(df, c("a", "x", "y", "z"))
  expect_match(result, "is missing the variables:")
  expect_match(result, "x")
  expect_match(result, "y")
  expect_match(result, "z")
})

# Tests for %has_all% operator ----
test_that("%has_all% returns TRUE when dataframe contains all specified variables", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %has_all% c("a", "b", "c")
  expect_true(result)
})

test_that("%has_all% returns FALSE when dataframe lacks any specified variable", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %has_all% c("a", "b", "x")
  expect_false(result)
})

test_that("%has_all% handles empty variable vector", {
  df <- data.frame(a = 1, b = 2)
  result <- df %has_all% character(0)
  expect_true(result)
})

test_that("%has_all% works with single variable name", {
  df <- data.frame(a = 1, b = 2)
  result_present <- df %has_all% "a"
  result_missing <- df %has_all% "x"
  expect_true(result_present)
  expect_false(result_missing)
})

# Tests for %has_any% operator ----
test_that("%has_any% returns TRUE when dataframe contains at least one specified variable", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %has_any% c("a", "x", "y")
  expect_true(result)
})

test_that("%has_any% returns FALSE when dataframe lacks all specified variables", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- df %has_any% c("x", "y", "z")
  expect_false(result)
})

test_that("%has_any% handles empty variable vector", {
  df <- data.frame(a = 1, b = 2)
  result <- df %has_any% character(0)
  expect_false(result)
})

test_that("%has_any% works with single variable name", {
  df <- data.frame(a = 1, b = 2)
  result_present <- df %has_any% "a"
  result_missing <- df %has_any% "x"
  expect_true(result_present)
  expect_false(result_missing)
})

# Tests for dtc_dupl_early() function ----
test_that("dtc_dupl_early identifies duplicated dates", {
  dts <- data.frame(
    USUBJID = c("001", "001", "001"),
    VISIT = c("VISIT 1", "VISIT 2", "VISIT 3"),
    VISITNUM = c(1, 2, 3),
    EXSTDTC = c("2020-01-01", "2020-01-15", "2020-01-15"),
    EXTRT = c("DRUG A", "DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_true(any(result$check.flag == "Duplicated", na.rm = TRUE))
})

test_that("dtc_dupl_early identifies dates earlier than last visit", {
  dts <- data.frame(
    USUBJID = c("001", "001", "001"),
    VISIT = c("VISIT 1", "VISIT 2", "VISIT 3"),
    VISITNUM = c(1, 2, 3),
    EXSTDTC = c("2020-01-01", "2020-01-15", "2020-01-10"),
    EXTRT = c("DRUG A", "DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_true(any(result$check.flag == "Datetime earlier than last Visit", na.rm = TRUE))
})

test_that("dtc_dupl_early returns no flags for normal progression", {
  dts <- data.frame(
    USUBJID = c("001", "001", "001"),
    VISIT = c("VISIT 1", "VISIT 2", "VISIT 3"),
    VISITNUM = c(1, 2, 3),
    EXSTDTC = c("2020-01-01", "2020-01-15", "2020-02-01"),
    EXTRT = c("DRUG A", "DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_true(all(is.na(result$check.flag)))
})

test_that("dtc_dupl_early handles single record", {
  dts <- data.frame(
    USUBJID = "001",
    VISIT = "VISIT 1",
    VISITNUM = 1,
    EXSTDTC = "2020-01-01",
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$check.flag))
})

test_that("dtc_dupl_early handles two records", {
  dts <- data.frame(
    USUBJID = c("001", "001"),
    VISIT = c("VISIT 1", "VISIT 2"),
    VISITNUM = c(1, 2),
    EXSTDTC = c("2020-01-01", "2020-01-15"),
    EXTRT = c("DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_equal(nrow(result), 2)
})

test_that("dtc_dupl_early handles multiple subjects", {
  dts <- data.frame(
    USUBJID = c("001", "001", "002", "002"),
    VISIT = c("VISIT 1", "VISIT 2", "VISIT 1", "VISIT 2"),
    VISITNUM = c(1, 2, 1, 2),
    EXSTDTC = c("2020-01-01", "2020-01-15", "2020-01-05", "2020-01-20"),
    EXTRT = c("DRUG A", "DRUG A", "DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_equal(nrow(result), 4)
})

test_that("dtc_dupl_early excludes missing dates and unscheduled visits", {
  dts <- data.frame(
    USUBJID = c("001", "001", "001", "001"),
    VISIT = c("VISIT 1", "UNSCHEDULED", "VISIT 2", "VISIT 3"),
    VISITNUM = c(1, 1.5, 2, 3),
    EXSTDTC = c("2020-01-01", "2020-01-10", NA, "2020-02-01"),
    EXTRT = c("DRUG A", "DRUG A", "DRUG A", "DRUG A"),
    stringsAsFactors = FALSE
  )
  result <- dtc_dupl_early(
    dts,
    vars = c("USUBJID", "EXTRT", "VISITNUM", "VISIT", "EXSTDTC"),
    groupby = c("USUBJID", "EXTRT"),
    dtc = "EXSTDTC",
    "USUBJID", "VISITNUM", "VISIT", "EXSTDTC"
  )
  expect_false("UNSCHEDULED" %in% result$VISIT)
  expect_false(any(is.na(result$EXSTDTC)))
})

# Tests for convert_var_to_ascii() function ----
test_that("convert_var_to_ascii converts non-ASCII characters", {
  df <- data.frame(
    var = c("test", "teäst"),
    stringsAsFactors = FALSE
  )
  result <- convert_var_to_ascii(df, "var")
  expect_equal(result$var[1], "test")
  expect_equal(result$var[2], "test")
})

test_that("convert_var_to_ascii handles pure ASCII strings", {
  df <- data.frame(
    var = c("test1", "test2", "test3"),
    stringsAsFactors = FALSE
  )
  result <- convert_var_to_ascii(df, "var")
  expect_equal(result$var, c("test1", "test2", "test3"))
})

test_that("convert_var_to_ascii handles multiple special characters", {
  df <- data.frame(
    usubjid = 1:2,
    var = c("test", "teästõ"),
    stringsAsFactors = FALSE
  )
  result <- convert_var_to_ascii(df, "var")
  expect_equal(result$var[1], "test")
  expect_equal(result$var[2], "test")
})

test_that("convert_var_to_ascii handles empty strings", {
  df <- data.frame(
    var = c("test", ""),
    stringsAsFactors = FALSE
  )
  result <- convert_var_to_ascii(df, "var")
  expect_equal(result$var[2], "")
})

test_that("convert_var_to_ascii handles NA values", {
  df <- data.frame(
    var = c("test", NA, "another"),
    stringsAsFactors = FALSE
  )
  result <- convert_var_to_ascii(df, "var")
  expect_true(is.na(result$var[2]))
})

# Tests for truncate_var_strings() function ----
test_that("truncate_var_strings truncates strings exceeding length", {
  AE <- data.frame(
    USUBJID = 1:2,
    AETERM = c("SHORT", "THIS IS A SUPER LONG AE TERM, SO LONG IN FACT THAT ITS OVER 50 CHARACTERS."),
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_true(nchar(result$AETERM[1]) <= 50)
  expect_true(nchar(result$AETERM[2]) <= 50)
  expect_match(result$AETERM[2], "\\.\\.\\.$")
})

test_that("truncate_var_strings does not truncate short strings", {
  AE <- data.frame(
    USUBJID = 1:3,
    AETERM = c("SHORT", "MEDIUM LENGTH", "SMALL"),
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_equal(result$AETERM, AE$AETERM)
})

test_that("truncate_var_strings adds ellipsis after truncation", {
  AE <- data.frame(
    USUBJID = 1,
    AETERM = "THIS AE TERM IS WAY TOO LONG FOR A NICELY FORMATTED REPORT",
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_match(result$AETERM, "\\.\\.\\.$")
})

test_that("truncate_var_strings handles boundary values", {
  AE <- data.frame(
    USUBJID = 1:2,
    AETERM = c("EXACTLY FIFTY CHARACTERS IN THIS STRING HERE NOW", "49 CHARACTERS IN THIS STRING HERE NOW EXACTLY"),
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_true(nchar(result$AETERM[1]) <= 50)
  expect_true(nchar(result$AETERM[2]) <= 50)
})

test_that("truncate_var_strings handles empty strings", {
  AE <- data.frame(
    USUBJID = 1:2,
    AETERM = c("NORMAL TEXT", ""),
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_equal(result$AETERM[2], "")
})

test_that("truncate_var_strings handles NA values", {
  AE <- data.frame(
    USUBJID = 1:3,
    AETERM = c("NORMAL", NA, "ANOTHER"),
    stringsAsFactors = FALSE
  )
  result <- truncate_var_strings(AE, "AETERM", 50)
  expect_true(is.na(result$AETERM[2]))
})
