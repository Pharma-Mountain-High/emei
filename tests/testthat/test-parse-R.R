# tests/testthat/test-parse-R.R
test_that("R/ 下所有 R 脚本可解析", {
  r_dir <- normalizePath(testthat::test_path("..", "..", "R"), mustWork = FALSE)
  testthat::skip_if_not(dir.exists(r_dir))
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(r_files), 0)
  invisible(lapply(r_files, function(f) {
    expect_error(parse(file = f), NA, info = f)
  }))
})
