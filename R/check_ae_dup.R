#' @title Check for duplicate AE entries
#'
#' @description Identifies duplicated AE entries based on USUBJID, AETERM,
#'   AEDECOD, AESTDTC, AEENDTC, AEMODIFY (if present), AELAT (if present) and AETOXGR or AESEV
#'
#' @param AE AE SDTM dataset with variables USUBJID, AETERM, AEDECOD,
#'   AESTDTC, AEENDTC, and AETOXGR or AESEV
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_all filter select n
#' @importFrom tidyselect any_of
#'
#' @author 1
#'
#' @examples
#'
#' # FAIL: first two records are duplicates
#' AE <- data.frame(
#'   USUBJID = c(1), AESTDTC = c("2020-01-01", "2020-01-01", "2020-02-01", "2020-03-01"),
#'   AEENDTC = rep("2020-02-01", 4), AEDECOD = letters[c(1, 1:3)],
#'   AETERM = letters[c(1, 1:3)], AETOXGR = c(1, 1:3),
#'   AESPID = "FORMNAME-R:5/L:5XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_dup(AE)
#'
#' # PASS: no duplicates
#' AE2 <- data.frame(
#'   USUBJID = 1:3,
#'   AESTDTC = c("2020-01-01", "2020-02-01", "2020-03-01"),
#'   AEENDTC = c("2020-01-10", "2020-02-10", "2020-03-10"),
#'   AEDECOD = c("头痛", "恶心", "皮疹"),
#'   AETERM = c("头痛", "恶心", "皮疹"),
#'   AETOXGR = c(1, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_dup(AE2)
#'
check_ae_dup <- function(AE) {
  if (AE %lacks_any% c(
    "USUBJID", "AEDECOD", "AESTDTC", "AEENDTC",
    "AETERM"
  )) {
    fail(lacks_msg(AE, c(
      "USUBJID", "AEDECOD", "AESTDTC",
      "AEENDTC", "AETERM"
    )))
  } else if (AE %has_all% c("AETOXGR", "AESEV")) {
    fail("AE has both variables: AETOXGR and AESEV.")
  } else if (AE %lacks_all% c("AETOXGR", "AESEV")) {
    fail("AE is missing both the AETOXGR and AESEV variable.")
  } else {
    toxgr_var <- if (AE %has_all% "AETOXGR") {
      "AETOXGR"
    } else {
      "AESEV"
    }
    lat_var <- if (AE %has_all% "AELAT") {
      "AELAT"
    } else {
      NULL
    }
    if (AE %lacks_any% c("AEMODIFY")) {
      df <- AE %>%
        select(
          USUBJID, AETERM, AEDECOD, AESTDTC, any_of(c("AESEVDTC", "SEVDTC")),
          AEENDTC, any_of(c(toxgr_var, lat_var))
        ) %>%
        group_by_all() %>%
        filter(n() > 1)
    } else {
      df <- AE %>%
        select(
          USUBJID, AETERM, AEDECOD, AESTDTC, any_of(c("AESEVDTC", "SEVDTC")),
          AEENDTC, AEMODIFY, any_of(c(toxgr_var, lat_var))
        ) %>%
        group_by_all() %>%
        filter(n() > 1)
    }
    if (nrow(df) != 0) {
      fail("AE has duplicated entries. ", df)
    } else {
      pass()
    }
  }
}
