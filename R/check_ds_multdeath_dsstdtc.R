#' @title Check DS with multiple death records with death dates, where death dates do not match
#'
#' @description This check looks for patients in DS who have multiple records indicating
#'   death, with non-missing mismatching death dates in DSSTDTC.
#'
#' @param DS Disposition SDTMv dataset with variables USUBJID, DSDECOD, and DSSTDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @importFrom dplyr %>% filter select group_by n
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @author JH
#'
#' @examples
#'
#' DS_error1 <- data.frame(
#'   STUDYID = rep(1, 6),
#'   USUBJID = c(1, 1, 1, 2, 1, 1),
#'   DSSEQ = c(1, 2, 3, 4, 5, 6),
#'   DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
#'   DSSCAT = LETTERS[1:6],
#'   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02", "2016-01-01"),
#'   stringsAsFactors = FALSE
#' )
#'
#' DS_error2 <- data.frame(
#'   STUDYID = rep(1, 6),
#'   USUBJID = c(1, 1, 1, 2, 1, 1),
#'   DSSEQ = c(1, 2, 3, 4, 5, 6),
#'   DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
#'   DSSCAT = LETTERS[1:6],
#'   DSSTDTC = c("", "2016-01", "", "", "2016-01-01", "2016-01-01"),
#'   stringsAsFactors = FALSE
#' )
#'
#' DS_noerror <- data.frame(
#'   STUDYID = rep(1, 6),
#'   USUBJID = c(1, 1, 1, 2, 1, 1),
#'   DSSEQ = c(1, 2, 3, 4, 5, 6),
#'   DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
#'   DSSCAT = LETTERS[1:6],
#'   DSSTDTC = c("", "2016-01-01", "", "", "2016-01-01", "2016-01-01"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ds_multdeath_dsstdtc(DS_error1)
#' check_ds_multdeath_dsstdtc(DS_error2)
#' check_ds_multdeath_dsstdtc(DS_noerror)
#' check_ds_multdeath_dsstdtc(DS_error1, preproc = ql_derive_seq)
check_ds_multdeath_dsstdtc <- function(DS, preproc = identity, ...) {
  if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSTDTC")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSTDTC")))
  } else {
    # Apply company specific preprocessing function
    DS <- preproc(DS, ...)

    # Get all records with a death date
    death_dates <- DS %>%
      filter(DSDECOD == "死亡" & !is_sas_na(DSSTDTC)) %>%
      select(any_of(c("USUBJID", "DSSCAT", "DSDECOD", "DSSTDTC", "SEQ")))

    # Get all patients where death dates don't match
    df <- death_dates %>%
      group_by(USUBJID) %>%
      filter(n() >= 2, length(unique(DSSTDTC)) > 1)

    if (nrow(df) == 0) {
      pass()
    } else {
      fail("DS has multiple non-missing death dates in DSSTDTC that do not match. ", df)
    }
  }
}
