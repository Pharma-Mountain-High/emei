#' @title Check that all ECG datetimes are earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies EGDTC values that are
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param EG ECG Test Results SDTM dataset with variables USUBJID, VISITNUM, VISIT, EGDTC, EGSTAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if
#' the test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # PASS: no out-of-order datetime records (unscheduled visits excluded)
#' EG1 <- data.frame(
#'   USUBJID = rep(c(101, 102), each = 5),
#'   EGDTC = rep(c(
#'     "2017-01-01T08:25", "2017-01-05T09:25",
#'     "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
#'   ), 2),
#'   VISITNUM = rep(1:5, 2),
#'   VISIT = rep(c("筛选期", "C1/D1", "C2/D1", "C3/D1", "计划外访视"), 2),
#'   EGSTAT = "",
#'   stringsAsFactors = FALSE
#' )
#' check_eg_egdtc_visit_ordinal_error(EG1)
#'
#' # FAIL: datetime earlier than prior scheduled visit
#' EG2 <- EG1
#' EG2$EGDTC[EG2$USUBJID == 101 & EG2$VISIT == "C3/D1"] <- "2017-01-10T08:25"
#' EG2$EGDTC[EG2$USUBJID == 102 & EG2$VISIT == "C1/D1"] <- "2017-01-01T06:25"
#' check_eg_egdtc_visit_ordinal_error(EG2)
#'
#' # PASS: duplicated datetime alone is not flagged
#' EG3 <- data.frame(
#'   USUBJID = rep("101", 6),
#'   EGDTC = rep("2017-01-01T08:25", 6),
#'   VISITNUM = rep(1:2, 3),
#'   VISIT = rep("筛选期", 6),
#'   EGSTAT = "",
#'   stringsAsFactors = FALSE
#' )
#' check_eg_egdtc_visit_ordinal_error(EG3)
#'
check_eg_egdtc_visit_ordinal_error <- function(EG) {
  class(EG) <- "data.frame"
  vars <- c("USUBJID", "VISITNUM", "VISIT", "EGDTC", "EGSTAT")
  if (EG %lacks_any% vars) {
    fail(lacks_msg(EG, vars))
  } else if (length(unique(EG[["VISITNUM"]])) <= 1) {
    fail(msg = "VISITNUM exists but only a single value. ")
  } else {
    myout <- as.data.frame(dtc_dupl_early(
      dts = subset(
        EG,
        EG$EGSTAT != "未查" & !grepl("UNS|计划外", toupper(EG$VISIT)),
      ), vars = vars, groupby = vars[c(1)], dtc = vars[4],
      vars[1], vars[2], vars[3], vars[4]
    ))
    if (nrow(myout) > 0) {
      myout <- myout[!is.na(myout$check.flag), ]
      myout <- subset(myout, myout$check.flag != "Duplicated", )
    }
    if (nrow(myout) == 0) {
      pass()
    } else if (nrow(myout) > 0) {
      rownames(myout) <- NULL
      fail(paste("EG has ", nrow(myout), " records with Possible EGDTC data entry error. ",
        sep = ""
      ), myout)
    }
  }
}
