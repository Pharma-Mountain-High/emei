#' @title Check that all LB dates are duplicated or earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies LBDTC values that are duplicated or
#' earlier than last visit's. Records with LBSTAT == '未查' and unscheduled
#' visits (VISIT with the string "计划外") are excluded.
#'
#' @param LB SDTM dataset with variables USUBJID, VISITNUM, VISIT, LBDTC, LBSTAT.
#'   LBTESTCD/LBCAT are used for grouping when available.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # no case
#' LB1 <- data.frame(
#'   USUBJID = c(rep("101", 5), rep("102", 5)),
#'   LBCAT = "Hematology",
#'   LBDTC = rep(c(
#'     "2017-01-01T08:25",
#'     "2017-01-05T09:25",
#'     "2017-01-15T10:25",
#'     "2017-01-20T08:25",
#'     "2017-01-25T08:25"
#'   ), 2),
#'   VISITNUM = rep(1:5, 2),
#'   VISIT = rep(c(
#'     "C1/D1",
#'     "C2/D1",
#'     "C3/D1",
#'     "计划外访视",
#'     "C4/D1"
#'   ), 2),
#'   LBSTAT = c(rep("", 9), "未查"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_lb_lbdtc_visit_ordinal_error(LB1)
#'
#' LB2 <- LB1
#' LB2$LBCAT <- "病毒学检查"
#' LB3 <- rbind(LB1, LB2)
#' check_lb_lbdtc_visit_ordinal_error(LB3)
#'
#' # adding cases with earlier date
#' LB3$LBDTC[LB3$USUBJID == 101 & LB3$VISIT == "C3/D1"] <- "2016-01-10T08:25"
#' LB3$LBDTC[LB3$USUBJID == 102 & LB3$VISIT == "C2/D1"] <- "2016-01-01T06:25"
#' check_lb_lbdtc_visit_ordinal_error(LB = LB3)
#'
#' # adding cases with duplicated date
#' LB3$LBDTC[LB3$USUBJID == 102 & LB3$VISIT == "C3/D1"] <- "2017-01-15T10:25"
#' LB3 <- LB3[order(LB3$USUBJID, LB3$VISITNUM, LB3$LBDTC), ]
#' check_lb_lbdtc_visit_ordinal_error(LB = LB3)
#'
#' # check if all NOT DONE
#' LB4 <- LB3
#' LB4$LBSTAT <- "未查"
#' check_lb_lbdtc_visit_ordinal_error(LB = LB4)
#'
#' # check dropping a required variable
#' LB4$LBSTAT <- NULL
#' check_lb_lbdtc_visit_ordinal_error(LB = LB4)
#'
check_lb_lbdtc_visit_ordinal_error <- function(LB) {
  class(LB) <- "data.frame"
  vars <- c(
    "USUBJID", "VISITNUM", "VISIT", "LBDTC",
    names(LB)[names(LB) %in% c("LBTESTCD", "LBCAT")], "LBSTAT"
  )
  if (LB %lacks_any% vars) {
    fail(lacks_msg(LB, vars))
  } else if (length(unique(LB[["VISITNUM"]])) <= 1) {
    fail(msg = "VISITNUM exists but only a single value. ")
  } else {
    subsetdf <- subset(LB, LB$LBSTAT != "未查" & !grepl(
      "计划外",
      toupper(LB$VISIT)
    ))
    if (nrow(subsetdf) > 0) {
      groupby_vars <- c("USUBJID", names(LB)[names(LB) %in% c("LBTESTCD", "LBCAT")])
      mydf2 <- dtc_dupl_early(
        dts = subsetdf, vars = vars,
        groupby = groupby_vars, dtc = "LBDTC", vars[1],
        vars[2], vars[3], "LBDTC"
      )
      myout <- mydf2[!is.na(mydf2$check.flag), ]
      myout <- myout[order(
        myout$USUBJID, myout$VISITNUM,
        myout$LBDTC
      ), ]
      if (nrow(myout) == 0) {
        pass()
      } else if (nrow(myout) > 0) {
        rownames(myout) <- NULL
        fail(paste("LB has ", nrow(myout), " record(s) with Possible LBDTC data entry error. ",
          sep = ""
        ), myout)
      }
    } else {
      fail("No lab records when subset to exclude 未查.")
    }
  }
}
