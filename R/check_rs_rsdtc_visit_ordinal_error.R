#' @title Check that all RS dates for INV Overall Response are duplicated or
#' earlier than last visit's (possible date entry error)
#'
#' @description This check identifies RSDTC values when RSEVAL == 'INVESTIGATOR'
#' and RSTESTCD == 'OVRLRESP' that are duplicated or earlier than last visit's.
#' Unscheduled and 'NOT DONE' visits are excluded.
#'
#' @param RS Response SDTM dataset with variables USUBJID, VISITNUM, VISIT,
#' RSDTC, RSTESTCD, RSEVAL, RSSTAT
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
#' # no cases
#' RS <- data.frame(
#'   USUBJID = 101:102,
#'   RSDTC = rep(c(
#'     "2017-01-01T08:25", "2017-01-05T09:25",
#'     "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
#'   ), 2),
#'   VISITNUM = rep(1:5, 2),
#'   VISIT = rep(c("Screening", "Cycle 1", "Cycle 2", "Cycle 3", "Follow-up"), 2),
#'   RSTESTCD = "OVRLRESP",
#'   RSEVAL = "INVESTIGATOR",
#'   RSSTAT = "",
#'   stringsAsFactors = FALSE
#' )
#' check_rs_rsdtc_visit_ordinal_error(RS)
#'
#' # adding cases with earler date
#' RS$RSDTC[RS$USUBJID == 101 & RS$VISIT == "Cycle 3"] <- "2017-01-02T08:25"
#' RS$RSDTC[RS$USUBJID == 102 & RS$VISIT == "Cycle 1"] <- "2017-01-01T06:25"
#' check_rs_rsdtc_visit_ordinal_error(RS)
#'
check_rs_rsdtc_visit_ordinal_error <- function(RS) {
  class(RS) <- "data.frame"
  vars <- c(
    "USUBJID", "VISITNUM", "VISIT", "RSDTC", "RSTESTCD",
    "RSEVAL", "RSSTAT"
  )
  if (RS %lacks_any% vars) {
    fail(lacks_msg(RS, vars))
  } else if (length(unique(RS[["VISITNUM"]])) <= 1) {
    fail("VISITNUM exists but only a single value. ")
  } else {
    subsetdf <- subset(RS, RS$RSTESTCD == "OVRLRESP" & RS$RSEVAL ==
      "研究者" & RS$RSSTAT != "未查" & !grepl(
      "UNS|计划外",
      toupper(RS$VISIT)
    ), )
    if (nrow(subsetdf) > 0) {
      mydf2 <- dtc_dupl_early(
        dts = subsetdf, vars = vars,
        groupby = vars[c(1)], dtc = vars[4], vars[1],
        vars[2], vars[3], vars[4]
      )
      myout <- mydf2[!is.na(mydf2$check.flag), ]
      myout <- subset(myout, myout$check.flag != "Duplicated", )
      if (nrow(myout) == 0) {
        pass()
      } else if (nrow(myout) > 0) {
        rownames(myout) <- NULL
        fail(paste("RS has ", nrow(myout), " records with Possible RSDTC data entry error. ",
          sep = ""
        ), myout)
      }
    } else {
      fail("No records when subset to overall responses by INV. ")
    }
  }
}
