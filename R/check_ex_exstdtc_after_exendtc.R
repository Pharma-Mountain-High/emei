#' @title Check that all exposure start dates are on or before exposure end dates
#'
#' @description This check identifies EXSTDTC values that are after EXENDTC values
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,EXTRT,EXSTDTC,EXENDTC
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
#' EX <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = 1:12,
#'   EXTRT = "SOME DRUG",
#'   EXSTDTC = c(
#'     "2017-01-01", "2017-01-03", "2017-01-01T14:26", "2017", "2017-02", "2017", "",
#'     "2017", "2017-01-01T14:26", "2017-01-01T14:26", "2017-01-01T14", "2017-01-01T14:26:02"
#'   ),
#'   EXENDTC = c(
#'     "2017-01-01", "2017-01-02", "2017-01-01T14:25", "2015", "2017-01", "2016-01-01", "2000",
#'     "2017-02", "2017-01-01", "2017-01", "2017-01-01T13", "2017-01-01T14:26:01"
#'   ),
#'   EXOCCUR = "是",
#'   VISIT = "CYCLE 1 DAY 1",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ex_exstdtc_after_exendtc(EX)
#'
#' EX$EXOCCUR <- NULL
#' EX$VISIT <- NULL
#' check_ex_exstdtc_after_exendtc(EX)
#'
#' EX$EXTRT <- NULL
#' check_ex_exstdtc_after_exendtc(EX)
#'
#' ## 7-digit vs 10-digit date comparisons use the same precision (month-level)
#' EX7 <- data.frame(
#'   USUBJID = c("A", "B"),
#'   EXTRT = "SOME DRUG",
#'   EXSTDTC = c("2017-01", "2017-02"),
#'   EXENDTC = c("2017-01-31", "2017-01-31"),
#'   stringsAsFactors = FALSE
#' )
#' check_ex_exstdtc_after_exendtc(EX7)
#'
#' ## Equal at shared precision (2017-01 vs 2017-01-31) -> pass
#' EX7_PASS <- data.frame(
#'   USUBJID = "C",
#'   EXTRT = "SOME DRUG",
#'   EXSTDTC = "2017-01",
#'   EXENDTC = "2017-01-31",
#'   stringsAsFactors = FALSE
#' )
#' check_ex_exstdtc_after_exendtc(EX7_PASS)
#'
check_ex_exstdtc_after_exendtc <- function(EX) {
  ### First check that required variables exist and return a message if they don't
  if (EX %lacks_any% c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) {
    fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")))
  } else {
    ## Get minimum length for when EXSTDTC and EXENDTC are different lengths
    EX$startdate <- substr(EX$EXSTDTC, 1, pmin(nchar(EX$EXSTDTC), nchar(EX$EXENDTC), na.rm = TRUE))
    EX$enddate <- substr(EX$EXENDTC, 1, pmin(nchar(EX$EXSTDTC), nchar(EX$EXENDTC), na.rm = TRUE))

    # We're not accounting for any time resolution smaller than minutes
    EX$startdate[nchar(EX$startdate) > 16] <- substr(EX$startdate[nchar(EX$startdate) > 16], 1, 16)
    EX$enddate[nchar(EX$enddate) > 16] <- substr(EX$enddate[nchar(EX$enddate) > 16], 1, 16)

    # Include VISIT and EXOCCUR in display if they exist in the data set
    myvars <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
    if (EX %has_any% "VISIT") {
      myvars <- c(myvars, "VISIT")
    }
    if (EX %has_any% "EXOCCUR") {
      myvars <- c(myvars, "EXOCCUR")
    }

    # Compare at shared precision only (year/month/day/hour/minute based on common substring length).
    mydf <- subset(EX, !is_sas_na(EX$EXSTDTC) & !is_sas_na(EX$EXENDTC) &
      !is_sas_na(EX$startdate) & !is_sas_na(EX$enddate) & (EX$startdate > EX$enddate),
    select = myvars
    )
    rownames(mydf) <- NULL

    # remove added vars
    EX$startdate <- NULL
    EX$enddate <- NULL

    ### Return message if no records with issue
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are issues with start date/time of IV
    } else if (nrow(mydf) > 0) {
      fail(paste("EX has ", nrow(mydf), " record(s) with EXSTDTC after EXENDTC. ", sep = ""), mydf)
    }
  }
}
