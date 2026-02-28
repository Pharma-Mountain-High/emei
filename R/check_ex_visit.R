#' @title Check for missing EX.VISIT
#'
#' @description This check looks for missing EX.VISIT values when EX.EXOCCUR=是
#' (or EX.EXOCCUR does not exist)
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,EXTRT,EXSTDTC,VISIT, and optional variable EXOCCUR
#'
#' @author 1
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' EX <- data.frame(
#'   USUBJID = 1:3,
#'   EXTRT = 1:3,
#'   EXSTDTC = 1:3,
#'   EXOCCUR = "是",
#'   VISIT = NA
#' )
#'
#' check_ex_visit(EX)
#'
#' EX$EXOCCUR <- NULL
#'
#' check_ex_visit(EX)
#'
#' EX$VISIT <- NULL
#'
#' check_ex_visit(EX) #
#'
#' ## PASS example
#' EX_PASS <- data.frame(
#'   USUBJID = 1:2,
#'   EXTRT = c("A", "B"),
#'   EXSTDTC = c("2020-01-01", "2020-01-02"),
#'   EXOCCUR = c("是", "是"),
#'   VISIT = c("C1D1", "C2D1")
#' )
#' check_ex_visit(EX_PASS)
#'
check_ex_visit <- function(EX) {
  ### First check that required variables exist and return a message if they don't
  if (EX %lacks_any% c("USUBJID", "EXTRT", "EXSTDTC", "VISIT")) {
    fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXSTDTC", "VISIT")))
  } else {
    if (EX %has_all% c("EXOCCUR")) {
      ### Subset EX to only records with missing VISIT
      mydf <- subset(EX, EX$EXOCCUR == "是" & (is_sas_na(EX$VISIT)), c("USUBJID", "EXTRT", "EXSTDTC", "EXOCCUR", "VISIT"))
      rownames(mydf) <- NULL
    } else {
      mydf <- subset(EX, is_sas_na(EX$VISIT), c("USUBJID", "EXTRT", "EXSTDTC", "VISIT"))
      rownames(mydf) <- NULL
    }

    ### Print to report

    ### Return message if no records with missing VISIT
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are records with missing VISIT
    } else if (nrow(mydf) > 0) {
      fail(
        paste0("Total number of records is ", nrow(mydf), ". "),
        mydf
      )
    }
  }
}
