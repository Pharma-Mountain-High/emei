#' @title Check if death date is the same in DM and DS domains
#'
#' @description This check compares death date in DM DTHDTC with death date in
#'    DS DSSTDTC. It is expected that they are the same.
#'
#' @param DM  SDTM dataset with variables USUBJID and DTHDTC
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSSTDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#' @importFrom dplyr full_join
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author JH
#'
#' @examples
#'
#' DM <- data.frame(
#'   STUDYID = rep(1, 3),
#'   USUBJID = 1:3,
#'   DTHDTC = c("2020-01-01", "2020-01-02", "2020-01-03")
#' )
#'
#' DS <- data.frame(
#'   STUDYID = rep(1, 3),
#'   USUBJID = 1:3,
#'   DSDECOD = rep("死亡", 3),
#'   DSSTDTC = c("2020-01-01", "2020-01-02", "2020-01-03"),
#'   DSSEQ = 11:13,
#'   stringsAsFactors = FALSE
#' )
#'
#' # no case
#' check_dd_dm_dthdtc_ds_dsstdtc(DM, DS)
#'
#' # 1 case
#' DS[3, "DSSTDTC"] <- "2000-01-01"
#' check_dd_dm_dthdtc_ds_dsstdtc(DM, DS, preproc = ql_derive_seq)
#'
#' # check for non existence of vars
#' DS$DSDECOD <- NULL
#' DS$DSSTDTC <- NULL
#' check_dd_dm_dthdtc_ds_dsstdtc(DM, DS)
#'
check_dd_dm_dthdtc_ds_dsstdtc <- function(DM, DS, preproc = identity, ...) {
  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSTDTC")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSTDTC")))
  } else {
    # Apply company specific preprocessing function

    DS <- preproc(DS, ...)

    # From DM keep rows where the death date is populated
    dm0 <- subset(DM, !is_sas_na(DM$DTHDTC), ) %>%
      select(any_of(c("USUBJID", "DTHDTC")))

    # From DS take DEATH records where DEATH date is populated
    ds0 <- subset(DS, !is_sas_na(DS$DSSTDTC) &
      (regexpr("死亡", DS$DSDECOD, ignore.case = TRUE) != -1), ) %>%
      select(any_of(c("USUBJID", "DSSTDTC", "DSSEQ")))

    # Merge DS and DM and if death dates in both are different then output in mydf
    mydf0 <- left_join(ds0, dm0, by = "USUBJID", suffix = c(".DS", ".DM"))
    mydf <- unique(subset(mydf0, !(mydf0$DTHDTC == mydf0$DSSTDTC), )) %>%
      select(USUBJID, DSSTDTC, DTHDTC, everything())


    ds11 <- as.data.frame(unique(mydf$USUBJID))
    names(ds11) <- "USUBJID"
    rownames(ds11) <- NULL

    n3 <- ""

    # declare number of patients
    n3 <- paste("There are ", nrow(ds11), " patients with a death date different in DS and DM. ", sep = "")
    if (nrow(ds11) > 0) {
      fail(n3, mydf)
    } else if (nrow(ds11) == 0) {
      pass()
    }
  }
}
