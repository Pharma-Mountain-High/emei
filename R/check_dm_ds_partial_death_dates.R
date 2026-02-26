#' @title Check for partial death dates in DM and DS
#'
#' @description This checks looks for partial death dates in DM and DS
#'
#' @param DM SDTM dataset with variable DTHDTC
#' @param DS Dispostion SDTM dataset with variables USUBJID,DSSCAT,DSSTDTC,DSDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @author Will Harris
#'
#' @examples
#'
#' DM <- data.frame(
#'   USUBJID = 1:3,
#'   DTHDTC = c("2017-01-01", "2017", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'   USUBJID = 1:4,
#'   DSSEQ = 11:14,
#'   DSSCAT = "STUDY DISCON",
#'   DSDECOD = "死亡",
#'   DSSTDTC = c("2017-01-01", "2017", "2017-01-02", "2016-10"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_dm_ds_partial_death_dates(DM, DS)
#' check_dm_ds_partial_death_dates(DM, DS, preproc = ql_derive_seq)
#'
#' DS$DSSTDTC <- NULL
#'
#' check_dm_ds_partial_death_dates(DM, DS)
check_dm_ds_partial_death_dates <- function(DM, DS, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (DS %lacks_any% c("USUBJID", "DSSCAT", "DSSTDTC", "DSDECOD","DSSEQ")) {
    fail(lacks_msg(DS, c("USUBJID", "DSSCAT", "DSSTDTC", "DSDECOD","DSSEQ")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    DS <- preproc(DS, ...)

    ### Find records with partial death dates (length <10) in DM and DS

    mydf1 <- subset(DS, DS$DSDECOD == "死亡" & !is_sas_na(DS$DSSTDTC) & nchar(DS$DSSTDTC) < 10 )%>%
      select(any_of(c("USUBJID", "DSSCAT", "DSDECOD", "DSSTDTC","SEQ")))
    mydf2 <- subset(DM, !is_sas_na(DM$DTHDTC) & nchar(DM$DTHDTC) < 10, ) %>%
      select(any_of(c("USUBJID",  "DTHDTC")))
    mydf <- merge(mydf1, mydf2, by = "USUBJID", all = TRUE)

    ### Print to report

    ### Return message if no records
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are records with partial dates
    } else if (nrow(mydf) > 0) {
      fail(
        paste("There are ", length(unique(mydf$USUBJID)),
          " patients with partial death dates. ",
          sep = ""
        ),
        mydf
      )
    }
  }
}
