#' @title Check if patient with Death due to AE also has Death record in DS
#'
#' @description Flag if patient has a Death in AE (i.e. AE record with non-missing AE.AEDTHDTC)
#'              but no Death in DS (i.e. record where DS.DSDECOD=DEATH and
#'              DS.DSTERM contains 'DEATH' and does not contain 'PROGRESSIVE DISEASE' or 'DISEASE RELAPSE'
#'              (so we can pick up records where DSTERM in 'DEATH','DEATH DUE TO ...' and exclude
#'              'DEATH DUE TO PROGRESSIVE DISEASE', 'DEATH DUE TO DISEASE RELAPSE')
#'
#' @param AE Adverse Events SDTM dataset with USUBJID, AEDTHDTC, AESPID (optional)
#' @param DS Disposition SDTM dataset with USUBJID, DSDECOD, DSTERM
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @author Jh
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AEDTHDTC = c("2018-01-01", "2018-01-02", "2018-01-03", "2018-01-04", ""),
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'   USUBJID = c(1, 1, 2, 3, 3, 4),
#'   DSTERM = c(
#'     "死亡", "其他", "不良反应",
#'     "死亡由于疾病进展", "不良反应",
#'     "死亡由于ABC"
#'   ),
#'   DSDECOD = c("死亡", "不良反应", "死亡", "死亡", "其他", "死亡"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_dd_death_date(AE, DS)
#' check_dd_death_date(AE, DS, preproc = roche_derive_rave_row)
#'

check_dd_death_date <- function(AE, DS, preproc = identity, ...) {
  if (AE %lacks_any% c("USUBJID", "AEDTHDTC")) {
    fail(lacks_msg(AE, c("USUBJID", "AEDTHDTC")))
  } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSTERM")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSTERM")))
  } else {
    AE <- preproc(AE, ...)
    AE <- AE %>% select(any_of(c("USUBJID", "AEDTHDTC", "RAVE")))
    ae0 <- subset(AE, !is_sas_na(AE$AEDTHDTC), )
    ds0 <- subset(DS, (grepl("死亡", toupper(DS$DSDECOD)) &
      grepl("死亡", toupper(DS$DSTERM)) & !grepl(
      "疾病进展",
      toupper(DS$DSTERM)
    ) & !grepl("疾病复发", toupper(DS$DSTERM))), ) %>% select(any_of(c("USUBJID", "DSTERM", "DSDECOD")))
    mydfprep <- ae0 %>% left_join(unique(ds0), by = "USUBJID")
    mydf <- subset(mydfprep, is_sas_na(mydfprep$DSDECOD), )
    if (nrow(mydf) == 0) {
      pass()
    } else {
      fail(
        msg = paste(length(unique(mydf$USUBJID)), "patient(s) with a death date in AE but death not reflected properly in DS. "),
        data = unique(mydf)
      )
    }
  }
}
