#' @title Check for AE outcome (AEOUT) of '死亡' with non-missing resolution
#' date that is not equal to the death date
#'
#' @description This check looks for AEs with outcome of '死亡' but AE
#' resolution date is not equal to DM death date.
#' Note that these datapoints are not collected the same way for all trials -
#' some trials leave AEENDTC missing if it was unresolved at death date. Confirm
#' within your team before querying this issue.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM,
#' AEDTHDTC, AEENDTC, AEOUT
#' @param DM SDTM dataset with variable DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author JH
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = 1:10,
#'   DOMAIN = "AE",
#'   AESEQ = 101:110,
#'   AEENDTC = c(NA, "NA", rep("2015-03-12", 4), NA, "2020-01-01", NA, ""),
#'   AEOUT = c("", "", "", "死亡", "痊愈/恢复", rep("死亡", 5)),
#'   AETERM = 1:10,
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:10,
#'   DTHDTC = c(NA, "NA", rep("2015-03-12", 4), NA, NA, "2020-01-01", ""),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_aeout_aeendtc_dm_dthdtc(AE, DM)
#' check_ae_aeout_aeendtc_dm_dthdtc(AE, DM, preproc = ql_derive_seq)
#'
#' AE$AESPID <- NULL
#' check_ae_aeout_aeendtc_dm_dthdtc(AE, DM)
#'
#' DM$DTHDTC <- NULL
#' AE$AEOUT <- NULL
#' check_ae_aeout_aeendtc_dm_dthdtc(AE, DM)
check_ae_aeout_aeendtc_dm_dthdtc <- function(AE, DM, preproc = identity, ...) {
  if (AE %lacks_any% c("USUBJID", "AETERM", "AEENDTC", "AESEQ", "AEOUT")) {
    fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEENDTC", "AESEQ", "AEOUT")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)
    ae1 <- AE %>%
      select(any_of(c("USUBJID", "AETERM", "AEENDTC", "AESEQ", "AEOUT")))
    dm1 <- DM %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    ae_dm <- ae1 %>% left_join(dm1, by = c("USUBJID"))

    mydf <- ae_dm %>%
      filter(AEOUT == "死亡") %>%
      filter(AEENDTC != DTHDTC | is_sas_na(AEENDTC))
    rownames(mydf) <- NULL

    ## Add note
    mydf$NOTE <- "**QUERY ONLY IF TEAM AGREES**"

    if (nrow(mydf) > 0) {
      fail(
        paste(nrow(mydf), " AE(s) with AEOUT = '死亡' but DTHDTC of DM and AEENDTC inconsistent. ", sep = ""),
        mydf
      )
    } else {
      pass() # pass if AEOUT is 死亡 but DTHDTC of DM and AEENDTC inconsistent
    }
  }
}
