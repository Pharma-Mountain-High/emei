#' @title Check if there is a death date and AEOUT is death agreement
#'
#' @description This check looks for AE death dates if AEOUT is death and for the
#'    reverse, i.e if there is an AE death date, then AEOUT should have the value
#'    "死亡".
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC,
#'    AEDECOD, AESTDTC and AEOUT
#' @param DM SDTM dataset with variable DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author JH
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = 1:3,
#'   AEDECOD = 1:3,
#'   AESTDTC = 1:3,
#'   AEOUT = rep("死亡", 3),
#'   AESEQ = 11:13,
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:3,
#'   DTHDTC = c("2020-01-01", "2020-01-02", "2020-01-03"),
#'   stringsAsFactors = FALSE
#' )
#' # pass
#' check_dd_ae_aeout_dthdtc(AE, DM)
#'
#' # fail - 1 case (AEDTHDTC not populated but AEOUT == FATAL)
#' AE1 <- AE
#' DM[3, "DTHDTC"] <- NA
#' check_dd_ae_aeout_dthdtc(AE1, DM)
#' check_dd_ae_aeout_dthdtc(AE1, DM, preproc = ql_derive_seq)
#'
#' # fail - 2 case -- even though DTHDTC populated
#' AE2 <- AE
#' AE2[1, "AEOUT"] <- NA
#' check_dd_ae_aeout_dthdtc(AE2, DM)
#' check_dd_ae_aeout_dthdtc(AE2, DM, preproc = ql_derive_seq)
#'
#' # 2 cases
#'
#' AE[1, "AEOUT"] <- NA
#' check_dd_ae_aeout_dthdtc(AE, DM)
#' check_dd_ae_aeout_dthdtc(AE, DM, preproc = ql_derive_seq)
#'
#' # 2 cases
#' AE[1, "AEOUT"] <- "NOT RECOVERED/NOT RESOLVED"
#' check_dd_ae_aeout_dthdtc(AE, DM)
#' check_dd_ae_aeout_dthdtc(AE, DM, preproc = ql_derive_seq)
#'
#'
#' # critical variables are missing
#' DM$DTHDTC <- NULL
#' AE$USUBJID <- NULL
#' check_dd_ae_aeout_dthdtc(AE, DM)
#' check_dd_ae_aeout_dthdtc(AE, DM, preproc = ql_derive_seq)
#'
check_dd_ae_aeout_dthdtc <- function(AE, DM, preproc = identity, ...) {
  if (AE %lacks_any% c("USUBJID", "AEOUT", "AEDECOD", "AESTDTC")) {
    fail(lacks_msg(AE, c("USUBJID", "AEOUT", "AEDECOD", "AESTDTC")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(AE, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)
    dm1 <- DM %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    AE <- AE %>% left_join(dm1, by = c("USUBJID"))

    # check if AEOUT=='死亡' that there is a corresponding DTHDTC, death date
    # check non-missing AEDTHDTC with AEOUT=='死亡'
    mydf <- unique(subset(
      AE,
      (AE$AEOUT == "死亡" & is_sas_na(AE$DTHDTC)) |
        (!is_sas_na(AE$DTHDTC) & AE$AEOUT != "死亡" | is_sas_na(AE$AEOUT)),
    )) %>%
      select(any_of(c("USUBJID", "AEDECOD", "AESTDTC", "DTHDTC", "AEOUT", "SEQ")))


    if (nrow(mydf) > 0) {
      fail(paste0(nrow(mydf), " record(s) with a discrepant AE outcome and AE death date. "), mydf)
    } else if (nrow(mydf) == 0) {
      pass()
    }
  }
}
