#' @title Check DM with DTHDTC value but AESDTH not "Y"
#'
#' @description This check looks for DM entries with an DTHDTC (death date)
#'   value and  AESDTH of AE not equal to "是"
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AESDTH, AEDECOD, AETERM, and AESTDTC
#' @param DM SDTM dataset with variable DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author 1
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = c(1:7),
#'   AEDECOD = c(letters[1:5], "", NA),
#'   AETERM = letters[1:7],
#'   AESDTH = "是",
#'   AESTDTC = c(1:7),
#'   AESEQ = c(11:17),
#'   AESPID = "FORMNAME-R:5/L:5XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' DM <- data.frame(
#'   USUBJID = c(1:7),
#'   DTHDTC = "2020-01-02"
#' )
#' # expect pass
#' check_ae_aesdth_dthdtc(AE, DM)
#' check_ae_aesdth_dthdtc(AE, DM, preproc = ql_derive_seq)
#'
#' # expect fail
#' AE1 <- AE
#' AE1$AESDTH[3] <- "否"
#' check_ae_aesdth_dthdtc(AE1, DM)
#' check_ae_aesdth_dthdtc(AE1, DM, preproc = ql_derive_seq)
#'
#' # expect fail with AESDTH = NA
#' AE2 <- AE
#' AE2$AESDTH[4] <- NA
#' check_ae_aesdth_dthdtc(AE2, DM)
#' check_ae_aesdth_dthdtc(AE2, DM, preproc = ql_derive_seq)
#'
#' # non-required variable missing
#' AE2$AESEQ <- NULL
#' check_ae_aesdth_dthdtc(AE2, DM)
#' check_ae_aesdth_dthdtc(AE2, preproc = ql_derive_seq)
#'
#' # required variable missing
#' AE2$AESDTH <- NULL
#' check_ae_aesdth_dthdtc(AE2, DM)
#' check_ae_aesdth_dthdtc(AE2, preproc = ql_derive_seq)
#' DM$DTHDTC <- NULL
#' check_ae_aesdth_dthdtc(AE1, DM)
#' check_ae_aesdth_dthdtc(AE1, DM, preproc = ql_derive_seq)
check_ae_aesdth_dthdtc <- function(AE, DM, preproc = identity, ...) {
  # Checks if required variables are present
  if (AE %lacks_any% c(
    "USUBJID", "AETERM", "AESDTH", "AEDECOD", "AESTDTC",
    "AESEQ"
  )) {
    fail(lacks_msg(AE, c(
      "USUBJID", "AETERM", "AESDTH", "AEDECOD", "AESTDTC",
      "AESEQ"
    )))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)
    ae1 <- AE %>%
      select(any_of(c(
        "USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AESDTH",
        "SEQ"
      )))
    dm1 <- DM %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    ae_dm <- ae1 %>% left_join(dm1, by = c("USUBJID"))
    # Rows where DTHDTC is not NA
    has_dthdtc <- !(is_sas_na(ae_dm[["DTHDTC"]]))



    # Rows where AESDTH != "是", with expanded logic for NA values
    no_aesdth <- !(ae_dm[["AESDTH"]] == "是") | (is_sas_na(ae_dm[["AESDTH"]]))

    # Subsets AE to select variables and rows where
    # AESDTH != "是" and AEDTHDTC has a value
    df <- ae_dm %>%
      filter(has_dthdtc, no_aesdth)

    rownames(df) <- NULL

    # Outputs a resulting message depending on whether there are instances
    # where AESDTH != "是" and AEDTHDTC having a value
    if (nrow(df) == 0) {
      pass()
    } else {
      fail(paste0("AE has ", nrow(df), " record(s) with AESDTH not equal to '是'
                  where DTHDTC of DM has a value. "), df)
    }
  }
}
