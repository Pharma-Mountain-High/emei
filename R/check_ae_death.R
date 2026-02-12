#' @title Check for Grade 5 AE death variable consistency
#'
#' @description Checks for grade 5 AEs not marked fatal (AEOUT), death not indicated (AESDTH), or no death date (AESDTHDTC)
#'
#' @param AE Adverse Event dataframe with variables USUBJID,AETOXGR,AEOUT,AEDTHDTC,AESDTH
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @author 1
#'
#' @importFrom dplyr %>% select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = 1:10,
#'   AETOXGR = c(1:5, 5, 5, 5, 5, 5),
#'   AESEQ = 1:10,
#'   AESDTH = c(rep(NA, 4), rep("是", 6)),
#'   AEOUT = c(rep(NA, 4), rep("死亡", 6)),
#'   AESPID = "FORMNAME-R:13/L:13XXXX"
#' )
#'
#' check_ae_death(AE)
#' check_ae_death(AE, preproc =ql_derive_seq)
#'
#'
#' AE$AESDTH[8] <- NA
#' AE$AEOUT[9] <- NA
#'
#' check_ae_death(AE)
#' check_ae_death(AE, preproc =ql_derive_seq)

check_ae_death <- function(AE, preproc = identity, ...) {
  ### Check that required variables exist and return a message if they don't.

  if (AE %lacks_any% c("USUBJID", "AETOXGR", "AEOUT",  "AESDTH")) {
    fail(lacks_msg(AE, c("USUBJID", "AETOXGR", "AEOUT",  "AESDTH")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)

    ### Subset AE to records with Grade 5 AE but have missing death date, or not marked fatal, or death not indicated
    ae5 <- subset(AE, AE$AETOXGR == "5" & (!grepl("死亡", AE$AEOUT) | is_sas_na(AE$AEOUT) | AE$AESDTH != "是" | is_sas_na(AE$AESDTH)), ) %>%
      select(any_of(c("USUBJID", "AETOXGR", "AEOUT",  "AESDTH", "AEGRPID", "AESPID","AESEQ")))
    rownames(ae5) <- NULL

    ### Print to report

    ### Return message if no such Grade 5 records
    if (nrow(ae5) == 0) {
      pass()

      ### Return subset dataframe if there are records with Grade 5 AE has missing death date, or not marked fatal, or death not indicated
    } else if (nrow(ae5) > 0) {
      fail(
        paste("Total number of records with grade 5 AEs and inconsistencies among AE death variables is ", nrow(ae5), ". ", sep = ""),
        ae5
      )
    }
  }
}
