#' @title Check for drug overdose
#'
#' @description This check looks for drug overdose
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AEDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' AE_clean <- data.frame(
#'   USUBJID = c("001", "002", "003"),
#'   AETERM = c("头痛", "恶心", "皮疹"),
#'   AEDECOD = c("Headache", "Nausea", "Rash"),
#'   AESTDTC = c("2024-01-01", "2024-01-05", "2024-01-10"),
#'   AEENDTC = c("2024-01-03", "2024-01-06", "2024-01-12"),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_drug_overdose(AE_clean)
#' # 期望：pass()，无问题
#'
#' AE_overdose <- data.frame(
#'   USUBJID = c("001", "002", "003"),
#'   AETERM = c("头痛", "药物过量", "恶心"),
#'   AEDECOD = c("Headache", "Drug overdose", "Nausea"),
#'   AESTDTC = c("2024-01-01", "2024-02-10", "2024-01-05"),
#'   AEENDTC = c("2024-01-03", "2024-02-11", "2024-01-06"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_drug_overdose(AE_overdose)
#' # 期望：fail()，返回 USUBJID=002 的记录
#'
check_ae_drug_overdose <- function(AE, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AETERM", "AEDECOD")) {
    fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEDECOD")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)

    ### Subset AE to only records with drug overdose
    AE %>%
      select(any_of(c("USUBJID", "SEQ", "SPID", "AESTDTC", "AETERM", "AEENDTC")))
    mydf <- subset(
      AE,
      (grepl("药物", toupper(AE$AETERM)) & grepl("过量", toupper(AE$AETERM)))
    )

    rownames(mydf) <- NULL

    ### Print to report
    ### Return message if no records withdrug overdose
    if (nrow(mydf) == 0) {
      pass()
      ### Return subset dataframe if there are records with drug overdose
    } else if (nrow(mydf) > 0) {
      fail(paste("AE has ", nrow(mydf), " record(s) with drug overdose. ",
        sep = ""
      ), mydf)
    }
  }
}
