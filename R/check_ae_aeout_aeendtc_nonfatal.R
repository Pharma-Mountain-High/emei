#' @title Check for non-fatal AEs with inconsistent AEOUT and AEENDTC
#'
#' @description Check for inconsistency between AE outcome (AEOUT) and
#' AE end date (AEENDTC) for non-fatal AEs (based on AEOUT). AE flagged if AEENDTC
#' not populated when AEOUT is "RECOVERED/RESOLVED", "IMPROVED", "RECOVERED/RESOLVED WITH SEQUELAE", "RECOVERED/RESOLVED TO BASELINE".
#' AE also flagged if AEENDTC is populated when AEOUT is
#' "UNKNOWN", "NOT IMPROVED".
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AESTDTC, AEENDTC, AEOUT
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = 1:10,
#'   AETERM = "AE",
#'   AESTDTC = c(
#'     NA, "NA", "2015-03-09", "2010-10", "2017-01-20", "1999-11-02",
#'     "", NA, "2017-08-20", "2014-12-01"
#'   ),
#'   AEENDTC = c(
#'     NA, "NA", "2015-03-12", "2010-10", "2017-01-22", "1999-11-07",
#'     "", NA, "2017-09-01", "2015-01-01"
#'   ),
#'   AEOUT = c(
#'     "", "", "", "", "未好转",
#'     "好转", "死亡", "痊愈/恢复", "不详", "不详"
#'   ),
#'   AESEQ = 11:20,
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aeout_aeendtc_nonfatal(AE)
#' check_ae_aeout_aeendtc_nonfatal(AE, preproc = ql_derive_seq)
#'
#' AE$AEENDTC <- NULL
#' check_ae_aeout_aeendtc_nonfatal(AE)
#'
#' AE$AEOUT <- NULL
#' check_ae_aeout_aeendtc_nonfatal(AE)
check_ae_aeout_aeendtc_nonfatal <- function(AE, preproc = identity, ...) {
  if (AE %lacks_any% c("USUBJID", "AESTDTC", "AETERM", "AEENDTC", "AEOUT")) {
    fail(lacks_msg(AE, c("USUBJID", "AESTDTC", "AETERM", "AEENDTC", "AEOUT")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)

    df <- AE %>%
      filter((is_sas_na(AEENDTC) & AEOUT %in% c("痊愈/恢复", "好转", "痊愈/恢复伴有后遗症", "痊愈/恢复至基线")) |
        (!is_sas_na(AEENDTC) & AEOUT %in% c("不详", "未好转"))) %>%
      select(any_of(c("USUBJID", "AETERM", "AESTDTC", "AEENDTC", "AEOUT", "SEQ")))

    if (nrow(df) > 0) {
      fail(paste0(nrow(df), " non-fatal AE(s) with inconsistent AEENDTC and AEOUT found. "), df)
    } else {
      pass()
    }
  }
}
