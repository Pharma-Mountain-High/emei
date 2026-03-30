#' @title Check that DBP is not higher than SBP in VS
#'
#' @description This check looks for non-missing diastolic BP is not higher than non-missing systolic BP
#' @param VS Vital Signs SDTM dataset with variables USUBJID,VISIT,VSDTC,VSTESTCD,VSSTRESN,VSSPID
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#' @export
#'
#' @importFrom dplyr %>% filter select rename
#'
#' @author Jh
#'
#' @examples
#'
#' vs <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = 1,
#'   VSSPID = c("1", "2", "1", "2"),
#'   VISIT = 1,
#'   VSDTC = c("2010-01-01", "2010-01-01", "2010-01-01", "2010-01-01"),
#'   VSTESTCD = c(
#'     "SYSBP", "SYSBP",
#'     "DIABP", "DIABP"
#'   ),
#'   VSSTRESN = c(80, 120, 100, 80)
#' )
#'
#' vs0 <- subset(vs, select = c(USUBJID, VSSPID, VSSTRESN))
#'
#' check_vs_sbp_lt_dbp(VS = vs)
#' check_vs_sbp_lt_dbp(VS = vs0)
#'
check_vs_sbp_lt_dbp <- function(VS) {
  req_always <- c("USUBJID", "VISIT", "VSDTC", "VSTESTCD", "VSSTRESN")
  if (VS %lacks_any% req_always) {
    fail(lacks_msg(VS, req_always))
  } else {
    has_vsspid <- "VSSPID" %in% names(VS)

    vs_cols <- if (has_vsspid) {
      c("USUBJID", "VISIT", "VSDTC", "VSTESTCD", "VSSTRESN", "VSSPID")
    } else {
      c("USUBJID", "VISIT", "VSDTC", "VSTESTCD", "VSSTRESN")
    }

    vs0 <- VS %>%
      select(all_of(vs_cols))

    sbp <- vs0 %>%
      filter(VSTESTCD == "SYSBP") %>%
      rename(VSDTC.SYSBP = VSDTC, SYSBP = VSSTRESN)

    dbp <- vs0 %>%
      filter(VSTESTCD == "DIABP") %>%
      rename(VSDTC.DIABP = VSDTC, DIABP = VSSTRESN)

    merge_by <- if (has_vsspid) {
      c("USUBJID", "VISIT", "VSSPID")
    } else {
      c("USUBJID", "VISIT")
    }

    mydf0 <- merge(sbp, dbp, by = merge_by)

    mydf <- mydf0 %>%
      filter(DIABP > SYSBP, SYSBP > 0, DIABP > 0) %>%
      select(USUBJID, VISIT, VSDTC.SYSBP, SYSBP, DIABP) %>%
      rename(VSDTC = VSDTC.SYSBP)

    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) > 0) {
      fail(paste0("VS has ", nrow(mydf), " records with Systolic BP < Diastolic BP. "), mydf)
    }
  }
}
