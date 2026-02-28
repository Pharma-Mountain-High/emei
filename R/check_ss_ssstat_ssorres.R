#' @title Check for non-missing SSORRES when SSSTAT is '未查'
#'
#' @description This check is for studies with LTFU mapped to the SS domain,
#' check that if '未查' (Unable to Contact), then there should not be
#' a response (SSORRES)
#'
#' @param SS Long-Term Survival Follow-Up SDTM dataset with variables USUBJID,
#' VISIT, SSSTAT, SSDTC, SSORRES, SSSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select arrange
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' SS <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = c(rep(1, 6), rep(2, 6)),
#'   SSSTRESC = c("存活", "死亡", "存活", "", "", "未知", "存活", "死亡", "存活", "", "", "未知"),
#'   SSORRES = c("存活", "死亡", "存活", "", "", "未知", "存活", "死亡", "存活", "", "", "未知"),
#'   VISIT = rep(paste0("生存随访", 1:6), 2),
#'   SSSTAT = rep(c("", "未查"), 6),
#'   SSDTC = rep("2016-01-01", 12),
#'   SSSEQ = rep(1:6, 2),
#'   SSSPID = sprintf("%02d", rep(1:6, 2)),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ss_ssstat_ssorres(SS)
#'
#' SS$SSORRES[2] <- NA
#' check_ss_ssstat_ssorres(SS)
#'
#' SS$SSSPID <- "01"
#' check_ss_ssstat_ssorres(SS, preproc = ql_derive_seq)
#'
#' SS$SSORRES[6] <- NA
#' SS$SSORRES[8] <- ""
#' SS$SSORRES[12] <- NA
#' check_ss_ssstat_ssorres(SS)
#'
#' SS$SSORRES <- NULL
#' check_ss_ssstat_ssorres(SS)
#'
check_ss_ssstat_ssorres <- function(SS, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (SS %lacks_any% c("USUBJID", "SSORRES", "VISIT", "SSSTAT", "SSDTC")) {
    fail(lacks_msg(SS, c("USUBJID", "SSORRES", "VISIT", "SSSTAT", "SSDTC")))
  } else {
    # Apply company specific preprocessing function
    SS <- preproc(SS, ...)

    mydf <- SS %>%
      ## Subset to required and optional variables
      select(any_of(c("USUBJID", "VISIT", "SSDTC", "SSORRES", "SSSTAT", "SEQ", "SSSPID"))) %>%
      # potential future refinement - exclude 'U' and 'UNKNOWN' result
      # use SSORRES, variable used for LSTALVDT
      filter(SSSTAT == "未查" & !is_sas_na(SSORRES)) %>%
      arrange(USUBJID, SSDTC, VISIT) %>%
      unique()

    rownames(mydf) <- NULL

    ### Print to report

    ### Return message if no records with issue in SS
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are issues in SS with NOT DONE but results
    } else if (nrow(mydf) > 0) {
      fail(
        paste0("SS (LTFU) has ", nrow(mydf), " record(s) for ", length(unique(mydf$USUBJID)),
          " unique patient(s) with non-missing SSORRES when SSSTAT='未查'. ",
          sep = " "
        ),
        mydf
      )
    }
  }
}
