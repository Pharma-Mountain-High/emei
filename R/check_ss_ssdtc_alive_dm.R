#' @title Check SS records with alive result and non-missing SSDTC later than DM death date
#'
#' @description This check looks for non-missing SS.SSDTC when SS.SSTESTCD='SURVSTAT' and
#'              SS.SSORRES='存活', then compares SS.SSDTC with DM.DTHDTC
#'              by subject and reports records where SS.SSDTC > DM.DTHDTC.
#'
#' @param SS Subject Status SDTM dataset with variables USUBJID, SSDTC, SSORRES, SSTESTCD, VISIT, SSSPID (optional), SEQ (optional)
#' @param DM Demographics SDTM dataset with variables USUBJID, DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr left_join filter mutate %>% select
#' @importFrom tidyselect any_of
#'
#' @author 1
#'
#' @examples
#'
#' SS <- data.frame(
#'   USUBJID = 1:5,
#'   SSSEQ = 1:5,
#'   SSDTC = "2020-01-02",
#'   SSTESTCD = "SURVSTAT",
#'   SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
#'   VISIT = "生存随访"
#' )
#'
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = "2020-01-03"
#' )
#'
#' check_ss_ssdtc_alive_dm(SS, DM)
#' check_ss_ssdtc_alive_dm(SS, DM, preproc = ql_derive_seq)
#'
#' SS1 <- data.frame(
#'   USUBJID = 1:5,
#'   SSSEQ = 1:5,
#'   SSDTC = "2020-01-04",
#'   SSTESTCD = "SURVSTAT",
#'   SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
#'   VISIT = "生存随访"
#' )
#'
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05")
#' )
#'
#' check_ss_ssdtc_alive_dm(SS1, DM)
#' check_ss_ssdtc_alive_dm(SS1, DM, preproc = ql_derive_seq)
#'
check_ss_ssdtc_alive_dm <- function(SS, DM, preproc = identity, ...) {
  # Validate required variables in SS and DM.
  if (SS %lacks_any% c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT")) {
    fail(lacks_msg(SS, c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply optional company specific preprocessing.
    SS <- preproc(SS, ...)
    
    myss <- subset(SS, !is_sas_na(SS$SSDTC) & SS$SSTESTCD == "SURVSTAT" & SS$SSORRES == "存活") %>%
      select(any_of(c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT", "SEQ", "SSSPID")))
    mydm <- subset(DM, !is_sas_na(DM$DTHDTC)) %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    mydf <- myss %>%
      left_join(mydm, by = "USUBJID") %>%
      mutate(
        ssdtc_date = as.Date(substr(SSDTC, 1, 10)),
        dthdtc_date = as.Date(substr(DTHDTC, 1, 10))
      ) %>%
      filter(!is.na(ssdtc_date), !is.na(dthdtc_date), ssdtc_date > dthdtc_date) %>%
      select(-ssdtc_date, -dthdtc_date)

    # Return pass when no inconsistent records are found.
    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) > 0) {
      fail(
        paste(length(unique(mydf$USUBJID)),
          " patient(s) with SSORRES='存活' and SSDTC later than DM.DTHDTC. ",
          sep = ""
        ),
        mydf
      )
    }
  }
}
