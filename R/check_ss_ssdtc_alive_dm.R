#' @title Check non-missing last ALIVE status date in SS is before than death date in DM
#'
#' @description This check looks for non-missing SS.SSDTC when SS.SSORRES contains '存活' and
#'              Subject Status Date/Time of Assessments is greater then
#'              Date/Time of Death in Demographics(SS.SSDTC > DM.DTHDTC)
#'
#' @param SS Subject Status SDTM dataset with variables USUBJID, SSDTC, SSORRES, SSTESTCD, VISIT, SSSPID (optional)
#' @param DM Demographics SDTM dataset with variables USUBJID, DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr left_join filter %>% select
#' @importFrom tidyselect any_of
#'
#' @author 1
#'
#' @examples
#'
#' SS <- data.frame(
#'   USUBJID = 1:5,
#'   SSDTC = "2020-01-02",
#'   SSTESTCD = "SURVSTAT",
#'   SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
#'   VISIT = "WEEK 4",
#'   SSSEQ = 1:5
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
#'   SSDTC = "2020-01-04",
#'   SSTESTCD = "SURVSTAT",
#'   SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
#'   VISIT = "WEEK 4",
#'   SSSEQ = 1:5
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
  ### First check that required variables exist and return a message if they don't
  if (SS %lacks_any% c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT")) {
    fail(lacks_msg(SS, c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    SS <- preproc(SS, ...)
    
    myss <- subset(SS, !is_sas_na(SS$SSDTC) & SS$SSTESTCD == "SURVSTAT" & grepl("存活", SS$SSORRES)) %>%
      select(any_of(c("USUBJID", "SSDTC", "SSORRES", "SSTESTCD", "VISIT", "SEQ", "SSSPID")))
    mydm <- subset(DM, !is_sas_na(DM$DTHDTC)) %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    mydf <- myss %>%
      left_join(mydm, by = "USUBJID") %>%
      filter(SSDTC > DTHDTC)

    ### Print to report

    ### Return message if no records
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are records where SS.SSDTC > DM.DTHDTC
    } else if (nrow(mydf) > 0) {
      fail(
        paste(length(unique(mydf$USUBJID)),
          " patient(s) with ALIVE status date in SS domain later than death date in DM domain. ",
          sep = ""
        ),
        mydf
      )
    }
  }
}
