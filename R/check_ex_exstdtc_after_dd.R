#' @title Check for EX dates occurring after death date
#'
#' @description This check looks for EX dates that occur after death date
#'
#' @param DM Demographics SDTM dataset with variables USUBJID,
#'   DTHDTC
#'
#' @param EX Exposure SDTM dataset with variables USUBJID,
#'   EXSTDTC, EXTRT, and EXDOSE. EXOCCUR is optional and, when available,
#'   only EXOCCUR = "是" records are checked.
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
#' @importFrom dplyr %>% arrange filter left_join select
#'
#' @author 1
#'
#' @examples
#'
#' DM <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   DTHDTC = c(rep("", 4), "2016-01-02"),
#'   stringsAsFactors = FALSE
#' )
#'
#' EX <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   EXSTDTC = rep("2015-12-31", 5),
#'   EXTRT = LETTERS[1:5],
#'   EXDOSE = 1:5,
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ex_exstdtc_after_dd(DM, EX)
#'
#' EX$EXSTDTC[1] <- "2016-01-03"
#' EX$USUBJID[1] <- EX$USUBJID[5]
#'
#' check_ex_exstdtc_after_dd(DM, EX)
#'
check_ex_exstdtc_after_dd <- function(DM, EX) {
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(DM, c("USUBJID", "DTHDTC")))
  }

  if (EX %lacks_any% c("USUBJID", "EXSTDTC", "EXTRT", "EXDOSE")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(EX, c("USUBJID", "EXSTDTC", "EXTRT", "EXDOSE")))
  }

  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
  } else {
    DM$DTHDTC <- impute_day01(DM$DTHDTC)
    EX$EXSTDTC <- impute_day01(EX$EXSTDTC)

    ### Get death dates from DM
    dm_dd <- DM %>%
      select(USUBJID, DTHDTC) %>%
      filter(!is_sas_na(DM[["DTHDTC"]])) %>%
      arrange(USUBJID, DTHDTC)

    death_dates <- unique(dm_dd)


    if (nrow(death_dates) == 0) {
      pass() # If no death dates, then check automatically passes
    } else {
      df <- EX %>%
        filter(
          EX$USUBJID %in% death_dates[["USUBJID"]],
          !is_sas_na(EX$EXSTDTC),
          !is_sas_na(EX$EXTRT),
          !is_sas_na(EX$EXDOSE)
        )

      if ("EXOCCUR" %in% names(EX)) {
        df <- df %>% filter(df$EXOCCUR == "是")
      }

      suppressWarnings(
        df <- df %>% select("USUBJID", "EXSTDTC") %>%
          left_join(death_dates, by = "USUBJID")
      )
      df <- df %>%
        filter(as.Date(df$DTHDTC) < as.Date(df$EXSTDTC))

      if (nrow(df) == 0) {
        pass()
      } else {
        fail("Patient(s) with EX occurring after death date. ", df)
      }
    }
  }
}
