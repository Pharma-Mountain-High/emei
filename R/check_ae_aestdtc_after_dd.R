#' @title Check for AE dates occurring after death date
#'
#' @description This check looks for AE dates that occur after death date
#'
#' @param AE Adverse Event SDTM dataset with variables USUBJID,
#'   AESTDTC, AEDECOD, AETERM, AESPID (optional)
#' @param DM Demographics SDTM dataset with variables USUBJID,
#'   DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% arrange filter left_join select everything
#' @importFrom tidyselect any_of
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'

#' @author 1
#'
#' @examples
#'
#' AE <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   AESTDTC = rep("2016-01-01", 5),
#'   AEDECOD = c("", "", rep("Myocarditis", 3)),
#'   AETERM = c("INJURY", rep("MYOCARDITIS", 4)),
#'   AESPID = "FORMNAME-R:19/L:19XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' DM <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   DTHDTC = c(rep("", 4), "2016-01-02"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aestdtc_after_dd(AE, DM)
#'
#' AE$AESTDTC[1] <- "2016-01-03"
#' AE$USUBJID[1] <- AE$USUBJID[5]
#'
#' check_ae_aestdtc_after_dd(AE, DM, preproc = roche_derive_rave_row)
#'
#' AE$AESPID <- NULL
#' check_ae_aestdtc_after_dd(AE, DM)
#'
#' AE$AESTDTC <- NULL
#' check_ae_aestdtc_after_dd(AE, DM)
#'
check_ae_aestdtc_after_dd <- function(AE, DM, preproc = identity, ...) {
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

  if (AE %lacks_any% c("USUBJID", "AESTDTC", "AEDECOD", "AETERM")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(AE, c("USUBJID", "AESTDTC", "AEDECOD", "AETERM")))
  }

  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(DM, c("USUBJID", "DTHDTC")))
  }

  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)
    DM <- preproc(DM, ...)

    AE$AESTDTC <- impute_day01(AE$AESTDTC)
    DM$DTHDTC <- impute_day01(DM$DTHDTC)

    ### Subset AE to fewer variables
    AE <- AE %>%
      select(any_of(c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEGRPID", "AESPID", "RAVE")))

    ### Get death dates from DM
    death_dates <- DM %>%
      select(USUBJID, DTHDTC) %>%
      filter(!is_sas_na(DM[["DTHDTC"]])) %>%
      arrange(USUBJID, DTHDTC)

    death_dates <- unique(death_dates)

    if (nrow(death_dates) == 0) {
      pass() # If no death dates, then check automatically passes
    } else {
      df1 <- AE %>%
        filter(AE$USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(AE$AESTDTC)) %>%
        select(-AEDECOD)

      suppressWarnings(
        df2 <- df1 %>%
          left_join(death_dates, by = "USUBJID")
      )
      df <- df2 %>%
        filter(as.Date(df2$DTHDTC) < as.Date(df2$AESTDTC)) %>%
        select(USUBJID, AETERM, AESTDTC, DTHDTC, everything())

      if (nrow(df) == 0) {
        pass()
      } else {
        fail(paste0(
          length(unique(df$USUBJID)),
          " patient(s) with AE occurring after death date. "
        ), df)
      }
    }
  }
}
