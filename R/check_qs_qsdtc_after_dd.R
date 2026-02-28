#' @title Check for QS dates occurring after death date
#'
#' @description This check looks for QS dates that occur after death date
#'
#' @param DM Demographics SDTM dataset with variables USUBJID,
#'   DTHDTC
#'
#' @param QS Questionnaire Test Findings SDTM dataset with variables
#'   USUBJID, QSDTC, QSCAT, and QSORRES. QSTESTCD and QSSTAT are optional.
#'
#' @importFrom dplyr %>% arrange filter left_join select n_distinct distinct
#'
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
#' DM <- data.frame(
#'   USUBJID = c(1, 2),
#'   DTHDTC = c("2016-01-01", "2016-01-01"),
#'   stringsAsFactors = FALSE
#' )
#'
#' QS <- data.frame(
#'   USUBJID = c(1, 1, 1, 2, 2, 2),
#'   QSDTC = c(
#'     "2015-06-30", "2015-09-30", "2015-12-30",
#'     "2015-06-30", "2015-09-30", "2015-12-30"
#'   ),
#'   QSCAT = "A",
#'   QSTESTCD = "ECOG",
#'   QSORRES = LETTERS[1:6],
#'   QSSTAT = "",
#'   VISIT = c("Week 1", "Week 12", "Week 24", "Week 1", "Week 12", "Week 24"),
#'   QSSTRESC = LETTERS[1:6],
#'   stringsAsFactors = FALSE
#' )
#'
#' check_qs_qsdtc_after_dd(DM, QS)
#'
#' QS$QSDTC[3:5] <- "2016-01-03"
#' check_qs_qsdtc_after_dd(DM, QS)
#'
#' QS$QSSTAT[3] <- "未查"
#' check_qs_qsdtc_after_dd(DM, QS)
#'
#' DM1 <- data.frame(
#'   USUBJID = 1,
#'   DTHDTC = "2016-01",
#'   stringsAsFactors = FALSE
#' )
#'
#' QS1 <- data.frame(
#'   USUBJID = 1,
#'   QSDTC = c("2015-06-30", "2016-01-15", "2016-01-15"),
#'   QSCAT = rep("EQ-5D-5L"),
#'   QSTESTCD = "ECOG",
#'   QSORRES = "1",
#'   QSSTAT = "",
#'   VISIT = c("C1/D1", "C2/D1", "C2/D1"),
#'   QSSTRESC = "1",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_qs_qsdtc_after_dd(DM = DM1, QS = QS1)
check_qs_qsdtc_after_dd <- function(DM, QS) {
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(DM, c("USUBJID", "DTHDTC")))
  }
  if (QS %lacks_any% c("USUBJID", "QSDTC", "QSCAT", "QSORRES")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(QS, c("USUBJID", "QSDTC", "QSCAT", "QSORRES")))
  }

  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
  } else {
    # Add day of "01" to dates that are in the format of "yyyy-mm"
    DM$DTHDTC <- impute_day01(DM$DTHDTC)
    QS$QSDTC <- impute_day01(QS$QSDTC)

    ### Get death dates from DM
    dm_dd <- DM %>%
      select(USUBJID, DTHDTC) %>%
      filter(!is_sas_na(DM[["DTHDTC"]])) %>%
      arrange(USUBJID, DTHDTC)

    death_dates <- unique(dm_dd)

    if (nrow(death_dates) == 0) {
      pass() # If no death dates, then check automatically passes
    } else {
      if (QS %has_all% c("QSSTAT")) {
        suppressWarnings(
          mydf0 <- QS %>%
            filter(
              !grepl("未查", QSSTAT) &
                USUBJID %in% death_dates[["USUBJID"]] &
                !is_sas_na(QSDTC) &
                !is_sas_na(QSORRES)
            ) %>%
            left_join(death_dates, by = "USUBJID")
        )
      } else {
        suppressWarnings(
          mydf0 <- QS %>%
            filter(USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(QSDTC), !is_sas_na(QSORRES)) %>%
            left_join(death_dates, by = "USUBJID")
        )
      }

      mydf <- mydf0 %>%
        filter(as.Date(DTHDTC) < as.Date(QSDTC)) %>%
        select(any_of(c("USUBJID", "QSDTC", "QSTESTCD", "VISIT", "QSEVAL", "QSCAT", "DTHDTC")))

      if (nrow(mydf) == 0) {
        pass()
      } else {
        fail(paste0(n_distinct(mydf$USUBJID), " unique patient(s) with ", nrow(mydf), " QS record(s) occurring after death date. "), (mydf %>% distinct()))
      }
    }
  }
}
