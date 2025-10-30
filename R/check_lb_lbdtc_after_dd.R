#' @title Check for LB dates occurring after death date
#'
#' @description This check looks for LB dates that occur after death date
#'
#' @param DM Demographics SDTM dataset with variables USUBJID,
#'   DTHDTC
#'
#' @param LB Laboratory Test Findings SDTM dataset with variables USUBJID,
#'   LBDTC, LBTESTCD, and LBORRES
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
#' LB <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   LBDTC = rep("2015-12-31", 5),
#'   LBTESTCD = letters[1:5],
#'   LBORRES = 1:5,
#'   stringsAsFactors = FALSE
#' )
#'
#' check_lb_lbdtc_after_dd(DM, LB)
#'
#' LB$LBDTC[1] <- "2016-01-03"
#' LB$USUBJID[1] <- LB$USUBJID[5]
#'
#' check_lb_lbdtc_after_dd(DM, LB)
#'
check_lb_lbdtc_after_dd <- function(DM, LB) {
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(DM, c("USUBJID", "DTHDTC")))
  }

  if (LB %lacks_any% c("USUBJID", "LBDTC", "LBTESTCD", "LBORRES")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(LB, c("USUBJID", "LBDTC", "LBTESTCD", "LBORRES")))
  }

  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
  } else {
    DM$DTHDTC <- impute_day01(DM$DTHDTC)
    LB$LBDTC <- impute_day01(LB$LBDTC)

    ### Get death dates from DM
    dm_dd <- DM %>%
      select(USUBJID, DTHDTC) %>%
      filter(!is_sas_na(DM[["DTHDTC"]])) %>%
      arrange(USUBJID, DTHDTC)

    death_dates <- unique(dm_dd)


    if (nrow(death_dates) == 0) {
      pass() # If no death dates, then check automatically passes
    } else {
      suppressWarnings(
        mydf0 <- LB %>%
          filter(USUBJID %in% death_dates[["USUBJID"]], !is_sas_na(LBDTC), !is_sas_na(LBORRES)) %>%
          select(USUBJID, LBDTC, LBTESTCD) %>%
          left_join(death_dates, by = "USUBJID")
      )
      mydf <- mydf0 %>%
        filter(as.Date(DTHDTC) < as.Date(LBDTC))

      if (nrow(mydf) == 0) {
        pass()
      } else {
        fail("Patient(s) with LB occurring after death date. ", mydf)
      }
    }
  }
}
