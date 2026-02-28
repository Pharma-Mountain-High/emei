#' @title Check for VS dates occurring after death date
#'
#' @description This check looks for VS dates that occur after death date
#'
#' @param DM Demographics SDTM dataset with variables USUBJID,
#'   DTHDTC
#'
#' @param VS Vital Signs SDTM dataset with variables USUBJID,
#'   VSDTC, VSTESTCD, and VSORRES. VSSTAT is optional.
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the check failed
#'
#' @export
#'
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
#' VS <- data.frame(
#'   STUDYID = 1:5, USUBJID = LETTERS[1:5],
#'   VSDTC = rep("2015-12-31", 5),
#'   VSTESTCD = letters[1:5],
#'   VSORRES = 1:5,
#'   VSSTAT = "",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_vs_vsdtc_after_dd(DM, VS)
#'
#' VS$VSDTC[1] <- "2016-01-03"
#' VS$USUBJID[1] <- VS$USUBJID[5]
#'
#' check_vs_vsdtc_after_dd(DM, VS)
#'
#' VS$VSSTAT[1] <- "未查"
#' check_vs_vsdtc_after_dd(DM, VS)
#'
check_vs_vsdtc_after_dd <- function(DM, VS) {
  lacks_msgs <- NULL # For storing lack_msg from all required SDTM datasets

  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(DM, c("USUBJID", "DTHDTC")))
  }

  if (VS %lacks_any% c("USUBJID", "VSDTC", "VSTESTCD", "VSORRES")) {
    lacks_msgs <- c(lacks_msgs, lacks_msg(VS, c(
      "USUBJID", "VSDTC", "VSTESTCD",
      "VSORRES"
    )))
  }

  if (length(lacks_msgs) > 0) {
    fail(msg = paste0(lacks_msgs, collapse = ". "))
  } else {
    DM$DTHDTC <- impute_day01(DM$DTHDTC)
    VS$VSDTC <- impute_day01(VS$VSDTC)

    ### Get death dates from DM
    dm_dd <- DM %>%
      select(USUBJID, DTHDTC) %>%
      filter(!is_sas_na(DM[["DTHDTC"]])) %>%
      arrange(USUBJID, DTHDTC)

    death_dates <- unique(dm_dd)

    if (nrow(death_dates) == 0) {
      pass() # If no death dates, then check automatically passes
    } else {
      if (VS %has_all% c("VSSTAT")) {
        suppressWarnings(
          df0 <- VS %>%
            filter(
              !grepl("未查", VSSTAT),
              USUBJID %in% death_dates[["USUBJID"]],
              !is_sas_na(VSDTC),
              !is_sas_na(VSORRES)
            ) %>%
            select(USUBJID, VSDTC, VSTESTCD) %>%
            left_join(death_dates, by = "USUBJID")
        )
      } else {
        suppressWarnings(
          df0 <- VS %>%
            filter(
              USUBJID %in% death_dates[["USUBJID"]],
              !is_sas_na(VSDTC),
              !is_sas_na(VSORRES)
            ) %>%
            select(USUBJID, VSDTC, VSTESTCD) %>%
            left_join(death_dates, by = "USUBJID")
        )
      }
      df <- df0 %>%
        filter(as.Date(df0$DTHDTC) < as.Date(df0$VSDTC))

      if (nrow(df) == 0) {
        pass()
      } else {
        fail("Patient(s) with VS occurring after death date. ", df)
      }
    }
  }
}
