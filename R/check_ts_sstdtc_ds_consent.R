#' @title Check for missing SSTDTC (Study Start Date) in TS
#'
#' @description This check looks for missing SSTDTC (Study Start Date) in TS;
#' if it's present, check that the date matches the earliest informed consent among
#' any subject enrolled in the study. The FDA Technical Rejection Criteria for
#' Study Data - effective September 15, 2021 requires Study Start Date
#' (https://www.fda.gov/media/100743/download). If missing, no data queries are
#' needed - this would be updating the assignment in the TS domain.
#'
#' @param TS Trial Summary SDTM dataset with variables TSPARMCD, TSPARM, TSVAL
#' @param DS Disposition SDTM dataset with variables DSCAT, DSDECOD, DSSTDTC
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select distinct mutate
#'
#' @author 1
#'
#' @examples
#'
#' TS1 <- data.frame(
#'   STUDYID = 1,
#'   TSPARMCD = "SSTDTC",
#'   TSPARM = "Study Start Date",
#'   TSVAL = "2017-01-01",
#'   TSVAL1 = "",
#'   TSVAL2 = ""
#' )
#'
#' TS2 <- data.frame(
#'   STUDYID = 2,
#'   TSPARMCD = "AEDICT",
#'   TSPARM = "Study Start Date",
#'   TSVAL = "MedDRA v23.0",
#'   TSVAL1 = ""
#' )
#'
#' TS3 <- data.frame(
#'   STUDYID = 3,
#'   TSPARMCD = "SSTDTC",
#'   TSPARM = "Study Start Date",
#'   TSVAL = ""
#' )
#'
#' TS4 <- data.frame(
#'   STUDYID = 1,
#'   TSPARMCD = "SSTDTC",
#'   TSPARM = "Study Start Date",
#'   TSVAL = "2020-01-02",
#'   TSVAL1 = "",
#'   TSVAL2 = ""
#' )
#'
#' TS5 <- rbind(TS1, TS4)
#'
#' TS6 <- data.frame(
#'   STUDYID = 1,
#'   TSPARMCD = "SSTDTC",
#'   TSPARM = "Study Start Date",
#'   TSVAL = "2020-01",
#'   TSVAL1 = "",
#'   TSVAL2 = ""
#' )
#'
#' DS1 <- data.frame(
#'   USUBJID = c(1, 1, 2, 3, 4),
#'   DSCAT = rep("方案里程碑", 5),
#'   DSSCAT = rep("方案里程碑", 5),
#'   DSDECOD = c(
#'     "签署知情同意", "其他", "医生决定",
#'     "其他", "签署知情同意"
#'   ),
#'   DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS1)
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS2)
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS3)
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS4)
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS5)
#' check_ts_sstdtc_ds_consent(DS = DS1, TS = TS6)
#'
check_ts_sstdtc_ds_consent <- function(DS, TS) {
  if (DS %lacks_any% c("DSCAT", "DSDECOD", "DSSTDTC")) {
    fail(lacks_msg(DS, c("DSCAT", "DSDECOD", "DSSTDTC")))
  } else if (TS %lacks_any% c("TSPARMCD", "TSPARM", "TSVAL")) {
    fail(lacks_msg(TS, c("TSPARMCD", "TSPARM", "TSVAL")))
  } else if (!("SSTDTC" %in% TS[["TSPARMCD"]])) {
    fail("TS.TSPARMCD = 'SSTDTC' (Study Start Date), which is required for FDA submissions, not found. Date in TSVAL should correspond with earliest informed consent date of enrolled patient. ")
  } else {
    mydf <- subset(TS, TS$TSPARMCD == "SSTDTC", c(
      "TSPARMCD",
      "TSPARM", "TSVAL"
    ))
    mydf$DS_FIRST_ICDATE <- NA
    ic <- subset(
      DS,
      DS$DSDECOD %in% c("签署知情同意", "知情同意签署") &
        DS$DSCAT == "方案里程碑",
      c("DSCAT", "DSDECOD", "DSSTDTC")
    )
    if (nrow(ic) >= 1) {
      ic$icdate <- substr(ic$DSSTDTC, 1, 10)
      ic <- ic %>%
        filter(nchar(DSSTDTC) == 10) %>%
        mutate(icdate = as.Date(icdate)) %>%
        filter(icdate == min(icdate)) %>%
        select(icdate) %>%
        distinct()
      mydf$DS_FIRST_ICDATE <- ic$icdate
    }
    if (nrow(mydf) > 1) {
      fail(
        "Multiple records with TS.TSPARMCD = 'SSTDTC' (Study Start Date) found when only one expected. TS with Study Start
Date required for FDA submissions. ",
        mydf
      )
    } else {
      if (is_sas_na(mydf$TSVAL)) {
        fail(
          "TS.TSPARMCD = 'SSTDTC' (Study Start Date) has missing TS.TSVAL (yyyy-mm-dd) character date. TS with Study Start Date required for FDA submissions. ",
          mydf
        )
      } else if (nchar(as.character(mydf$TSVAL)) < 10) {
        fail(
          "TS.TSPARMCD = 'SSTDTC' (Study Start Date) is missing complete TS.TSVAL (yyyy-mm-dd) character date. TS with Study Start Date required for FDA submissions. ",
          mydf
        )
      } else if (is_sas_na(mydf$DS_FIRST_ICDATE)) {
        fail(
          "TS.TSPARMCD = 'SSTDTC' (Study Start Date) has TS.TSVAL (yyyy-mm-dd), which is required for FDA submissions, but earliest informed consent date from DS cannot be calculated as a reference date. ",
          mydf
        )
      } else if (mydf$TSVAL == mydf$DS_FIRST_ICDATE) {
        pass()
      } else {
        fail(
          "TS.TSPARMCD = 'SSTDTC' (Study Start Date) has TS.TSVAL that does not match earliest informed consent date based on DS. TS with Study Start Date required for FDA submissions. ",
          mydf
        )
      }
    }
  }
}
