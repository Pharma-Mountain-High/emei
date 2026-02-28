#' @title Check for missing TUDTC values
#'
#' @description This check looks for missing TUDTC values and returns a data frame.
#'   If TUEVAL exists, only records with TUEVAL = '研究者' are checked.
#'   If TUEVAL does not exist, all records are checked.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUDTC,
#' VISIT, TUORRES, TUEVAL (optional), TUTESTCD (optional), TULNKID (optional), TUSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Beeya Na
#'
#' @examples
#' TU <- data.frame(
#'   USUBJID = "1001",
#'   TUDTC = "2020-05-05",
#'   VISIT = "C1D1",
#'   TUORRES = 1:10,
#'   TUSEQ = 1:10,
#'   TULNKID = sprintf("T%02d", 1:10),
#'   TUSPID = sprintf("%02d", 1:10),
#'   TUEVAL = "研究者",
#'   TUTESTCD = "TUMIDENT",
#'   stringsAsFactors = FALSE
#' )
#'
#' TU$TUDTC[1] <- ""
#' TU$TUDTC[2] <- "NA"
#' TU$TUDTC[3] <- NA
#'
#' check_tu_tudtc(TU, preproc = ql_derive_seq)
#'
#' TU$TUEVAL[1] <- ""
#' TU$TUTESTCD <- NULL
#' check_tu_tudtc(TU, preproc = ql_derive_seq)
#'
#' TU$TUEVAL[2] <- "独立评估者"
#' TU$TUEVAL[3] <- "独立评估者"
#' TU$TUDTC[4] <- ""
#' check_tu_tudtc(TU)
#'
#' TU$TUSPID <- NULL
#' check_tu_tudtc(TU)
#'
#' TU$VISIT <- NULL
#' check_tu_tudtc(TU)
#'
#' ## PASS example
#' TU_PASS <- data.frame(
#'   USUBJID = c("1001", "1002"),
#'   TUDTC = c("2020-05-05", "2020-05-06"),
#'   VISIT = c("C1D1", "C1D1"),
#'   TUORRES = c("A", "B"),
#'   TUEVAL = c("研究者", "独立评估者"),
#'   stringsAsFactors = FALSE
#' )
#' check_tu_tudtc(TU_PASS)
#'
check_tu_tudtc <- function(TU, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (TU %lacks_any% c("USUBJID", "TUDTC", "VISIT", "TUORRES")) {
    fail(lacks_msg(TU, c("USUBJID", "TUDTC", "VISIT", "TUORRES")))
  } else {
    # Apply company specific preprocessing function
    TU <- preproc(TU, ...)
    myvars <- c("USUBJID", "VISIT", "TUDTC", "TUORRES", names(TU)[names(TU) %in% c("SEQ", "TUEVAL", "TUTESTCD", "TULNKID", "TUSPID")])

    TU <- subset(TU, select = myvars)

    if (TU %lacks_any% "TUEVAL") {
      ### Subset TU to only records with missing TUDTC
      mydf <- TU %>%
        filter(is_sas_na(TUDTC))
    } else {
      mydf <- TU %>%
        filter(is_sas_na(TUDTC), toupper(TUEVAL) == "研究者")
    }

    rownames(mydf) <- NULL

    if (nrow(mydf) == 0) {
      pass()
    } else {
      ### Return subset dataframe if there are records with missing TUDTC
      fail(paste(
        "TU has", length(unique(mydf$USUBJID)), "patient(s) with", nrow(mydf),
        "record(s) with missing TUDTC. "
      ), mydf)
    }
  }
}
