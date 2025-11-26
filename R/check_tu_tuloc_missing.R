#' @title Check for missing TULOC values
#'
#' @description This check looks for target lesions with missing TULOC values and
#' returns a data frame. Only applies to assessments by investigator.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUDTC,
#' VISIT, TUORRES, TULOC,TUMETHOD, TUSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author Will Harris
#'
#' @examples

#' # Passing example: no missing TULOC among investigator rows
#' TU <- data.frame(
#'   USUBJID  = c("01", "02", "03"),
#'   TUDTC    = c("2020-01-01", "2020-01-02", "2020-01-03"),
#'   VISIT    = "C1D1",
#'   TUORRES  = "TARGET",
#'   TULOC    = c("LIVER", "LUNG", "BONE"),
#'   TUMETHOD = c("CT", "CT", "MRI"),
#'   TUEVAL   = c("研究者", "研究者", "研究者"),
#'   stringsAsFactors = FALSE
#' )
#' check_tu_tuloc_missing(TU)
#'
#' # Failing example: introduce missing TULOC
#' TU$TULOC[2] <- ""
#' TU$TULOC[3] <- NA
#' res <- check_tu_tuloc_missing(TU)
#' attr(res, "msg")
#' head(as.data.frame(attr(res, "data")))
#'
#' # Example without TUEVAL column (still checks TUMETHOD and TULOC)
#' TU2 <- TU
#' TU2$TUEVAL <- NULL
#' check_tu_tuloc_missing(TU2)


check_tu_tuloc_missing <- function(TU, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (TU %lacks_any% c("USUBJID", "TUDTC", "VISIT", "TUORRES", "TULOC", "TUMETHOD")) {
    fail(lacks_msg(TU, c("USUBJID", "TUDTC", "VISIT", "TUORRES", "TULOC", "TUMETHOD")))
  } else {
    # Apply company specific preprocessing function
    TU <- preproc(TU, ...)

    if (TU %lacks_any% "TUEVAL") {
      ### Subset TU to only target lesions with missing TULOC
      mydf <- TU %>%
        filter(is_sas_na(TULOC), !is_sas_na(TUMETHOD)) %>%
        select(any_of(c("USUBJID", "TUDTC", "VISIT", "TUORRES", "TULOC", "RAVE")))
    } else {
      mydf <- TU %>%
        filter(is_sas_na(TULOC), !is_sas_na(TUMETHOD), toupper(TUEVAL) == "研究者" | is_sas_na(TUEVAL)) %>%
        select(any_of(c("USUBJID", "TUDTC", "VISIT", "TUORRES", "TULOC", "RAVE")))
    }

    rownames(mydf) <- NULL

    if (nrow(mydf) == 0) {
      pass()
    } else {
      ### Return subset dataframe if there are records with missing TUDTC
      fail(paste("There are", nrow(mydf), "target lesions with missing TULOC. "), mydf)
    }
  }
}
