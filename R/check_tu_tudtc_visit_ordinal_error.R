#' @title Check that all TU  dates are duplicated or earlier than last
#' visit's (possible datetime data entry error)
#'
#' @description This check identifies TUDTC values that are duplicated or
#' earlier than last visit's. Unscheduled visits are excluded.
#'
#' @param TU Tumor Identification SDTM dataset with variables USUBJID, TUORRES,
#'   TULOC, VISITNUM, VISIT, TUDTC, TUEVAL. Optional variables TUSPID and TULNKID
#'   are used for grouping when available.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # no case
#' TU <- data.frame(
#'   USUBJID = 101:102,
#'   TUORRES = rep(c("新病灶", "靶病灶"), 5),
#'   TULOC = rep(c("骨", "肝"), 5),
#'   TUDTC = rep(c(
#'     "2017-01-01T08:25", "2017-01-05T09:25",
#'     "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
#'   ), 2),
#'   VISITNUM = rep(1:5, 2),
#'   VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "C4/D1", "C5/D1"), 2),
#'   TUEVAL = "研究者",
#'   stringsAsFactors = FALSE
#' )
#' check_tu_tudtc_visit_ordinal_error(TU)
#'
#' # adding cases with earler date
#' TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "C4/D1"] <- "2017-01-10T08:25"
#' TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "C2/D1"] <- "2017-01-01T06:25"
#' check_tu_tudtc_visit_ordinal_error(TU)
#'
#' # adding cases with duplicated date
#' TU$TUDTC[TU$USUBJID == 101 & TU$VISIT == "C5/D1"] <- "2017-01-10T08:25"
#' TU$TUDTC[TU$USUBJID == 102 & TU$VISIT == "C3/D1"] <- "2017-01-01T06:25"
#' check_tu_tudtc_visit_ordinal_error(TU)
check_tu_tudtc_visit_ordinal_error <- function(TU) {
  class(TU) <- "data.frame"
  key_vars_optional <- names(TU)[names(TU) %in% c("TUSPID", "TULNKID")]
  vars <- c(
    "USUBJID", "TUORRES", "TULOC", key_vars_optional,
    "VISITNUM", "VISIT", "TUDTC", "TUEVAL"
  )
  if (TU %lacks_any% vars) {
    fail(lacks_msg(TU, vars))
  } else if (length(unique(TU[["VISITNUM"]])) <= 1) {
    fail(msg = "VISITNUM exists but only a single value. ")
  } else {
    subsetdf <- subset(TU, TU$TUEVAL == "研究者" & !grepl(
      "计划外",
      toupper(TU$VISIT)
    ), )
    if (nrow(subsetdf) > 0) {
      groupby_vars <- c("USUBJID", "TUORRES", "TULOC", key_vars_optional)
      order_vars <- c("USUBJID", "TUORRES", "TULOC", key_vars_optional, "VISITNUM", "VISIT", "TUDTC")
      mydf2 <- do.call(
        dtc_dupl_early,
        c(
          list(
            dts = subsetdf,
            vars = vars,
            groupby = groupby_vars,
            dtc = "TUDTC"
          ),
          as.list(order_vars)
        )
      )
      myout <- mydf2[!is.na(mydf2$check.flag), ]
      if (nrow(myout) == 0) {
        pass()
      } else if (nrow(myout) > 0) {
        rownames(myout) <- NULL
        fail(paste("TU has ", nrow(myout), " records with Possible TUDTC data entry error. ",
          sep = ""
        ), myout)
      }
    } else {
      fail("No records when subset to only INV records. ")
    }
  }
}
