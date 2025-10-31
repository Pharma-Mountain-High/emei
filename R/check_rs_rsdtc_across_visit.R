#' @title Check RS records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date RSDTC occurs
#'  across multiple visits. Only applies to assessments by investigator,
#'  selected based on uppercased RSEVAL = "INVESTIGATOR" or missing or
#'  RSEVAL variable does not exist.
#'
#' @param RS Disease Response SDTM dataset with variables USUBJID, RSDTC, VISIT,
#' RSEVAL (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select left_join
#' @importFrom stats aggregate
#' @importFrom tidyselect any_of
#'
#' @author Will Harris
#'
#' @examples
#'
#' # example that will be flagged
#' RS <- data.frame(
#'   USUBJID = 1,
#'   RSDTC = c(rep("2016-01-01", 3), rep("2016-06-01", 5), rep("2016-06-24", 2)),
#'   VISIT = c(rep("C1D1", 3), rep("C1D2", 3), rep("C2D1", 4)),
#'   RSSPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_rs_rsdtc_across_visit(RS)
#' check_rs_rsdtc_across_visit(RS, preproc = roche_derive_rave_row)
#'
#' # example that will not be flagged because not Investigator
#' RS0 <- RS
#' RS0$RSEVAL <- "INDEPENDENT ASSESSOR"
#' check_rs_rsdtc_across_visit(RS0)
#' check_rs_rsdtc_across_visit(RS0, preproc = roche_derive_rave_row)
#'
#' # example with log line differences in Rave form with records flagged
#' RS1 <- RS
#' RS1$RSSPID <- c(
#'   rep("FORMNAME-R:13/L:13XXXX", 4),
#'   rep("FORMNAME-R:13/L:14XXXX", 2),
#'   rep("FORMNAME-R:03/L:13XXXX", 2),
#'   rep("FORMNAME-R:9/L:13XXXX", 2)
#' )
#'
#' check_rs_rsdtc_across_visit(RS1)
#' check_rs_rsdtc_across_visit(RS1, preproc = roche_derive_rave_row)
#'
#' # example with RSTESTCD with records flagged
#' RS2 <- RS1
#' RS2$RSTESTCD <- c(
#'   rep("OVRLRESP", 2), rep("OTHER", 2),
#'   rep("OVRLRESP", 2), rep("OTHER", 2), rep("OVRLRESP", 2)
#' )
#' check_rs_rsdtc_across_visit(RS2)
#' check_rs_rsdtc_across_visit(RS2, preproc = roche_derive_rave_row)
#'
#' # example with records flagged without xxSPID
#' RS3 <- RS
#' RS3$RSSPID <- NULL
#' check_rs_rsdtc_across_visit(RS3)
#' check_rs_rsdtc_across_visit(RS3, preproc = roche_derive_rave_row)
#'
#' # example without required variable
#' RS4 <- RS
#' RS4$VISIT <- NULL
#' check_rs_rsdtc_across_visit(RS4)
#' check_rs_rsdtc_across_visit(RS4, preproc = roche_derive_rave_row)
#'
check_rs_rsdtc_across_visit <- function(RS, preproc = identity, ...) {
  if ((RS %lacks_any% c("USUBJID", "RSDTC", "VISIT"))) {
    fail(lacks_msg(RS, c("USUBJID", "RSDTC", "VISIT")))
  } else {
    RS <- preproc(RS, ...)
    rssub <- RS
    # 若存在 RSEVAL，则仅保留研究者或缺失的记录
    if (rssub %has_any% "RSEVAL") {
      rssub <- rssub %>% filter(toupper(RSEVAL) == "研究者" | is_sas_na(RSEVAL))
    }
    # 若存在 RSTESTCD，则排除 ECOG 相关记录
    if (rssub %has_any% "RSTESTCD") {
      rssub <- rssub %>% filter(!grepl("ECOG", RSTESTCD))
    }
    rssub <- rssub %>%
      select(USUBJID, RSDTC, VISIT, any_of(c("RSTESTCD", "RAVE"))) %>%
      filter(!is_sas_na(RSDTC))
    rs_orig <- rssub
    rssub <- rssub %>% select(-any_of(c("RSTESTCD", "RAVE")))
    if (nrow(rssub) > 0) {
      mypairs <- unique(rssub)
      mypairs$x <- 1
      mydf0 <- aggregate(mypairs$x, by = list(
        USUBJID = mypairs$USUBJID,
        RSDTC = mypairs$RSDTC
      ), FUN = sum)
      mydf0 <- mydf0 %>%
        select("USUBJID", "RSDTC") %>%
        filter(mydf0$x > 1)
      mypairs0 <- mypairs %>% select(
        "USUBJID", "RSDTC",
        "VISIT"
      )
      mydf <- merge(mydf0, mypairs0,
        by = c("USUBJID", "RSDTC"),
        all.x = TRUE
      ) %>%
        left_join(rs_orig, by = c(
          "USUBJID",
          "RSDTC", "VISIT"
        )) %>%
        unique()
      rownames(mydf) <- NULL
    } else {
      mydf <- data.frame()
    }
    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) > 0) {
      fail(
        paste(nrow(mydf), "records with same date at >1 visit. "),
        mydf
      )
    }
  }
}
