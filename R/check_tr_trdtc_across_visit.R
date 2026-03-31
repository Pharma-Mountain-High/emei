#' @title Check TR Longest/Sum Diameter Records where the same date occurs across multiple visits
#'
#' @description This check identifies records where the same date TRDTC occurs
#'  across multiple visits for selected tumor response measurements
#'  (TRTESTCD in "LDIAM", "SUMDIAM", "DIAMTETER", "TUMSTATE").
#'  Same-date checks are performed by USUBJID + TRDTC + TRTESTCD,
#'  and TRLNKID is additionally included when available.
#'  Only applies to investigator assessments (TREVAL = "研究者"), or when
#'  TREVAL is missing or the TREVAL variable does not exist.
#'
#' @param TR Tumor Result SDTM dataset with variables USUBJID, TRDTC, TRTESTCD, VISIT,
#' TREVAL (optional), TRLNKID (optional)
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
#' @author 1
#'
#' @examples
#'
#' # PASS: no records flagged with current key (USUBJID + TRDTC + TRTESTCD + TRLNKID)
#' TR <- data.frame(
#'   USUBJID = 1,
#'   TRDTC = c(rep("2016-01-01", 3), rep("2016-06-01", 5), rep("2016-06-24", 2)),
#'   VISIT = c(rep("C1D1", 3), rep("C1D2", 3), rep("C2D1", 4)),
#'   TRTESTCD = c(rep("LDIAM", 5), rep("SUMDIAM", 2), rep("SAXIS", 3)),
#'   TRSEQ = 1:10,
#'   TRLNKID = c(sprintf("T%02d", 1:6), "T01", sprintf("T%02d", 1:3)),
#'   TRSPID = c(sprintf("%02d", 1:6), "01", sprintf("%02d", 1:3)),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_tr_trdtc_across_visit(TR)
#' check_tr_trdtc_across_visit(TR, preproc = ql_derive_seq)
#'
#' # FAIL: same date across visits after aligning TRLNKID within same TRTESTCD
#' TR2 <- TR
#' TR2$TRLNKID[TR2$VISIT == "C2D1" & TR2$TRTESTCD == "SUMDIAM"] <- "T06"
#'
#' check_tr_trdtc_across_visit(TR2)
#' check_tr_trdtc_across_visit(TR2, preproc = ql_derive_seq)
#'
#' # PASS: missing optional variable
#' TR3 <- TR
#' TR3$TRSPID <- NULL
#'
#' check_tr_trdtc_across_visit(TR3)
#' check_tr_trdtc_across_visit(TR3, preproc = ql_derive_seq)
#'
#' # missing required variable
#' TR4 <- TR
#' TR4$TRTESTCD <- NULL
#'
#' check_tr_trdtc_across_visit(TR4)
#' check_tr_trdtc_across_visit(TR4, preproc = ql_derive_seq)
#'
check_tr_trdtc_across_visit <- function(TR, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if ((TR %lacks_any% c("USUBJID", "TRDTC", "VISIT", "TRTESTCD"))) {
    fail(lacks_msg(TR, c("USUBJID", "TRDTC", "VISIT", "TRTESTCD")))
  } else {
    # Apply company specific preprocessing function
    TR <- preproc(TR, ...)

    ### Find unique pairs of TRDTC and VISIT per USUBJID

    if (TR %lacks_any% "TREVAL") {
      trsub <- TR %>%
        filter(TRTESTCD %in% c("LDIAM", "SUMDIAM", "DIAMTETER", "TUMSTATE") &
          !is_sas_na(TRDTC)) %>%
        select(USUBJID, TRDTC, VISIT, TRTESTCD, any_of(c("SEQ", "TRLNKID", "TRSPID")))
    } else {
      trsub <- TR %>%
        filter(TRTESTCD %in% c("LDIAM", "SUMDIAM", "DIAMTETER", "TUMSTATE") &
          (toupper(TREVAL) == "研究者" | is_sas_na(TREVAL)) & !is_sas_na(TRDTC)) %>%
        select(USUBJID, TRDTC, VISIT, TRTESTCD, any_of(c("SEQ", "TRLNKID", "TRSPID")))
    }

    tr_orig <- trsub # Save additional columns for merging in later
    key_vars <- c("USUBJID", "TRDTC", "TRTESTCD", names(trsub)[names(trsub) %in% c("TRLNKID")])
    trsub <- trsub %>% select(any_of(c(key_vars, "VISIT"))) # dont want to unique on non-key vars

    if (nrow(trsub) > 0) {
      mypairs <- unique(trsub)
      mypairs$x <- 1

      ### Get counts of visit values per date for each subject
      mydf0 <- aggregate(mypairs$x, by = mypairs[, key_vars, drop = FALSE], FUN = sum)

      ### Subset where count is >1 and output
      mydf0 <- mydf0 %>%
        select(any_of(key_vars)) %>%
        filter(mydf0$x > 1)

      mypairs0 <- mypairs %>%
        select(any_of(c(key_vars, "VISIT")))

      mydf <- merge(mydf0, mypairs0, by = key_vars, all.x = TRUE) %>%
        left_join(tr_orig, by = c(key_vars, "VISIT")) %>% # merge in RAVE var if it exists
        unique()
      rownames(mydf) <- NULL
    } else {
      mydf <- data.frame()
    }

    ### if no consistency
    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) > 0) {
      ### Return subset dataframe if there are records with inconsistency
      fail(
        paste("There are", nrow(mydf), "TR Longest/Sum Diameter records where the same date occurs accross multiple visits. "),
        mydf
      )
    }
  }
}
