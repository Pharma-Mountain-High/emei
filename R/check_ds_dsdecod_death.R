#' @title Check for study discontinuation record if death indicated
#'
#' @description If a patient has a record where DS.DSDECOD == DEATH they should
#'   also have a Study Discon Record
#'
#' @param DS Disposition domain with variables USUBJID, DSDECOD, DSSCAT, and
#' optional variables DSCAT, DSSTDTC, DSSPID
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author JH
#'
#' @importFrom dplyr distinct %>% select filter
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @examples
#'
#' DS <- data.frame(
#'   STUDYID = "XXX",
#'   USUBJID = 1:3,
#'   DSSEQ = 1:3,
#'   DSDECOD = c(NA, "死亡", NA),
#'   DSSTDTC = c(NA, "DSDATE", NA),
#'   DSCAT = c("处置事件", "处置事件", "其他"),
#'   DSSCAT = c(
#'     "研究结束",
#'     "治疗结束",
#'     "研究结束"
#'   ),
#'   DSOTH = 1:3,
#'   DSSPID = "XXX-R:0",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ds_dsdecod_death(DS)
#' check_ds_dsdecod_death(DS, preproc = ql_derive_seq)
#'
#' DS$DSSCAT[2] <- "研究结束"
#' check_ds_dsdecod_death(DS)
#'
#' DS$DSDECOD <- NULL
#' check_ds_dsdecod_death(DS)
#'
check_ds_dsdecod_death <- function(DS, preproc = identity, ...) {
  if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSCAT")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSCAT")))
  } else {
    # Apply company specific preprocessing function
    DS <- preproc(DS, ...)
    DS <- DS %>%
      select(any_of(c("USUBJID", "DSDECOD", "DSCAT", "DSSCAT", "DSSTDTC", "SEQ")))

    # Subset DS death records and include only records without a STUDY
    # COMPLETION/DISCONTINUATION form

    # find records with DEATH indicated in DS - note that these are not
    # necessarily unique by USUBJID
    ref <- DS %>%
      filter(grepl("死亡", toupper(DSDECOD)))

    # find patients that have a STUDY DISCONTINUATION record
    discon <- DS %>%
      filter(
        !grepl("药物", toupper(DSSCAT)),
        !grepl("治疗", toupper(DSSCAT)),
        (
          (grepl("研究", toupper(DSSCAT)) & grepl("结束", toupper(DSSCAT))) |
            grepl("终止研究", toupper(DSSCAT)) |
            grepl("研究终止", toupper(DSSCAT))
        )
      ) %>%
      mutate(DISCFL = "Y") %>%
      select(USUBJID, DISCFL)

    discon <- distinct(discon)

    # merge datasets to output only patients without DISCON records in DS
    # but known to have died
    mydf0 <- merge(
      x = ref,
      y = discon,
      by = c("USUBJID"),
      all.x = TRUE,
      all.y = FALSE
    )

    mydf <- mydf0 %>%
      filter(is_sas_na(DISCFL)) %>%
      select(-any_of(c("DISCFL", "DSSCAT")))

    # replace <NA> with blank in DSSCAT
    mydf[is.na(mydf)] <- ""

    rownames(mydf) <- NULL

    if (nrow(mydf) == 0) {
      pass()
    } else {
      fail(
        paste(
          nrow(mydf), "record(s) for",
          length(unique(mydf$USUBJID)),
          "unique patient(s) with DSDECOD='死亡' but no",
          "record with DSSCAT indicating STUDY DISCONTINUATION. "
        ),
        data = mydf
      )
    }
  }
}
