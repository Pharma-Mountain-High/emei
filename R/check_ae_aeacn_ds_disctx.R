#' @title Check if, whenever a patient experiences an AE leading to permanent drug discontinuation,
#' they also have a DS record indicating this.
#'
#' @description Flags patients with AE leading to "永久停药"
#' but no corresponding "治疗结束"/"研究结束" record in DS.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD, and at least one AEACN* variable
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT, DSDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check returns 0 obs, otherwise return subset dataframe.
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # FAIL: USUBJID 2 has "永久停药" in AE but no "结束" record in DS
#' AE <- data.frame(
#'   USUBJID = c("1", "2", "3"),
#'   AESEQ = c(1, 1, 1),
#'   AESTDTC = rep("2020-05-05", 3),
#'   AETERM = c("头痛", "心肌梗死", "寒战"),
#'   AEDECOD = c("头痛", "心肌梗死", "寒战"),
#'   AEACN = c("无", "永久停药", "无"),
#'   AEACN1 = c("", "", "永久停药"),
#'   stringsAsFactors = FALSE
#' )
#' DS <- data.frame(
#'   USUBJID = c("1", "3"),
#'   DSCAT = c("受试者处置", "受试者处置"),
#'   DSSCAT = c("治疗结束", "研究结束"),
#'   DSDECOD = c("完成", "不良事件"),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_aeacn_ds_disctx(AE, DS)
#'
#' # PASS: all patients with "永久停药" have corresponding DS "结束" record
#' DS2 <- data.frame(
#'   USUBJID = c("1", "2", "3"),
#'   DSCAT = c("受试者分布事件", "受试者分布事件", "受试者分布事件"),
#'   DSSCAT = c("治疗结束", "治疗结束", "研究结束"),
#'   DSDECOD = c("完成", "不良事件", "不良事件"),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_aeacn_ds_disctx(AE, DS2)
#'
check_ae_aeacn_ds_disctx <- function(AE, DS, preproc = identity, ...) {
  # First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AEDECOD") || length(grep("^AEACN", names(AE))) == 0) {
    fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AEACN*")))
  } else if (DS %lacks_any% c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD")) {
    fail(lacks_msg(DS, c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)

    # Get all AEACN* and AEACNOT* prefixed columns
    aeacnotx_cols <- grep("^AEACN", names(AE), value = TRUE)

    # Keep only AE columns that are needed
    ae1 <- AE %>%
      select(any_of(c("USUBJID", "AESEQ", "AEDECOD", "AESTDTC", aeacnotx_cols, "AEGRPID", "AESPID")))

    # Filter for AE records where any AEACNOTx column contains "永久停药"
    ae1$subj_discont_fl <- apply(ae1[aeacnotx_cols], 1, function(row) any(grepl("永久停药", row)))

    ae2 <- ae1 %>%
      filter(subj_discont_fl) %>%
      select(-subj_discont_fl)

    # Keep only DS columns that are needed
    ds1 <- DS %>% select(c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD"))

    # Filter for DS records indicating subject didn't complete the study
    ds2 <- ds1 %>% filter(grepl("结束", DSSCAT))

    # Merge AE and DS to cross-check records
    ae_ds <- ae2 %>% left_join(ds2, by = c("USUBJID"))

    # Keep only AE records where there is no corresponding DS record
    mydf <- ae_ds %>% filter(is.na(DSDECOD))
    rownames(mydf) <- NULL

    # Return pass message if no there is no inconsistency between AE and DS
    if (nrow(mydf) == 0) {
      pass()

      # Return subset dataframe if there are records with inconsistencies
    } else if (nrow(mydf) > 0) {
      return(fail(
        paste(length(unique(mydf$USUBJID)),
          " patient(s) with AEs leading to '永久停药' but no corresponding record in DS. ",
          sep = ""
        ),
        mydf
      ))
    }
  }
}
