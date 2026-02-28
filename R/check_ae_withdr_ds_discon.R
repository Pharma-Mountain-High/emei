#' @title Check if an AE leading to drug being withdrawn is reflected in DS
#'
#' @description This checks that if there is an AE with AEACN="DRUG WITHDRAWN" then there should be a treatment discontinuation
#'              record indicated by DS.DSSCAT
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEACN
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT, DSSCAT
#' @param TS Trial Summary SDTM dataset with variables TSPARMCD, TSVAL
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' AE <- data.frame(
#'   USUBJID = c("001", "002"),
#'   AEACN = c("永久停药", "永久停药"),
#'   AEDECOD = c("恶心", "头痛"),
#'   stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'   USUBJID = c("001", "003"),
#'   DSSCAT = c("研究中止", "研究结束"),
#'   DSCAT = c("处置事件", "处置事件"),
#'   DSDECOD = c("不良事件", "完成"),
#'   stringsAsFactors = FALSE
#' )
#'
#' TS <- data.frame(
#'   TSPARMCD = c("TRT"),
#'   TSVAL = c("Drug A"),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_withdr_ds_discon(AE, DS, TS)
check_ae_withdr_ds_discon <- function(AE, DS, TS, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AEACN")) {
    fail(lacks_msg(AE, c("USUBJID", "AEACN")))
  } else if (DS %lacks_any% c("USUBJID", "DSSCAT", "DSCAT", "DSDECOD")) {
    fail(lacks_msg(DS, c("USUBJID", "DSSCAT", "DSCAT", "DSDECOD")))
  } else if (TS %lacks_any% c("TSPARMCD", "TSVAL")) {
    fail(lacks_msg(DS, c("TSPARMCD", "TSVAL")))
  } else {
    # calculate number of drugs in the study
    agent_num <- filter(TS, (TSPARMCD %in% c("TRT", "COMPTRT"))) %>% nrow()

    # if a study is not single agent the check won't be executed
    if (agent_num != 1) {
      fail("This check is only applicable for single agent studies, but based on TS domain this study is not single agent or study type cannot be determined. ")
    }
    # only run for single agent studies
    else if (agent_num == 1) {
      # in ae keep rows where the drug was withdrawn

      # Apply company specific preprocessing function
      AE <- preproc(AE, ...)

      ae0 <- subset(AE, AE$AEACN == "永久停药", ) %>%
        select(any_of(c("USUBJID", "AEACN", "AEDECOD", "AEGRPID", "AESPID")))


      # find matching patients in DS
      DS <- preproc(DS, ...)

      ds0 <- subset(DS, (DS$USUBJID %in% ae0$USUBJID))
      ds1 <- subset(ds0, (grepl("研究结束", toupper(ds0$DSSCAT)) |
        grepl("研究中止", toupper(ds0$DSSCAT)) |
        grepl("治疗结束", toupper(ds0$DSSCAT))) &
        (grepl("处置事件", toupper(ds0$DSCAT)) |
          grepl("受试者分布事件", toupper(ds0$DSCAT))) &
        (ds0$DSDECOD != "完成")) %>%
        select(any_of(c("USUBJID", "DSSCAT", "DSCAT", "AEGRPID", "AESPID", "AESEQ")))


      # check which patients have TREATMENT DISCON FORM
      mydfprep <- merge(unique(ds1), ae0, "USUBJID", all.y = TRUE)

      ## to fix the following line to use the subset function
      mydf <- subset(mydfprep, is_sas_na(mydfprep$DSSCAT))
      rownames(mydf) <- NULL

      ### Print to report

      ### Return message if no records with missing TREATMENT DISCON form (i.e., DS.DSSCAT includes "TREATMENT DISCON" and DS.DSCAT includes "DISPO")
      if (nrow(mydf) == 0) {
        pass()
        ### Return subset dataframe if there are records with missing TREATMENT DISCON
      } else if (nrow(mydf) > 0) {
        fail(
          msg = paste(
            "There are",
            length(unique(mydf$USUBJID)),
            "patient(s) where AE data treatment discontinuation but no treatment discontinuation record in DS. "
          ),
          data = mydf
        )
      }
    }
  }
}
