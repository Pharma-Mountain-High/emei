#' @title Check if death reported in DM then death indicator also present in DS or AE
#'
#' @description This checks that when death is indicated in DM with either of DTHFL or
#'              DTHDTC then there should be death indicated in either AE or DS.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDTHDTC, AESDTH, AEOUT
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSSTDTC
#' @param DM Demographics SDTM dataset with variables USUBJID, DTHFL, DTHDTC
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
#' AE <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = 1:3,
#'   AEDTHDTC = c(NA, 1, NA),
#'   AESDTH = c(NA, "是", NA),
#'   AEOUT = c(NA, "死亡", NA),
#'   AETOXGR = c(NA, "5", NA)
#' )
#'
#' DS <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = 1:3,
#'   DSDECOD = c(NA, "死亡", NA),
#'   DSSTDTC = c(NA, "DSDATE", NA)
#' )
#'
#' DM <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = 1:3,
#'   DTHFL = c(NA, "是", "是"),
#'   DTHDTC = c(NA, "DMDATE", "DMDATE")
#' )
#'
#' check_dm_ae_ds_death(DM, DS, AE)
#'
#' DS$DSDECOD <- NULL
#'
#' check_dm_ae_ds_death(DM, DS, AE)
#'
check_dm_ae_ds_death <- function(DM, DS, AE) {
  if (AE %lacks_any% c("USUBJID", "AEDTHDTC", "AEOUT", "AESDTH")) {
    fail(lacks_msg(AE, c(
      "USUBJID", "AEDTHDTC", "AEOUT",
      "AESDTH"
    )))
  } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSSTDTC")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSSTDTC")))
  } else if (DM %lacks_any% c("USUBJID", "DTHFL", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHFL", "DTHDTC")))
  } else {
    # 在两处 subset 前增加，按分支分别计算 sel
    if (AE %has_any% "AETOXGR") {
      sel <- intersect(c("USUBJID", "AEDTHDTC", "AETOXGR", "AESDTH", "AEOUT", "AEGRPID", "AESPID"), names(AE))
      aedth <- subset(AE, !(is_sas_na(AE$AEDTHDTC)) | AE$AESDTH == "是" | grepl("死亡", AE$AEOUT) | AE$AETOXGR == "5",
        select = sel
      )
    } else {
      sel <- intersect(c("USUBJID", "AEDTHDTC", "AESDTH", "AEOUT", "AEGRPID", "AESPID"), names(AE))
      aedth <- subset(AE, !(is_sas_na(AE$AEDTHDTC)) | AE$AESDTH == "是" | grepl("死亡", AE$AEOUT),
        select = sel
      )
    }
    dsdth <- subset(DS, grepl("死亡", DS$DSDECOD), select = c(
      "USUBJID",
      "DSSTDTC"
    ))
    dmdth <- subset(DM, !(is_sas_na(DM$DTHDTC)) | DM$DTHFL ==
      "是", select = c("USUBJID", "DTHDTC", "DTHFL"))
    if (nrow(aedth) > 0) {
      aedths <- aedth[order(aedth$USUBJID, aedth$AEDTHDTC), ]
      aedthsl <- unique(aedths)
    } else if (nrow(aedth) == 0) {
      aedthsl <- aedth
    }
    if (nrow(dsdth) > 0) {
      dsdths <- dsdth[order(dsdth$USUBJID, dsdth$DSSTDTC), ]
      dsdthsl <- unique(dsdths)
    } else if (nrow(dsdth) == 0) {
      dsdthsl <- dsdth
    }
    aeds <- merge(
      x = aedthsl, y = dsdthsl, by = "USUBJID",
      all = TRUE
    )
    mydf <- subset(dmdth, !(dmdth$USUBJID %in% aeds$USUBJID))
    rownames(mydf) <- NULL
    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) > 0) {
      fail(
        msg = paste(length(unique(mydf$USUBJID)), "patient(s) where DM data indicates death but no record indicating death in DS or AE. "),
        data = mydf
      )
    }
  }
}
