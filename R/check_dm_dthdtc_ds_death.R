#' @title Check for missing DTHDTC where DS indicates death
#'
#' @description This check looks for missing DTHDTC values if a patient has a
#'   DS record where DSDECOD="死亡"
#'
#' @param DM SDTM dataset with variables USUBJID, DTHDTC
#' @param DS Disposition SDTM dataset with variables USUBJID, DSDECOD, DSTERM, DSSTDTC
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
#' DM <- data.frame(
#'   USUBJID = 1:3,
#'   DTHDTC = c(NA, NA, "2020-01-01")
#' )
#'
#'
#' # older mapping
#' DS <- data.frame(
#'   USUBJID = 1:4,
#'   DSTERM = c(
#'     "死亡", "死亡",
#'     "死亡", "死亡"
#'   ),
#'   DSDECOD = rep("死亡", 4),
#'   DSSTDTC = "2020-01-01"
#' )
#'
#' check_dm_dthdtc_ds_death(DM, DS)
#'
#' DS$DSSTDTC <- NULL
#'
#' check_dm_dthdtc_ds_death(DM, DS)
#'
#' # newer mapping that
#' DS <- data.frame(
#'   USUBJID = 1:4,
#'   DSTERM = c(
#'     "不良事件", "不良事件",
#'     "不良事件", "死亡"
#'   ),
#'   DSDECOD = rep("死亡", 4),
#'   DSSTDTC = "2020-01-01"
#' )
#'
#' # pass for study with newer mapping, as another function (check_dd_death_date.R) covers this
#' check_dm_dthdtc_ds_death(DM, DS)

check_dm_dthdtc_ds_death <- function(DM, DS) {
  ### First check that required variables exist and return a message if they don't
  if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(AE, c("USUBJID", "DTHDTC")))
  } else if (DS %lacks_any% c("USUBJID", "DSDECOD", "DSTERM", "DSSTDTC")) {
    fail(lacks_msg(DS, c("USUBJID", "DSDECOD", "DSTERM", "DSSTDTC")))
  } else {

    if ("死亡" %in% DS$DSTERM) {
      ### if yes - then use existing function ###

      ######### Only consider DS records from patients who have a record with DEATH
      ds0a <- subset(DS, (regexpr("死亡", DS$DSDECOD, ignore.case = TRUE) != -1) &
        (regexpr("死亡", DS$DSTERM, ignore.case = TRUE) != -1), )

      ######### If there are no DS records that qualify for the check ###########
      if (nrow(ds0a) == 0) {
        pass()

        ######### If there are DS records that qualify ###########
      } else if (nrow(ds0a) > 0) {
        # Look for all records in DM where the death date is populated
        dm0a <- subset(DM, !is_sas_na(DM$DTHDTC), )

        # Check for subjects in ds0a that do not have a matching record in ae0a
        ds11 <- subset(ds0a, !(ds0a$USUBJID %in% dm0a$USUBJID), c("USUBJID", "DSTERM", "DSSTDTC"))
        rownames(ds11) <- NULL

        # If all all subjects in ds0a have a matching record in ae0a
        if (nrow(ds11) == 0) {
          pass()

          ### Return subset dataframe if there are records with inconsistency
        } else if (nrow(ds11) > 0) {
          fail(
            msg = paste(
              length(unique(ds11$USUBJID)),
              "patient(s) where DS.DSDECOD contains '死亡' and DS.DSTERM contains '死亡'",
              "but with no death date in DM.DTHDTC (DSTERM mapping only applicable to older studies). "
            ),
            data = ds11
          )
        }
      }
    }

    ######### If study has newer mapping where DSDECOD = DEATH
    else {
      ## assign pass, based on the idea this check being covered via check_dd_death_date.R
      ## consider consolidating those that check and this one

      pass()
    }
  }
}
