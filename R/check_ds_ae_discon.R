#' @title Check for treatment discontinuation consistency between DS and AE
#'
#' @description This check looks for DS records indicating treatment
#'              discontinuation due to AE (DSDECOD/DSTERM contains "不良事件"),
#'              then verifies there should be AE.AEACN*="永久停药"
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM,
#' AEDECOD, AESTDTC, AEACN (and optional AEACNx variables)
#'
#' @param DS Disposition SDTM dataset with variables USUBJID, DSCAT,
#' DSSCAT, DSDECOD, DSSTDTC. DSTERM is optional.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#' test failed
#'
#' @importFrom dplyr distinct
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # PASS: DS has discon due to AE and AE has matching "永久停药"
#' AE <- data.frame(
#'   USUBJID = 1:3,
#'   AESTDTC = "2017-01-01",
#'   AETERM = c("头痛", "恶心", "皮疹"),
#'   AEDECOD = c("头痛", "恶心", "皮疹"),
#'   AEACN = c("永久停药", "剂量不变", "不适用"),
#'   stringsAsFactors = FALSE
#' )
#'
#' DS <- data.frame(
#'   USUBJID = 1:3,
#'   DSSTDTC = "2017-01-01",
#'   DSCAT = rep("处置事件", 3),
#'   DSSCAT = rep("治疗结束", 3),
#'   DSDECOD = c("不良事件", "完成", "完成"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ds_ae_discon(DS, AE)
#'
#' # FAIL: USUBJID=2 DS discon due to AE but no matching AEACN="永久停药"
#' DS[2, "DSDECOD"] <- "不良事件"
#' check_ds_ae_discon(DS, AE)
#'
#' # multiple AEACNx: USUBJID=1 has "永久停药" in AEACN1
#' AE2 <- data.frame(
#'   USUBJID = 1:3,
#'   AESTDTC = "2017-01-01",
#'   AETERM = c("头痛", "恶心", "皮疹"),
#'   AEDECOD = c("头痛", "恶心", "皮疹"),
#'   AEACN = rep("MULTIPLE", 3),
#'   AEACN1 = c("永久停药", "剂量不变", "不适用"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ds_ae_discon(DS, AE2)
#'
check_ds_ae_discon <- function(DS, AE) {
  ### First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN")) {
    fail(lacks_msg(AE, c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN")))
  } else if (DS %lacks_any% c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD", "DSSTDTC")) {
    fail(lacks_msg(DS, c("USUBJID", "DSCAT", "DSSCAT", "DSDECOD", "DSSTDTC")))
  } else {
    # keep variables on which we want to check action taken with treatment
    ae0 <- subset(AE,
      select = c(
        "USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEACN",
        grep("^AEACN.", names(AE), value = TRUE)
      )
    )


    # select if AE dataset has AEACNx variables
    aeacnvars <- grep("^AEACN.", names(ae0), value = TRUE)

    # create a where condition for AE
    whrcond <- paste("AEACN=='永久停药'")
    for (i in aeacnvars) {
      whrcond <- paste(whrcond, " | ", i, "=='永久停药'", sep = "")
    }

    # loop through each AEACNx and keep DRUG WITHDRAWN AEs
    ae1 <- subset(
      ae0,
      eval(parse(text = whrcond))
    )

    # keep unique usubjid
    ae2 <- distinct(ae1, USUBJID, .keep_all = TRUE)


    # keep records with discontinuation treatment due to AE
    ae_reason <- grepl("不良事件", DS$DSDECOD)
    if ("DSTERM" %in% names(DS)) {
      ae_reason <- ae_reason | grepl("不良事件", DS$DSTERM)
    }
    ds0 <- subset(DS,
      grepl("处置事件|受试者分布事件", DSCAT) & ae_reason,
      select = c("USUBJID", "DSSCAT", "DSDECOD", "DSSTDTC")
    )


    finout <- merge(ae2, ds0,
      by = "USUBJID",
      all = TRUE
    )


    # check if DS record with AE has corresponding record in AE
    mydf <- subset(finout,
      is_sas_na(AETERM),
      select = c("USUBJID", "DSSCAT", "DSDECOD", "DSSTDTC")
    )

    rownames(mydf) <- NULL

    ### Return message if no inconsistency between AEOUT and AEDTHDTC
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {
      return(fail(paste(length(unique(mydf$USUBJID)),
        " patient(s) with Treatment Discon due to AE but no AE record indicating drug withdrawn. ",
        sep = ""
      ), mydf))
    } # end else if mydf has records
  } # end else if required variable exist
} # end check_ds_ae_discon()
