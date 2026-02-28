#' @title Check for death variable consistency when AEOUT=="DEATH"
#'
#' @description This check looks for consistency in AESDTH, AEDTHDTC, and
#' AETOXGR (if applicable) when AEOUT is 'DEATH'.  Note, this check expects
#' AE grade/severity variables to be populated for either all records or none.
#' In a case where both AETOXGR and AESEV exist and some records are supposed
#' to have AETOXGR populated while others have AESEV (ie the two variables are
#' mutually exclusive) then this check will likely return false positives.
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AEDECOD,
#' AESTDTC, AEDTHDTC, AEOUT, AESDTH
#' @param DM SDTM dataset with variable DTHDTC
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr filter select %>% bind_rows
#'
#' @export
#'
#' @author JH
#'
#' @examples
#'
#' # AETOXGR, no AESEV
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA),
#'   AETOXGR = c("5", "5", "5", NA, NA),
#'   AESEQ = 11:15,
#'   stringsAsFactors = FALSE
#' )
#'
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("01FEB2017", NA, "02FEB2017", "03FEB2017", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE, DM)
#' check_ae_fatal(AE, DM, preproc = ql_derive_seq)
#'
#' AE$AETOXGR <- NULL
#' check_ae_fatal(AE, DM)
#'
#' AE$AEDECOD <- NULL
#' check_ae_fatal(AE, DM)
#'
#'
#' # AESEV, no AETOXGR
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA),
#'   AESEV = c("SEVERE", "MILD", "SEVERE", NA, NA),
#'   AESEQ = 11:15,
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("01FEB2017", "02FEB2017", "03FEB2017", "04FEB2017", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE, DM)
#' check_ae_fatal(AE, DM, preproc = ql_derive_seq)
#'
#' AE$AESEV <- NULL
#' check_ae_fatal(AE, DM)
#'
#' # Both AESEV and AETOXGR have non-missing values
#'
#' AE <- data.frame(
#'   USUBJID = 1:7,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5", "AE6", "AE7"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA, "是", "是"),
#'   AESEV = c("SEVERE", "MILD", "SEVERE", NA, NA, "MILD", "SEVERE"),
#'   AETOXGR = c("5", "5", "5", NA, NA, "1", "5"),
#'   AESEQ = 11:17,
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:7,
#'   DTHDTC = c("01FEB2017", NA, "02FEB2017", "03FEB2017", NA, "04FEB2017", "05FEB2017"),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE, DM)
#' check_ae_fatal(AE, DM, preproc = ql_derive_seq)
#'
#'
#' # Neither AESEV or AETOXGR
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA),
#'   AESPID = "FORMNAME-R:12/L:2XXXX",
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("01FEB2017", NA, "02FEB2017", "03FEB2017", NA),
#'   stringsAsFactors = FALSE
#' )
#' check_ae_fatal(AE, DM)
#'
#' # AETOXGR exists but unmapped AESEV
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA),
#'   AESEV = rep(NA, 5),
#'   AETOXGR = c("5", "5", "5", NA, NA),
#'   AESEQ = 111:115,
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("01FEB2017", NA, "02FEB2017", "03FEB2017", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#'
#' check_ae_fatal(AE, DM)
#' check_ae_fatal(AE, DM, preproc = ql_derive_seq)
#'
#' # AETOXGR and AESEV exist, by both are unmapped
#'
#' AE <- data.frame(
#'   USUBJID = 1:5,
#'   AESTDTC = "01JAN2017",
#'   AEDECOD = c("AE1", "AE2", "AE3", "AE4", "AE5"),
#'   AEOUT = "死亡",
#'   AESDTH = c("是", "是", "否", "是", NA),
#'   AESEV = NA,
#'   AETOXGR = NA,
#'   AESEQ = "FORMNAME-R:12/L:2XXXX",
#'   stringsAsFactors = FALSE
#' )
#' DM <- data.frame(
#'   USUBJID = 1:5,
#'   DTHDTC = c("01FEB2017", NA, "02FEB2017", "03FEB2017", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_fatal(AE, DM)
#' check_ae_fatal(AE, DM, preproc = ql_derive_seq)
check_ae_fatal <- function(AE, DM, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AEDECOD", "AESTDTC", "AEOUT", "AESDTH")) {
    fail(lacks_msg(AE, c("USUBJID", "AEDECOD", "AESTDTC", "AEOUT", "AESDTH")))
  } else if (DM %lacks_any% c("USUBJID", "DTHDTC")) {
    fail(lacks_msg(DM, c("USUBJID", "DTHDTC")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)

    outlist <- list() # empty list for results
    ae1 <- AE
    dm1 <- DM %>%
      select(any_of(c("USUBJID", "DTHDTC")))
    AE <- ae1 %>% left_join(dm1, by = c("USUBJID"))

    # check if AEOUT=='死亡' that there is a corresponding AEDTHDTC, death date

    # 1. AETOXGR exists and is populated
    if (AE %has_any% "AETOXGR") { # if var exists
      if (!all(is_sas_na(AE$AETOXGR))) { # Only run check if var is mapped
        outlist[[1]] <- AE %>%
          filter(AEOUT == "死亡" & (is_sas_na(DTHDTC) | AETOXGR != 5 | is_sas_na(AETOXGR) | AESDTH != "是" | is_sas_na(AESDTH)))
      }
    }

    # 2. AESEV exists and is populated
    if (AE %has_any% "AESEV") { # if var exists
      if (!all(is_sas_na(AE$AESEV))) { # Only run check if var is mapped
        outlist[[2]] <- AE %>%
          filter(AEOUT == "死亡" & (is_sas_na(DTHDTC) | AESEV != "SEVERE" | AESDTH != "是" | is_sas_na(AESDTH)))
      }
    }

    # 3. If neither AETOXGR or AESEV exist
    if (!(AE %has_any% "AESEV" & AE %has_any% "AETOXGR")) {
      outlist[[3]] <- AE %>%
        filter(AEOUT == "死亡" & (is_sas_na(DTHDTC) | AESDTH != "是" | is_sas_na(AESDTH)))
    }

    # 4. If both AETOXGR or AESEV exist but both are not populated
    if ((AE %has_any% "AESEV" & AE %has_any% "AETOXGR")) {
      if (all(is_sas_na(AE$AESEV)) & all(is_sas_na(AE$AETOXGR))) { # Only run check if var is mapped
        outlist[[4]] <- AE %>%
          filter(AEOUT == "死亡" & (is_sas_na(DTHDTC) | AESDTH != "是" | is_sas_na(AESDTH)))
      }
    }

    mydf <- bind_rows(outlist)
    # leave only variables on which we want to check for fatalities and their corresponding death dates
    mydf <- unique(mydf[, intersect(names(AE), c("STUDYID", "USUBJID", "AEDECOD", "AESTDTC", "DTHDTC", "AEOUT", "AESEV", "AETOXGR", "AESDTH", "SEQ"))])
    rownames(mydf) <- NULL

    ### Return message if no inconsistency between AEOUT and AEDTHDTC
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are records with inconsistency
    } else if (nrow(mydf) > 0) {
      fail(paste("AE has ", length(unique(mydf$USUBJID)), " patient(s) with AE death variable inconsistencies when outcome marked FATAL. ", sep = ""), mydf)
    }
  }
}
