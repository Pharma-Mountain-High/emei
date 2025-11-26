#' @title Check for missing ECDOSU records
#'
#' @description This check looks for missing ECODOSU values for valid doses(ECMOOD="Y" and ECOCCUR = "Y")
#'
#' @param EC  SDTM dataset with variables USUBJID,ECTRT,ECSTDTC,ECDOSU
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @importFrom dplyr %>% select filter matches
#'
#' @export
#'
#' @author Jen Chen
#'
#' @examples
#'
#' EC <- data.frame(
#'   USUBJID = 1:10,
#'   ECTRT = 1:10,
#'   ECSTDTC = 1:10,
#'   ECDOSE = 1:10,
#'   ECOCCUR = as.character(c(rep("是", 5), rep("否", 5))),
#'   ECDOSU = as.character(rep("mg", 10)),
#'   ECMOOD = "已执行"
#' )
#'
#' EC$ECDOSU[1] <- ""
#' EC$ECDOSU[2] <- "NA"
#' EC$ECDOSU[3] <- NA
#'
#' check_ec_ecdosu(EC)
#'
#' EC$ECSTDTC <- NULL
#'
#' check_ec_ecdosu(EC)
#'
check_ec_ecdosu <- function(EC) {
  ### First bifurcate EC into a df based on occurrence of ECOCCUR
  if ("ECOCCUR" %in% names(EC)) {
    df <- EC %>% filter(ECMOOD == "已执行", ECOCCUR == "是")
  } else {
    df <- EC
  }

  ### Check that required variables exist and return a message if they don't
  if (EC %lacks_any% c("USUBJID", "ECTRT", "ECSTDTC", "ECDOSE", "ECDOSU")) {
    fail(lacks_msg(EC, c("USUBJID", "ECTRT", "ECSTDTC", "ECDOSE", "ECDOSU")))
  } else {
    ### Subset EC to only records with missing ECDOSU
    df <- df %>%
      # select all variables matching regular ecpressions.
      select(matches(match = "USUBJID$|ECTRT$|ECSTDTC$|ECSTDTC$|ECDOSE$|ECDOSU$|ECMOOD$")) %>%
      # order
      select(USUBJID, ECTRT, ECDOSE, ECSTDTC, ECDOSE, ECDOSU, ECMOOD) %>%
      # filter missing ECDOSU obs
      filter(is_sas_na(ECDOSU))

    rownames(df) <- NULL

    ### Print to report

    ### Return message if no records in df
    if (nrow(df) == 0) {
      pass()

      ### Return subset dataframe if there are records with missing ECDOSU
    } else if (nrow(df) > 0) {
      fail(
        msg = paste0("There are ", nrow(df), " records with missing dose units. "), data = df
      )
    }
  }
}
