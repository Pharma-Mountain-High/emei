#' @title check ec exdose pos exoccur_no
#'
#' @param EC SDTM dataset
#' @param drug  TRT A
#' @description EX Valid Dose, Missing EXOCCUR	Records with missing drug administration
#' occurrence  info but non-missing dose value
#'
#' @return Returns message depending on if there are records with positive dose
# but occurrence not marked as "是"
#' @export
#' @importFrom dplyr %>% filter select
#' @examples
#' EC <- data.frame(
#'   USUBJID = 1:6,
#'   ECSTDTC = rep("2017-01-01", 6),
#'   ECTRT = c(rep("TRT A", 3), rep("TRT B", 3)),
#'   ECOCCUR = c("是", "否", ".", "", "是", "否"),
#'   ECDOSE = c(0, 10, 5, 15, 0, 8),
#'   VISIT = "VISIT 1",
#'   ECMOOD = "已执行",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ec_ecdose_pos_ecoccur_no(EC)
#'
#' check_ec_ecdose_pos_ecoccur_no(EC, drug = "TRT A")
#' check_ec_ecdose_pos_ecoccur_no(EC, drug = "TRT B")
#'
#' EC$ECDOSE <- NULL
#'
#' check_ec_ecdose_pos_ecoccur_no(EC)
#'
check_ec_ecdose_pos_ecoccur_no <- function(EC, drug = NULL) {
  EC <- EC %>% filter(ECMOOD == "已执行")
  # Checks if required variables are present
  if (EC %lacks_any% c("USUBJID", "ECTRT", "ECSTDTC", "ECOCCUR", "ECDOSE", "VISIT")) {
    fail(lacks_msg(EC, c("USUBJID", "ECTRT", "ECSTDTC", "ECOCCUR", "ECDOSE", "VISIT")))

    # Checks validity of drug name argument
  } else if (!is.null(drug) && !(drug %in% EC[["ECTRT"]])) {
    fail(msg = "Drug name not found in dataset. ")
  } else {
    # Subsets EC to rows where ECOCCUR is not "Y" but ECDOSE is positive
    df <- EC %>%
      select("USUBJID", "ECTRT", "ECSTDTC", "ECOCCUR", "ECDOSE", "VISIT") %>%
      filter(EC$ECOCCUR != "是" & EC$ECDOSE > 0)

    if (!is.null(drug)) {
      df <- df %>% filter(df$ECTRT == drug)
    }

    # Returns message depending on if there are records with positive dose
    # but occurrence not marked as "是"
    if (nrow(df) != 0 && !is.null(drug)) {
      fail(paste0(
        "There are ", length(unique(df$USUBJID)), " patients with ",
        "positive dose amount (ECDOSE>0) when occurrence (ECOCCUR) for ",
        drug, " is not '是'. "
      ), df)
    } else if (nrow(df) != 0) {
      fail(paste0(
        "There are ", length(unique(df$USUBJID)), " patients with ",
        "positive dose amount (ECDOSE>0) when occurrence (ECOCCUR)",
        " is not '是'. "
      ), df)
    } else {
      pass()
    }
  }
}
