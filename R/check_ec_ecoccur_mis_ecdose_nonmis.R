#' @title EC Missing Occurrence but Non-Missing Dose Check
#' @description Records with missing drug administration occurrence info but non-missing dose value

#'
#' @param EC EC domain data frame containing USUBJID, ECTRT, ECOCCUR, ECDOSE, ECSTDTC, ECMOOD
#'
#' @return Boolean value with attributes; returns FALSE with attached data if issues are found
#' @export
#' @importFrom dplyr %>% filter select
#' @examples
#'
#' EC <- data.frame(
#'   USUBJID = 1:5,
#'   ECSTDTC = rep("2017-01-01", 5),
#'   ECTRT = c(rep("TRT A", 2), rep("TRT B", 3)),
#'   ECOCCUR = c("是", NA, ".", "", "否"),
#'   ECDOSE = c(10, 15, 0, 20, 5),
#'   ECMOOD = "已执行",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ec_ecoccur_mis_ecdose_nonmis(EC)
#'
#' EC2 <- data.frame(
#'   USUBJID = 1:4,
#'   ECSTDTC = rep("2017-01-01", 4),
#'   ECTRT = rep("TRT A", 4),
#'   ECOCCUR = c("是", "否", "是", "否"),
#'   ECDOSE = c(10, 15, 20, 5),
#'   ECMOOD = "已执行",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ec_ecoccur_mis_ecdose_nonmis(EC2)
#'
#' EC3 <- EC
#' EC3$ECOCCUR <- NULL
#'
#' check_ec_ecoccur_mis_ecdose_nonmis(EC3)
#'
check_ec_ecoccur_mis_ecdose_nonmis <- function(EC) {
  # 先校验所需字段是否存在
  required_vars <- c("USUBJID", "ECTRT", "ECOCCUR", "ECDOSE", "ECSTDTC", "ECMOOD")
  if (EC %lacks_any% required_vars) {
    return(fail(lacks_msg(EC, required_vars)))
  }

  # 再进行筛选，避免缺列时报错
  EC <- EC %>% filter(ECMOOD == "已执行", ECOCCUR != "是")

  if (length(EC$USUBJID) == 0) {
    return(pass())
  }

  ex2 <- EC %>%
    select("USUBJID", "ECTRT", "ECOCCUR", "ECDOSE", "ECSTDTC") %>%
    filter(!is.na(ECDOSE))
  rownames(ex2) <- NULL

  if (nrow(ex2) > 0) {
    fail(paste("There are", nrow(ex2), "EC records with ECOCCUR missing but ECDOSE not missing. "), ex2)
  } else {
    pass()
  }
}
