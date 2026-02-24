#' @title Check for concomitant medication indication with text string "预防用药"
#' when not given for prophylaxis
#'
#' @description This check looks for patients with text string "预防用药" in CMINDC
#' when CMPROPH is not checked as "是" in studies with given for prophylaxis
#' variable (CMPROPH)
#'
#' @param CM Concomitant Medication SDTM dataset with variables USUBJID, CMTRT,
#' CMSTDTC, CMINDC, CMPROPH, CMSPID (optional)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#'
#' @author  1
#'
#' @examples
#'
#' CM <- data.frame(
#'   USUBJID = c(rep(1, 3), rep(2, 3), rep(3, 3)),
#'   CMTRT = letters[1:9],
#'   CMSTDTC = rep("2017-01-01", 9),
#'   CMINDC = c(
#'     rep("INDICATION 1", 2), rep("indication 2", 2),
#'     rep("预防用药", 2), rep("预防用药", 2), "预防用药"
#'   ),
#'   CMPROPH = c(rep("是", 3), rep(NA, 2), rep("", 2), "NA", "."),
#'   CMSPID = "/F:XXX-D:12345-R:123",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_cm_cmindc(CM)
#' check_cm_cmindc(CM, preproc = ql_derive_seq)
#'
#' CM$CMPROPH[7] <- "是"
#' check_cm_cmindc(CM)
#'
#' CM$CMSPID <- NULL
#' check_cm_cmindc(CM, preproc = ql_derive_seq)
#'
#' CM$CMPROPH <- NULL
#' check_cm_cmindc(CM)
#'
check_cm_cmindc <- function(CM, preproc = identity, ...) {
  ### First check that required variables exist and return a message if they don't
  if (CM %lacks_any% c("USUBJID", "CMTRT", "CMSTDTC", "CMINDC", "CMPROPH")) {
    fail(lacks_msg(CM, c("USUBJID", "CMTRT", "CMSTDTC", "CMINDC", "CMPROPH")))
  } else {
    # Apply company specific preprocessing function
    CM <- preproc(CM, ...)

    perm_var <- c("SEQ")
    int_var <- intersect(names(CM), perm_var)

    # keep records not indicated as prophylactic via CMPROPH

    cmNP <- CM %>%
      filter(is_sas_na(CMPROPH)) %>%
      select(any_of(c("USUBJID", int_var, "CMTRT", "CMSTDTC", "CMINDC", "CMPROPH")))

    mydf <- cmNP %>% filter(grepl("预防用药", toupper(CMINDC)))
    rownames(mydf) <- NULL

    # check

    if (nrow(mydf) == 0) {
      pass()
    } else {
      fail(
        paste0(
          "There are ", length(unique(mydf$USUBJID)),
          " patients with CM indication containing '预防用药' when given for prophylaxis variable is not checked as '是'. "
        ),
        mydf
      )
    }
  }
}
