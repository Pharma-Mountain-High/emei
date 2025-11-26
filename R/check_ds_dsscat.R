#' @title Check for patients with more than one study discontinuation records
#'
#' @description This check looks for patient who has more than one study
#' discontinuation records
#'
#' @param DS Disposition SDTM dataset with variables USUBJID,DSSCAT
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Madeleine Ma
#'
#' @examples
#'
#' DS <- data.frame(
#'   USUBJID = c(rep(1, 3), rep(2, 3), rep(3, 3)),
#'   DSSCAT = rep(c("研究结束", "不良事件", "方案"), 3),
#'   stringsAsFactors = FALSE
#' )
#' check_ds_dsscat(DS)
#'
#' DS$DSSCAT[8] <- "研究结束"
#' check_ds_dsscat(DS)
#'
#' DS$DSSCAT <- NULL
#' check_ds_dsscat(DS)
#'
check_ds_dsscat <- function(DS) {
  if (DS %lacks_any% c("USUBJID", "DSSCAT")) {
    fail(lacks_msg(DS, c("USUBJID", "DSSCAT")))
  } else {
    myds <- DS %>%
      select("USUBJID", "DSSCAT") %>%
      filter(grepl(
        "研究",
        toupper(DSSCAT)
      ) & grepl("结束", toupper(DSSCAT)) &
        !grepl("药物", toupper(DSSCAT)) & !grepl(
        "治疗",
        toupper(DSSCAT)
      ))
    if (nrow(myds) == 0) {
      fail(paste("There are no study discontinuation records"))
    } else {
      n_uniqueID <- length(unique(myds$USUBJID))
      n_occur <- data.frame(table(myds$USUBJID))
      colnames(n_occur) <- c("USUBJID", "Freq")
      IDlist <- subset(n_occur, n_occur$Freq > 1)
      myrecs <- merge(myds, subset(IDlist, select = c("USUBJID")),
        by = "USUBJID", all.y = TRUE
      )
      colnames(IDlist) <- c("Duplicate USUBJID", "Number of Records")
      if (n_uniqueID == nrow(myds)) {
        pass()
      } else if (n_uniqueID != nrow(myds)) {
        fail(
          paste("Patients with more than one study discontinuation records. "),
          myrecs
        )
      }
    }
  }
}
