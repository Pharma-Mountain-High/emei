#' @title Check for multiple dates at the same visit in QS
#'
#' @description Identifies multiple dates at the same visit in QS by
#'   USUBJID, QSCAT, QSTESTCD, and VISIT.
#'   Unscheduled visits are excluded.
#'
#' @param QS QS SDTM dataset with variables USUBJID, QSCAT, QSTESTCD, VISIT, QSDTC
#'
#' @return Boolean value for whether the check passed or failed, with 'msg'
#'   attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select rename mutate distinct slice group_by n
#'
#' @author 1
#'
#' @examples
#'
#' QS1 <- data.frame(
#'   USUBJID = c(rep(101, 5), rep(102, 5)),
#'   QSCAT = "DLQI",
#'   QSTESTCD = "DLQI01",
#'   QSDTC = rep(c(
#'     "2017-01-01T08:25", "2017-01-05T09:25",
#'     "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
#'   ), 2),
#'   VISITNUM = rep(1:5, 2),
#'   VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "计划外", "C4/D1"), 2),
#'   stringsAsFactors = FALSE
#' )
#' check_qs_dup(QS = QS1)
#'
#' # multiple dates for the same visit in QS
#' QS2 <- QS1
#' QS2$VISIT[QS2$USUBJID == 101] <- "C1/D1"
#' check_qs_dup(QS = QS2)
#'
check_qs_dup <- function(QS) {
  req_vars <- c("USUBJID", "QSTESTCD", "QSDTC", "VISIT")
  if (QS %lacks_any% req_vars) {
    fail(lacks_msg(QS, req_vars))
  } else {
    has_qscat <- "QSCAT" %in% names(QS)
    QS <- mutate(QS, QSDTC1 = substr(QSDTC, 1, 10))

    if (has_qscat) {
      df <- QS %>%
        select(USUBJID, QSCAT, QSTESTCD, QSDTC1, VISIT) %>%
        rename(QSDTC = QSDTC1) %>%
        group_by(USUBJID, QSCAT, QSTESTCD, QSDTC) %>%
        slice(1)
      df <- subset(df, !grepl("计划外", toupper(df$VISIT)))
      df2 <- df %>%
        group_by(USUBJID, QSCAT, QSTESTCD, VISIT) %>%
        filter(n() > 1)
    } else {
      df <- QS %>%
        select(USUBJID, QSTESTCD, QSDTC1, VISIT) %>%
        rename(QSDTC = QSDTC1) %>%
        group_by(USUBJID, QSTESTCD, QSDTC) %>%
        slice(1)
      df <- subset(df, !grepl("计划外", toupper(df$VISIT)))
      df2 <- df %>%
        group_by(USUBJID, QSTESTCD, VISIT) %>%
        filter(n() > 1)
    }

    if (nrow(df2) != 0) {
      fail("Multiple dates for the same visit in QS. ", df2)
    } else {
      pass()
    }
  }
}
