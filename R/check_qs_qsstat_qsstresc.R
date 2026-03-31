#' @title Check for non-missing QSSTRESC if QSSTAT is NOT DONE
#'
#' @description This check is for studies with PRO outcomes data (i.e., QS domain),
#' check that within a given instrument (e.g., QS.QSCAT='BFI' or QS.QSCAT ='MDASI"),
#' if QS.QSSTAT=NOT DONE or QSTESTCD=QSALL, then there should be no populated
#' responses(QS.QSSTRESC) for a particular visit (QS.VISIT), return a dataframe if otherwise
#'
#' @param QS Questionnaires SDTM dataset with variables USUBJID, QSSTRESC,
#' VISIT, QSSTAT, QSCAT, QSDTC, QSTESTCD
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author Jh
#'
#' @examples
#'
#' QS <- data.frame(
#'   STUDYID = 1,
#'   USUBJID = c(rep(1, 6), rep(2, 6)),
#'   QSSTRESC = 1:12,
#'   VISIT = c(rep(1, 3), rep(2, 3), rep(1, 3), rep(2, 3)),
#'   QSSTAT = rep(c("", "未查"), 6),
#'   QSCAT = rep(c("INDIVIDUAL", "OVERALL", "BFI"), 4),
#'   QSDTC = "2016-01-01",
#'   QSTESTCD = c(rep("QSALL", 6), rep("ECOG01", 6)),
#'   stringsAsFactors = FALSE
#' )
#'
#' check_qs_qsstat_qsstresc(QS)
#'
#' QS$QSSTRESC[4] <- " "
#' QS$QSSTRESC[6] <- NA
#' QS$QSSTRESC[8] <- "."
#' check_qs_qsstat_qsstresc(QS)
#'
#' QS$QSSTRESC <- NULL
#' check_qs_qsstat_qsstresc(QS)
#'
check_qs_qsstat_qsstresc <- function(QS) {
  has_qscat <- "QSCAT" %in% names(QS)

  req <- if (has_qscat) {
    c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSCAT", "QSDTC", "QSTESTCD")
  } else {
    c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSDTC", "QSTESTCD")
  }

  ### First check that required variables exist and return a message if they don't
  if (QS %lacks_any% req) {
    fail(lacks_msg(QS, req))
  } else {
    sel_all <- if (has_qscat) {
      c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSCAT", "QSDTC", "QSTESTCD")
    } else {
      c("USUBJID", "QSSTRESC", "VISIT", "QSSTAT", "QSDTC", "QSTESTCD")
    }

    merge_by <- if (has_qscat) {
      c("USUBJID", "VISIT", "QSCAT", "QSDTC", "QSTESTCD")
    } else {
      c("USUBJID", "VISIT", "QSDTC", "QSTESTCD")
    }

    sel_nd <- if (has_qscat) {
      c("USUBJID", "VISIT", "QSSTAT", "QSCAT", "QSDTC", "QSTESTCD")
    } else {
      c("USUBJID", "VISIT", "QSSTAT", "QSDTC", "QSTESTCD")
    }

    sel_ans <- if (has_qscat) {
      c("USUBJID", "VISIT", "QSSTRESC", "QSCAT", "QSDTC", "QSTESTCD")
    } else {
      c("USUBJID", "VISIT", "QSSTRESC", "QSDTC", "QSTESTCD")
    }

    sel_out <- if (has_qscat) {
      c("USUBJID", "VISIT", "QSCAT", "QSDTC", "QSSTAT", "QSSTRESC", "QSTESTCD")
    } else {
      c("USUBJID", "VISIT", "QSDTC", "QSSTAT", "QSSTRESC", "QSTESTCD")
    }

    # in QS keep rows where QSSTAT = 未查 or QSTESTCD = QSALL
    qsND <- QS %>%
      filter(QSSTAT == "未查" | QSTESTCD == "QSALL") %>%
      select(all_of(sel_all))

    qsANS <- QS %>%
      select(all_of(sel_all)) %>%
      filter(!is_sas_na(QSSTRESC))

    qsNDsub <- qsND %>% select(all_of(sel_nd))
    qsANSsub <- qsANS %>% select(all_of(sel_ans))

    qsPREP <- merge(qsNDsub, qsANSsub, merge_by, all.x = TRUE)

    mydf <- qsPREP %>%
      filter((QSSTAT == "未查" & !is_sas_na(QSSTRESC)) | QSTESTCD == "QSALL") %>%
      select(all_of(sel_out))

    mydf <- unique(mydf)
    rownames(mydf) <- NULL

    ### Print to report

    ### Return message if no records with issue in QS
    if (nrow(mydf) == 0) {
      pass()

      ### Return subset dataframe if there are issues in QS with NOT DONE but results
    } else if (nrow(mydf) > 0) {
      fail(
        paste0("There are non-missing QSSTRESC records for the following ",
          "visits when QSSTAT=未查 or QSTESTCD=QSALL. ",
          sep = " "
        ),
        mydf
      )
    }
  }
}
