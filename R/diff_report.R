#' @title Create a sdtmchecks list object with column indicating whether
#'  the issue was previously seen
#'
#' @description This report will identify flagged records from an sdtmchecks
#' report that are "new" and those that are "old" for a study. This will help
#' quickly target newly emergent issues that may require a new query or
#' investigation while indicating issues that were encountered from a prior
#' report and may have already been queried.
#'
#' @param old_report an older sdtmchecks list object as created by run_all_checks
#' @param new_report a newer sdtmchecks list object as created by run_all_checks
#'
#' @return list of sdtmchecks results based on new_report with Status indicator
#'
#' @importFrom dplyr %>% left_join mutate
#' @export
#'
#' @examples
#'
#' # Step 1: Simulate an older AE dataset with one missing preferred term
#' # Load the metadata
#' data(sdtmchecksmeta)
#'
#' ae <- data.frame(
#'   USUBJID = 1:5,
#'   DOMAIN = c(rep("AE", 5)),
#'   AESEQ = 1:5,
#'   AESTDTC = 1:5,
#'   AETERM = 1:5,
#'   AEDECOD = 1:5,
#'   AESPID = c(
#'     "FORMNAME-R:13/L:13XXXX",
#'     "FORMNAME-R:16/L:16XXXX",
#'     "FORMNAME-R:2/L:2XXXX",
#'     "FORMNAME-R:19/L:19XXXX",
#'     "FORMNAME-R:5/L:5XXXX"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' ae$AEDECOD[1] <- NA
#'
#' # Step 2: Use the run_all_checks() function to generate list of check
#' # results on this "old" data
#'
#' # Filter sdtmchecksmeta so that only one check is present
#' metads <- subset(sdtmchecksmeta, check == "check_ae_aedecod")
#' old <- run_all_checks(metads = metads)
#'
#'
#'
#' # Step 3: Simulate a newer, updated AE dataset with another record with a
#' # new missing preferred term
#'
#' ae <- data.frame(
#'   USUBJID = 1:6,
#'   DOMAIN = c(rep("AE", 6)),
#'   AESEQ = 1:6,
#'   AESTDTC = 1:6,
#'   AETERM = 1:6,
#'   AEDECOD = 1:6,
#'   AESPID = c(
#'     "FORMNAME-R:13/L:13XXXX",
#'     "FORMNAME-R:16/L:16XXXX",
#'     "FORMNAME-R:2/L:2XXXX",
#'     "FORMNAME-R:19/L:19XXXX",
#'     "FORMNAME-R:5/L:5XXXX",
#'     "FORMNAME-R:1/L:5XXXX"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' ae$AEDECOD[1] <- NA
#' ae$AEDECOD[6] <- NA
#'
#' # Step 4: use the run_all_checks() function to generate list of check results
#' #  on this "new" data
#'
#' new <- run_all_checks(metads = metads)
#'
#' # Step 5: Diff to create a column indicating if the finding is new
#' res <- diff_reports(old_report = old, new_report = new)
#'
#' # optionally output results as spreadsheet with report_to_xlsx()
#' report_to_xlsx(res, outfile = paste0(
#'   "reports/sdtmchecks_diff_",
#'   Sys.Date(), ".xlsx"
#' ))
#'
#' @keywords ex_rpt
#' @family ex_rpt
#'

diff_reports <- function(old_report, new_report) {
  if (!is.list(old_report) | !is.list(new_report)) {
    stop("Inputs are expected to be lists as created by Emei::run_all_checks")
  } else {
    new_issues <- sapply(names(new_report), function(check_name) {
      if ("data" %in% names(new_report[[check_name]])) {
        # if the check has a "data" attributes
        if (nrow(new_report[[check_name]]$data) > 0) {
          # TRUE if data has any records
          TRUE
        } else {
          # FALSE if data exists but no records
          FALSE
        }
      } else {
        # FALSE if no data attributes
        FALSE
      }
    }, USE.NAMES = TRUE)

    new_issues <- names(new_issues[new_issues == TRUE])

    new_report <- new_report[new_issues]

    res <- sapply(new_issues, function(check_name) {
      if (!(check_name %in% names(old_report))) {
        res_new <- new_report[[check_name]]
        res_new$data$Status <- "NEW"
        res_new
      } else if (nrow(old_report[[check_name]]$data) == 0) {
        res_new <- new_report[[check_name]]
        res_new$data$Status <- "NEW"
        res_new
      } else {
        res_new <- new_report[[check_name]]
        res_old <- old_report[[check_name]]
        res_old$data$Status <- "OLD"

        res_new$data <- res_new$data %>%
          left_join(res_old$data, relationship = "many-to-many") %>%
          mutate(Status = ifelse(is.na(Status), "NEW", Status))

        res_new
      }
    }, USE.NAMES = TRUE, simplify = FALSE)

    return(res)
  }
}
