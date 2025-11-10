#' @title Save report as an xlsx file
#'
#' @param res results list created by run_all_checks
#' @param outfile file path/name to write to
#' @param extrastring optionally display extra info alongside version info,
#'  e.g. diff info
#'
#' @import openxlsx
#' @importFrom utils packageDescription
#' @importFrom tidyselect any_of
#'
#' @return xlsx file
#' @export
#'
#' @family ex_rpt
#' @keywords internal ex_rpt
#'
#' @examples
#'
#' # Create Dummy data
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
#' cm <- data.frame(
#'   USUBJID = 1:5,
#'   DOMAIN = rep("CM", 5),
#'   CMTRT = rep("DRUG TERM", 5),
#'   CMDECOD = rep("CODED DRUG TERM", 5),
#'   CMSTDTC = 1:5,
#'   CMENDTC = 1:5,
#'   CMCAT = "CONCOMITANT MEDICATIONS",
#'   CMSPID = c(
#'     "FORMNAME-R:13/L:13XXXX",
#'     "FORMNAME-R:16/L:16XXXX",
#'     "FORMNAME-R:2/L:2XXXX",
#'     "FORMNAME-R:19/L:19XXXX",
#'     "FORMNAME-R:5/L:5XXXX"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' data(sdtmchecksmeta)
#' res <- run_all_checks(verbose = FALSE)
#' data(sdtmchecksmeta)
#' fileName <- file.path(tempdir(), "check_results.xlsx")
#' report_to_xlsx(res = res, outfile = fileName)
#'
report_to_xlsx <- function(res,
                           outfile,
                           nickname =
                             utils::packageDescription("Emei")[["Version"]],
                           extrastring = "") {
  # prepare summary page
  # pull columns (xls_title, pdf_title, nrec, notes) from the list and create
  # a summary data frame
  summary_cols <- lapply(res, "[", c(
    "xls_title", "pdf_title", "nrec", "notes",
    "pdf_subtitle"
  ))
  summary_data_0 <- as.data.frame(do.call(rbind, summary_cols))
  summary_data <- summary_data_0 %>%
    mutate(version = "") %>%
    select(-any_of("pdf_subtitle"))
  summary_data[, "nrec"] <- as.numeric(summary_data[, "nrec"])
  summary_data[1, "version"] <- nickname
  summary_data[2, "version"] <- extrastring

  # assign column names（当前包名与版本，不依赖 sdtmchecks）
  pkg_name <- tryCatch(
    {
      utils::packageName()
    },
    error = function(e) NA_character_
  )
  if (is.null(pkg_name) || is.na(pkg_name)) pkg_name <- "Emei"
  pkg_ver <- tryCatch(
    {
      utils::packageDescription(pkg_name)[["Version"]]
    },
    error = function(e) "unknown"
  )

  colnames(summary_data) <- c(
    "Data check (Tab name)",
    "Description",
    "N of Failed records",
    "Notes",
    paste0(pkg_name, " v.", pkg_ver)
  )


  # create workbook
  wb <- createWorkbook()

  # add some formatting to summary page
  addWorksheet(wb, "Summary results")

  setColWidths(wb, "Summary results", cols = 1, widths = 30)
  setColWidths(wb, "Summary results", cols = 2, widths = 65)
  setColWidths(wb, "Summary results", cols = 3, widths = 20)
  setColWidths(wb, "Summary results", cols = 4, widths = 35)
  setColWidths(wb, "Summary results", cols = 5, widths = 25)

  addFilter(wb, "Summary results", cols = 1:ncol(summary_data), rows = 1)

  # write summary data on the 1st page of XLS file
  writeData(
    wb, "Summary results",
    as.data.frame(summary_data),
    startRow = 1,
    startCol = 1,
    headerStyle = createStyle(textDecoration = "bold")
  )

  # Highlight the rows with problematic queries
  # ( i.e. have non-missing comments at column D)
  redStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  orangeStyle <- createStyle(fontColour = "#000000", bgFill = "#fac966")
  boldnickname <- createStyle(textDecoration = "bold")

  conditionalFormatting(
    wb, "Summary results",
    cols = 1:4, rows = 1:nrow(summary_data) + 1,
    rule = '$D2!=" "', style = redStyle
  )
  conditionalFormatting(
    wb, "Summary results",
    cols = 2:4, rows = 1:nrow(summary_data) + 1,
    rule = "$C2>0", style = orangeStyle
  )
  conditionalFormatting(
    wb, "Summary results",
    cols = 1, rows = 1:nrow(summary_data) + 1,
    rule = "$C2>0", style = orangeStyle
  )
  conditionalFormatting(
    wb, "Summary results",
    cols = 5, rows = 1:3,
    rule = '!=""', style = boldnickname
  )

  # Add comments with PDF subtitles to summary results page
  for (i in 1:nrow(summary_data_0)) {
    writeComment(
      wb, "Summary results",
      col = 2, row = i + 1,
      comment = createComment(
        unlist(summary_data_0[i, "pdf_subtitle"]),
        author = "sdtmchecks",
        visible = FALSE,
        width = 2,
        height = 4
      )
    )
  }

  # loop through the data checks results and write them into separated sheet
  # in xls file.
  for (i in 1:length(res)) {
    # do not create xls sheet for data checks with 0 results
    if (res[[i]]$nrec != 0) {
      addWorksheet(wb, res[[i]]$xls_title)


      # Begin writing individual xls tab at row 2.
      # Row=1 will be used to create a HYPERLINK back to 'Summary results'
      # sheet. writeData(wb, res[[i]]$xls_title, as.data.frame(res[[i]]$data),
      # startRow = 2, startCol = 1)
      writeData(wb, res[[i]]$xls_title, as.data.frame(res[[i]]$data),
        startRow = 1, startCol = 1
      )

      # create a HYPERLINK between a row on 'Summary results'
      # sheet and individual tab
      # need to have i+1 because the 1st row on 'Summary results' sheet
      # has column names

      # writeFormula(wb, sheet="Summary results", startRow=i+1, startCol=1,
      #              x=makeHyperlinkString(sheet=res[[i]]$xls_title, row=1,
      # col=1, text=res[[i]]$xls_title ))

      writeData(
        wb,
        sheet = "Summary results",
        startRow = i + 1,
        startCol = 1,
        x = res[[i]]$xls_title
      )


      # create a HYPERLINK between an individual tab and the row on
      # 'Summary results' sheet need to have i+1 because the 1st row on
      # 'Summary results' sheet has column names
      # writeFormula(wb, sheet=res[[i]]$xls_title, startRow=1,
      #              x=makeHyperlinkString(sheet="Summary results",
      # row=i+1, col=5, text="Link to Summary Tab" ))

      # Add comments with PDF sub titles to each individual page
      writeComment(
        wb, res[[i]]$xls_title,
        col = 1, row = 1,
        comment = createComment(
          unlist(summary_data_0[i, "pdf_subtitle"]),
          author = "sdtmchecks",
          visible = FALSE,
          width = 2,
          height = 4
        )
      )
    } # end of if
  } # end of loop

  saveWorkbook(wb, file = outfile, overwrite = TRUE)
  return(invisible())
}

#' @title Create a sdtmchecks list object with column indicating whether
#'  the issue was previously seen
#'
#' @description This report will identify flagged records from an sdtmchecks
#' report that are "new" and those that are "old" for a study. This will help
#' quickly target newly emergent issues that may require a new query or
#' investigation while indicating issues that were encountered from a prior
#' report and may have already been queried. This `diff_reports()` function
#' requires a newer and older set of results from `Emei::run_all_checks()`
#' , which will generate a list of check results. An added column "Status" is
#' created with values of "NEW" and "OLD" in the list of check results,
#' flagging whether a given record that is present in the new result
#'  (ie `new_report`) is also present in the old result (ie `old_report`).
#' It makes a difference which report is defined as "new" and "old".
#' This code only keeps results flagged in the new report and drops
#' old results not in the new report because they were presumably resolved.
#'
#' @param old_report an older sdtmchecks list object as created
#' by `run_all_checks`
#' @param new_report a newer sdtmchecks list object as created
#' by `run_all_checks`
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
#' data(sdtmchecksmeta)
#' metads <- sdtmchecksmeta[sdtmchecksmeta$check == "check_ae_aedecod", ]
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
#' ## optionally output results as spreadsheet with Emei::report_to_xlsx()
#' # report_to_xlsx(res, outfile=paste0("reports/sdtmchecks_diff_",
#' # Sys.Date(),".xlsx"))
#'
#' @keywords ex_rpt
#' @family ex_rpt
#'

diff_reports <- function(old_report, new_report) {
  # it makes a difference which report is defined as "new_report" and
  # "old_report" this code only keeps results flagged in the new report
  # it ignore old results not in new report (because they were resolved)

  if (!is.list(old_report) | !is.list(new_report)) {
    stop("Inputs are expected to be lists as created by Emei::run_all_checks")
  } else {
    ###
    # First: subset to only results with flagged issues in the new report
    ###

    new_issues <- sapply(names(new_report), function(check_name) {
      if ("data" %in% names(new_report[[check_name]])) {
        # if the check has a "data" attributes
        if (nrow(new_report[[check_name]]$data) > 0) {
          # TRUE if data has any records
          TRUE
        } else { # FALSE if data exists but no records
          FALSE
        }
      } else { # FALSE if no data attributes
        FALSE
      }
    }, USE.NAMES = TRUE)

    new_issues <- names(new_issues[new_issues == TRUE])
    # filter to just flagged records
    new_report <- new_report[new_issues]
    # subset new report to just flagged records

    ### -------------------------
    # Second: Do the diff
    #    i.e., Compare the flagged records in the new vs. old report.
    #          A new column "Status" will be added to all results of the
    #          "new_report" based on the flagged record comparison.
    #          The new column will have either "NEW" or "OLD" populated.
    ### -------------------------
    res <- sapply(new_issues, function(check_name) {
      if (!(check_name %in% names(old_report))) {
        # if check not in old report then these issues are new

        res_new <- new_report[[check_name]]
        res_new$data$Status <- "NEW"
        res_new
      } else if (nrow(old_report[[check_name]]$data) == 0) {
        # if check in the old report but old report didn't have any issues
        # then these issues are new

        res_new <- new_report[[check_name]]
        res_new$data$Status <- "NEW"
        res_new
      } else { # else both old and new report have some issues flagged,
        # so we diff them

        res_new <- new_report[[check_name]]
        res_old <- old_report[[check_name]]
        res_old$data$Status <- "OLD"

        res_new$data <- res_new$data %>%
          left_join(res_old$data, relationship = "many-to-many") %>%
          # behold the magic of dplyr automatically identifying columns
          # to join on
          mutate(Status = ifelse(is.na(Status), "NEW", Status))

        res_new
      }
    }, USE.NAMES = TRUE, simplify = FALSE)

    return(res)
  }
}
