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
    if (res[[i]]$nrec != 0) {
      addWorksheet(wb, res[[i]]$xls_title)

      # Row=1: HYPERLINK back to 'Summary results'; data starts at row 2
      writeData(wb, res[[i]]$xls_title, as.data.frame(res[[i]]$data),
                startRow = 2, startCol = 1
      )

      # 汇总页该行 → 独立 Tab 超链接
      # i+1 是因为汇总页第1行为列名
      writeFormula(wb,
                   sheet = "Summary results", startRow = i + 1, startCol = 1,
                   x = makeHyperlinkString(
                     sheet = res[[i]]$xls_title, row = 1,
                     col = 1, text = res[[i]]$xls_title
                   )
      )

      # 独立 Tab 第1行 → 汇总页返回超链接
      writeFormula(wb,
                   sheet = res[[i]]$xls_title, startRow = 1,
                   x = makeHyperlinkString(
                     sheet = "Summary results",
                     row = i + 1, col = 5, text = "Link to Summary Tab"
                   )
      )

      # 各独立页添加 PDF 子标题注释
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
    }
  }

  saveWorkbook(wb, file = outfile, overwrite = TRUE)
  return(invisible())
}
