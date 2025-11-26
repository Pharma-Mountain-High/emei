#' @title Export sdtmchecksmeta Metadata to Excel File
#'
#' @description
#' Exports the built-in `sdtmchecksmeta` metadata to an Excel file for easy viewing and editing.
#' Supports filtering by category and priority, and can add formatting styles.
#'
#' @param outfile Character string, output file path. Defaults to `"sdtmchecksmeta.xlsx"` in
#'   the current working directory.
#' @param filter_category Character vector, optional. Filter by category, e.g., `c("ALL", "ONC")`.
#'   Default `NULL` means no filtering, export all categories.
#' @param filter_priority Character vector, optional. Filter by priority, e.g., `c("High", "Medium")`.
#'   Default `NULL` means no filtering, export all priorities.
#' @param add_formatting Logical value, whether to add Excel formatting (bold headers, auto column
#'   width, filters, etc.). Default `TRUE`.
#' @param verbose Logical value, whether to print detailed information. Default `TRUE`.
#'
#' @return Invisibly returns the exported data frame. On success, prints a message showing the
#'   exported file path and number of records.
#'
#' @export
#'
#' @import openxlsx
#' @importFrom utils data
#' @family utils_rpt
#' @keywords utils_rpt
#'
#' @examples
#'
#' # Example 1: Export all metadata to default file
#' \dontrun{
#' export_metadata()
#' }
#'
#' # Example 2: Export to specified path
#' \dontrun{
#' export_metadata(
#'   outfile = "~/my_checks_metadata.xlsx"
#' )
#' }
#'
#' # Example 3: Export only high-priority checks for ALL category
#' \dontrun{
#' export_metadata(
#'   outfile = "high_priority_checks.xlsx",
#'   filter_category = "ALL",
#'   filter_priority = "High"
#' )
#' }
#'
#' # Example 4: Export multiple categories and priorities without formatting
#' \dontrun{
#' export_metadata(
#'   outfile = "selected_checks.xlsx",
#'   filter_category = c("ALL", "ONC", "PRO"),
#'   filter_priority = c("High", "Medium"),
#'   add_formatting = FALSE
#' )
#' }
#'
#' # Example 5: Export to temporary directory (for testing)
#' temp_file <- file.path(tempdir(), "test_metadata.xlsx")
#' export_metadata(
#'   outfile = temp_file,
#'   verbose = TRUE
#' )
#'
export_metadata <- function(outfile = "sdtmchecksmeta.xlsx",
                            filter_category = NULL,
                            filter_priority = NULL,
                            add_formatting = TRUE,
                            verbose = TRUE) {
  # Parameter validation
  if (!is.character(outfile) || length(outfile) != 1 || nchar(outfile) == 0) {
    stop("Parameter 'outfile' must be a non-empty character string.")
  }

  if (!is.logical(add_formatting) || length(add_formatting) != 1) {
    stop("Parameter 'add_formatting' must be a logical scalar.")
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("Parameter 'verbose' must be a logical scalar.")
  }

  # Load metadata
  if (verbose) message("Loading sdtmchecksmeta metadata...")
  data(sdtmchecksmeta, envir = environment())

  # Copy data to avoid modifying original object
  meta_data <- sdtmchecksmeta
  total_count <- nrow(meta_data)

  if (verbose) message(sprintf("Total metadata records: %d", total_count))

  # Apply filters
  if (!is.null(filter_category)) {
    if (!is.character(filter_category)) {
      stop("Parameter 'filter_category' must be a character vector.")
    }
    meta_data <- meta_data[meta_data$category %in% filter_category, ]
    if (verbose) {
      message(sprintf(
        "Filtered by category: %s -> %d record(s)",
        paste(filter_category, collapse = ", "),
        nrow(meta_data)
      ))
    }
  }

  if (!is.null(filter_priority)) {
    if (!is.character(filter_priority)) {
      stop("Parameter 'filter_priority' must be a character vector.")
    }
    meta_data <- meta_data[meta_data$priority %in% filter_priority, ]
    if (verbose) {
      message(sprintf(
        "Filtered by priority: %s -> %d record(s)",
        paste(filter_priority, collapse = ", "),
        nrow(meta_data)
      ))
    }
  }

  # If no filters, prompt that all data will be exported
  if (is.null(filter_category) && is.null(filter_priority) && verbose) {
    message("No filter specified, exporting all metadata")
  }

  # Check if there is data
  if (nrow(meta_data) == 0) {
    stop("No data to export after filtering. Please check filter_category and filter_priority parameters.")
  }

  # Create output directory (if it doesn't exist)
  outdir <- dirname(outfile)
  if (!dir.exists(outdir) && outdir != ".") {
    if (verbose) message(sprintf("Creating output directory: %s", outdir))
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create workbook
  if (verbose) message(sprintf("Creating Excel file: %s", outfile))
  wb <- createWorkbook()

  # Add worksheet
  addWorksheet(wb, "sdtmchecksmeta")

  # Write data
  writeData(wb, "sdtmchecksmeta", meta_data, startRow = 1, startCol = 1)

  # Add formatting
  if (add_formatting) {
    # Header style (bold)
    headerStyle <- createStyle(
      textDecoration = "bold",
      fontSize = 11,
      halign = "center",
      valign = "center",
      border = "TopBottomLeftRight",
      borderColour = "#000000"
    )

    addStyle(wb, "sdtmchecksmeta",
             style = headerStyle,
             rows = 1,
             cols = seq_len(ncol(meta_data)),
             gridExpand = TRUE
    )

    # Auto column width
    setColWidths(wb, "sdtmchecksmeta",
                 cols = seq_len(ncol(meta_data)),
                 widths = "auto"
    )

    # Add filters
    addFilter(wb, "sdtmchecksmeta",
              rows = 1,
              cols = seq_len(ncol(meta_data))
    )

    # Freeze first row
    freezePane(wb, "sdtmchecksmeta", firstRow = TRUE)

    if (verbose) message("Formatting styles added (bold headers, auto column width, filters, freeze first row)")
  }

  # Save workbook
  saveWorkbook(wb, file = outfile, overwrite = TRUE)

  # Success message
  if (verbose) {
    message(sprintf(
      "Successfully exported %d record(s) to: %s",
      nrow(meta_data),
      normalizePath(outfile, winslash = "/", mustWork = FALSE)
    ))
  }

  # Return data (invisible)
  return(invisible(meta_data))
}
