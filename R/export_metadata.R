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
  # 参数校验
  if (!is.character(outfile) || length(outfile) != 1 || nchar(outfile) == 0) {
    stop("参数 'outfile' 必须为非空字符串。")
  }

  if (!is.logical(add_formatting) || length(add_formatting) != 1) {
    stop("参数 'add_formatting' 必须为逻辑标量。")
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("参数 'verbose' 必须为逻辑标量。")
  }

  # 加载元数据
  if (verbose) message("正在加载 sdtmchecksmeta 元数据...")
  data(sdtmchecksmeta, envir = environment())

  # 复制数据以避免修改原始对象
  meta_data <- sdtmchecksmeta
  total_count <- nrow(meta_data)

  if (verbose) message(sprintf("元数据总记录数：%d", total_count))

  # 应用筛选
  if (!is.null(filter_category)) {
    if (!is.character(filter_category)) {
      stop("参数 'filter_category' 必须为字符向量。")
    }
    meta_data <- meta_data[meta_data$category %in% filter_category, ]
    if (verbose) {
      message(sprintf(
        "按 category 筛选：%s -> %d 条记录",
        paste(filter_category, collapse = ", "),
        nrow(meta_data)
      ))
    }
  }

  if (!is.null(filter_priority)) {
    if (!is.character(filter_priority)) {
      stop("参数 'filter_priority' 必须为字符向量。")
    }
    meta_data <- meta_data[meta_data$priority %in% filter_priority, ]
    if (verbose) {
      message(sprintf(
        "按 priority 筛选：%s -> %d 条记录",
        paste(filter_priority, collapse = ", "),
        nrow(meta_data)
      ))
    }
  }

  # 如果没有筛选，提示正在导出全部数据
  if (is.null(filter_category) && is.null(filter_priority) && verbose) {
    message("未指定筛选条件，将导出全部元数据")
  }

  # 检查是否有数据
  if (nrow(meta_data) == 0) {
    stop("筛选后没有数据可导出。请检查 filter_category 和 filter_priority 参数。")
  }

  # 创建输出目录（如果不存在）
  outdir <- dirname(outfile)
  if (!dir.exists(outdir) && outdir != ".") {
    if (verbose) message(sprintf("创建输出目录：%s", outdir))
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  # 创建工作簿
  if (verbose) message(sprintf("正在创建 Excel 文件：%s", outfile))
  wb <- createWorkbook()

  # 添加工作表
  addWorksheet(wb, "sdtmchecksmeta")

  # 写入数据
  writeData(wb, "sdtmchecksmeta", meta_data, startRow = 1, startCol = 1)

  # 添加格式化
  if (add_formatting) {
    # 表头样式（加粗）
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

    # 自动列宽
    setColWidths(wb, "sdtmchecksmeta",
      cols = seq_len(ncol(meta_data)),
      widths = "auto"
    )

    # 添加筛选器
    addFilter(wb, "sdtmchecksmeta",
      rows = 1,
      cols = seq_len(ncol(meta_data))
    )

    # 冻结首行
    freezePane(wb, "sdtmchecksmeta", firstRow = TRUE)

    if (verbose) message("已添加格式化样式（表头加粗、自动列宽、筛选器、冻结首行）")
  }

  # 保存工作簿
  saveWorkbook(wb, file = outfile, overwrite = TRUE)

  # 成功消息
  if (verbose) {
    message(sprintf(
      "成功导出 %d 条记录到：%s",
      nrow(meta_data),
      normalizePath(outfile, winslash = "/", mustWork = FALSE)
    ))
  }

  # 返回数据（不可见）
  return(invisible(meta_data))
}
