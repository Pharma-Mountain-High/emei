#' @title 导出 sdtmchecksmeta 元数据到 Excel 文件
#'
#' @description
#' 将包内置的 `sdtmchecksmeta` 元数据导出为 Excel 文件，方便查看和编辑。
#' 支持按类别（category）和优先级（priority）筛选，并可添加格式化样式。
#'
#' @param outfile 字符串，输出文件路径。默认为当前工作目录下的 `"sdtmchecksmeta.xlsx"`。
#' @param filter_category 字符向量，可选。按 category 筛选，如 `c("ALL", "ONC")`。
#'   默认 `NULL` 表示不筛选，导出所有类别。
#' @param filter_priority 字符向量，可选。按 priority 筛选，如 `c("High", "Medium")`。
#'   默认 `NULL` 表示不筛选，导出所有优先级。
#' @param add_formatting 逻辑值，是否添加 Excel 格式化（表头加粗、自动列宽、筛选器等）。
#'   默认 `TRUE`。
#' @param verbose 逻辑值，是否打印详细信息。默认 `TRUE`。
#'
#' @return 不可见地返回导出的数据框。成功时会打印消息显示导出的文件路径和记录数。
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
#' # 示例 1：导出所有元数据到默认文件
#' \dontrun{
#' export_metadata()
#' }
#'
#' # 示例 2：导出到指定路径
#' \dontrun{
#' export_metadata(
#'     outfile = "~/my_checks_metadata.xlsx"
#' )
#' }
#'
#' # 示例 3：仅导出 ALL 类别的高优先级检查
#' \dontrun{
#' export_metadata(
#'     outfile = "high_priority_checks.xlsx",
#'     filter_category = "ALL",
#'     filter_priority = "High"
#' )
#' }
#'
#' # 示例 4：导出多个类别和优先级，不添加格式化
#' \dontrun{
#' export_metadata(
#'     outfile = "selected_checks.xlsx",
#'     filter_category = c("ALL", "ONC", "PRO"),
#'     filter_priority = c("High", "Medium"),
#'     add_formatting = FALSE
#' )
#' }
#'
#' # 示例 5：在临时目录导出（用于测试）
#' temp_file <- file.path(tempdir(), "test_metadata.xlsx")
#' export_metadata(
#'     outfile = temp_file,
#'     verbose = TRUE
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
