#' @title SDTM 数据检查一键执行（合并 SUPP、预处理、生成报告）
#'
#' @description
#' 从指定目录读取 SDTM `.sas7bdat` 文件，自动合并各域的 SUPP 数据集，按既定规则
#' 对`dm`、`ae`、`vs` 进行轻量标准化预处理，随后调用 `run_all_checks()` 执行
#' 全量数据检查；可选将检查结果导出为 Excel 文件（文件名与命令行工具保持一致）。
#'
#' @param proj 字符串，必填。项目编号，用于输出文件命名（例如 `QLG2198_301`）。
#' @param folder 字符串，必填。SDTM 数据目录，函数会读取目录下所有 `.sas7bdat`
#' 文件。
#' @param priority 字符向量或单个逗号分隔字符串。默认 `c("High","Medium","Low")`
#'   。若传入单个字符串（如 "High,Medium"），会自动拆分、去重并保序。
#' @param type 字符向量或单个逗号分隔字符串。默认 `c("ALL","ONC","PRO")`。
#'   若传入单个字符串（如 "ALL,ONC"），会自动拆分、去重并保序。
#' @param export_excel 逻辑值，是否导出 Excel 报告，默认 `TRUE`。
#' @param outdir 字符串，报告输出目录，默认 `"report"`；不存在将尝试创建。
#' @param save_rds 逻辑值，是否额外保存读取到的源数据列表为
#'  \code{source_data_\{proj\}.rds}，默认 \code{FALSE}。
#' @param verbose 逻辑值，是否输出运行信息，默认 `TRUE`。
#'
#'
#' @return 返回 `run_all_checks()` 的结果对象。若 `export_excel = TRUE`，
#'   将为返回对象附加属性 `outfile`，其值为生成的 Excel 文件的完整路径。
#'
#'
#' @examples
#' \dontrun{
#' # 最小示例：指定项目编号与 SDTM 目录
#' res <- emei(
#'   proj = "QLG2198_301",
#'   folder = "~/development/Projects02/QLG2198/
#'   QLG2198-301/SP/ole_csr/data/sdtm"
#' )
#'
#' # 自定义 priority/type（支持向量或逗号分隔字符串）
#' res <- emei(
#'   proj = "QL1706_308",
#'   folder = "~/development/Projects02/QL1706-308/SP/dryrun_ph2/data/sdtm",
#'   priority = c("High", "Medium"),
#'   type = "ALL,ONC"
#' )
#'
#' # 仅返回对象，不导出 Excel；同时保存源数据 RDS
#' res <- emei(
#'   proj = "QL1706_308",
#'   folder = "~/development/Projects02/QL1706-308/SP/dryrun_ph2/data/sdtm",
#'   export_excel = FALSE,
#'   save_rds = TRUE
#' )
#' }
#' @export
#'
#'
emei <- function(
    proj,
    folder,
    priority = c("High", "Medium", "Low"),
    type = c("ALL", "ONC", "PRO"),
    export_excel = TRUE,
    outdir = "report",
    save_rds = FALSE,
    verbose = TRUE) {
  # 参数校验
  if (missing(proj) || !is.character(proj) || length(proj) != 1 ||
    nchar(proj) == 0) {
    stop("参数 'proj' 必须为非空字符串。")
  }
  if (missing(folder) || !is.character(folder) || length(folder) != 1 ||
    nchar(folder) == 0) {
    stop("参数 'folder' 必须为非空字符串。")
  }
  if (!dir.exists(folder)) {
    stop(sprintf("数据目录不存在：%s", folder))
  }
  if (!is.logical(export_excel) || length(export_excel) != 1) {
    stop("参数 'export_excel' 必须为逻辑标量。")
  }
  if (!is.logical(save_rds) || length(save_rds) != 1) {
    stop("参数 'save_rds' 必须为逻辑标量。")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("参数 'verbose' 必须为逻辑标量。")
  }
  if (!is.character(outdir) || length(outdir) != 1 || nchar(outdir) == 0) {
    stop("参数 'outdir' 必须为非空字符串。")
  }

  # 归一化 priority/type：支持向量或逗号分隔字符串
  normalize_char_opt <- function(x, allowed, case = c("asis", "title")) {
    case <- match.arg(case)
    if (length(x) == 1L && grepl(",", x, fixed = TRUE)) {
      x <- unlist(strsplit(x, ",", fixed = TRUE))
    }
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (case == "title") {
      x <- paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
    }
    # 仅保留允许集，去重保序
    keep <- x[x %in% allowed]
    if (length(keep) == 0L) keep <- allowed
    unique(keep)
  }

  priority <- normalize_char_opt(priority,
    allowed = c("High", "Medium", "Low"),
    case = "title"
  )
  type <- normalize_char_opt(type,
    allowed = c("ALL", "ONC", "PRO"),
    case = "asis"
  )

  # 读取 .sas7bdat
  files <- list.files(folder, pattern = "(?i)\\.sas7bdat$", full.names = TRUE)
  if (length(files) == 0L) {
    stop(sprintf("目录下未找到 .sas7bdat 文件：%s", folder))
  }
  if (verbose) message(sprintf("读取 %d 个 .sas7bdat 文件...", length(files)))

  datasets <- lapply(files, function(f) haven::read_sas(f))
  names(datasets) <- stringr::str_to_lower(
    tools::file_path_sans_ext(basename(files))
  )

  if (isTRUE(save_rds)) {
    rds_path <- file.path(getwd(), sprintf("source_data_%s.rds", proj))
    saveRDS(datasets, file = rds_path)
    if (verbose) message(sprintf("已保存源数据 RDS：%s", rds_path))
  }

  # 合并 SUPP → 主域
  supp_names <- grep("^supp", names(datasets), value = TRUE)
  if (length(supp_names) > 0L) {
    for (supp_name in supp_names) {
      main_name <- sub("^supp", "", supp_name)
      if (!main_name %in% names(datasets)) next
      if (verbose) message(sprintf("合并 SUPP：%s -> %s", supp_name, main_name))
      datasets[[main_name]] <- merge_supp(
        datasets[[main_name]],
        datasets[[supp_name]]
      )
    }
    # 移除所有 supp*
    datasets <- datasets[setdiff(names(datasets), supp_names)]
  }

  # 注入 .GlobalEnv 以兼容 sdtmchecks
  list2env(datasets, envir = .GlobalEnv)

  # 运行检查
  if (verbose) message("执行检查中...")
  metads_obj <- if (exists("sdtmchecksmeta")) {
    get("sdtmchecksmeta")
  } else {
    get("sdtmchecksmeta", envir = asNamespace("sdtmchecks"))
  }
  sdtmreport <- run_all_checks(
    metads = metads_obj,
    priority = priority,
    type = type,
    verbose = verbose
  )

  # 可选导出 Excel
  if (isTRUE(export_excel)) {
    if (!dir.exists(outdir)) {
      ok <- try(dir.create(outdir, recursive = TRUE), silent = TRUE)
      if (inherits(ok, "try-error") || !dir.exists(outdir)) {
        stop(sprintf("无法创建输出目录：%s", outdir))
      }
    }
    outfile <- file.path(outdir, sprintf(
      "%s_sdtm_checks_report_%s.xlsx",
      proj, as.character(Sys.Date())
    ))
    report_to_xlsx(res = sdtmreport, outfile = outfile)
    attr(sdtmreport, "outfile") <- normalizePath(outfile,
      winslash = "/",
      mustWork = FALSE
    )
    if (verbose) message(sprintf("报告已生成：%s", attr(sdtmreport, "outfile")))
  }

  return(sdtmreport)
}
