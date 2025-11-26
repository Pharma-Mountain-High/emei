#' @title One-Click SDTM Data Check Execution (Merge SUPP, Preprocessing, Generate Report)
#'
#' @description
#' Reads SDTM `.sas7bdat` files from a specified directory, automatically merges SUPP datasets
#' for each domain, performs lightweight standardized preprocessing on `dm`, `ae`, and `vs`
#' according to established rules, then calls `run_all_checks()` to execute comprehensive
#' data checks. Optionally exports check results to an Excel file (filename consistent with
#' command-line tools).
#'
#' @param proj Character string, required. Project number used for output file naming (e.g., `QLG2198_301`).
#' @param folder Character string, required. SDTM data directory. The function will read all
#' `.sas7bdat` files in the directory.
#' @param priority Character vector or single comma-separated string. Default `c("High","Medium","Low")`.
#'   If a single string is provided (e.g., "High,Medium"), it will be automatically split,
#'   deduplicated, and ordered.
#' @param type Character vector or single comma-separated string. Default `c("ALL","ONC","PRO")`.
#'   If a single string is provided (e.g., "ALL,ONC"), it will be automatically split,
#'   deduplicated, and ordered.
#' @param export_excel Logical value, whether to export Excel report. Default `TRUE`.
#' @param outdir Character string, report output directory. Default `"report"`; will attempt
#'   to create if it doesn't exist.
#' @param save_rds Logical value, whether to additionally save the read source data list as
#'  \code{source_data_\{proj\}.rds}. Default \code{FALSE}.
#' @param verbose Logical value, whether to output runtime information. Default `TRUE`.
#'
#' @return Returns the result object from `run_all_checks()`. If `export_excel = TRUE`,
#'   the returned object will have an `outfile` attribute containing the full path to
#'   the generated Excel file.
#'
#' @examples
#' \dontrun{
#' # Minimal example: specify project number and SDTM directory
#' res <- emei(
#'   proj = "QLG2198_301",
#'   folder = "~/development/Projects02/QLG2198/
#'   QLG2198-301/SP/ole_csr/data/sdtm"
#' )
#'
#' # Custom priority/type (supports vector or comma-separated string)
#' res <- emei(
#'   proj = "QL1706_308",
#'   folder = "~/development/Projects02/QL1706-308/SP/dryrun_ph2/data/sdtm",
#'   priority = c("High", "Medium"),
#'   type = "ALL,ONC"
#' )
#'
#' # Return object only, no Excel export; also save source data RDS
#' res <- emei(
#'   proj = "QL1706_308",
#'   folder = "~/development/Projects02/QL1706-308/SP/dryrun_ph2/data/sdtm",
#'   export_excel = FALSE,
#'   save_rds = TRUE
#' )
#' }
#' @export
emei <- function(proj,
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
