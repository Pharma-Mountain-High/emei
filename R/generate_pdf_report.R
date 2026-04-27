#' Generate a PDF report from sdtmchecks results
#'
#' This function takes the output of \code{Emei::run_all_checks()} and
#' renders it into a landscape-oriented PDF report using the Emei.Rmd
#' template.
#'
#' @param all_rec      List. Check results as returned by
#'                     \code{sdtmchecks::run_all_checks()}, or loaded from an
#'                     .rds file previously saved by the Shiny app.
#' @param output_file  Character. Path for the output PDF file. If only a
#'                     filename is given it will be created in the current
#'                     working directory.
#' @param mystudy      Character. Study identifier displayed in the report
#'                     header (default: \code{"Unknown"}).
#' @param category     Character vector. Check categories to include, e.g.
#'                     \code{c("ALL", "ONC", "PRO")}.
#'                     Defaults to all categories.
#' @param priority     Character vector. Check priorities to include, e.g.
#'                     \code{c("High", "Medium", "Low")}. Defaults to all.
#' @param data_path    Character. The SDTMv data path that was used when the
#'                     checks were run (shown in the report header).
#' @param server_name  Character. Server display name for the report header
#'                     (default: \code{"Local"}).
#' @param rmd_path     Character. Path to the \code{Emei.Rmd} template.
#'                     By default it looks for the file in the same directory
#'                     as this script.
#'
#' @return Invisibly returns the path to the generated PDF file.
#' @import kableExtra knitr shiny
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom shiny isRunning
#'
#'
#'
#'
generate_pdf_report <- function(all_rec,
                                output_file  = "sdtmchecks_report.pdf",
                                mystudy      = "Unknown",
                                category     = c("ALL", "ONC", "PRO"),
                                priority     = c("High", "Medium", "Low"),
                                data_path    = getwd(),
                                server_name  = "Local",
                                rmd_path     = NULL) {

  # --- Validate inputs --------------------------------------------------------
  if (!is.list(all_rec) || length(all_rec) == 0) {
    stop("'all_rec' must be a non-empty list of check results ",
         "(as returned by Emei::run_all_checks()).")
  }

  # --- Locate the Rmd template ------------------------------------------------
  if (is.null(rmd_path)) {
    # Try to find Emei.Rmd relative to this script's location,
    # falling back to the current working directory.
    script_dir <- tryCatch(
      dirname(sys.frame(1)$ofile),
      error = function(e) getwd()
    )
    rmd_path <- file.path(script_dir, "Emei.Rmd")
    if (!file.exists(rmd_path)) {
      rmd_path <- file.path(getwd(), "Emei.Rmd")
    }
  }
  if (!file.exists(rmd_path)) {
    stop("Cannot find Emei.Rmd template at: ", rmd_path,
         "\nPlease provide the correct path via the 'rmd_path' argument.")
  }

  # --- Prepare check metadata (metads) in the global environment --------------
  # The Rmd template reads `metads` from its parent (global) environment to
  # look up pdf_title and pdf_subtitle for each check.  We replicate the same
  # subsetting logic used in app.R lines 619-625.
  metads <- sdtmchecksmeta
  if ("fxn_in_qilu" %in% names(metads)) {
    metads <- metads %>% mutate(fxn_in = fxn_in_qilu)
  }
  metads <- metads %>%
    filter(category %in% !!category & priority %in% !!priority)
  # assign("metads", metads, envir = globalenv())

  # --- Ensure `nickname` is accessible in the global environment --------------
  # `nickname` is a data object exported by the sdtmchecks package.  Loading
  # the package makes it available, but we explicitly place it in the global
  # env so the Rmd template's new.env(parent = globalenv()) can find it.
  # if (exists("nickname", where = asNamespace("sdtmchecks"))) {
  #   assign("nickname", get("nickname", envir = asNamespace("sdtmchecks")),
  #          envir = globalenv())
  # } else {
  #   assign("nickname", "", envir = globalenv())
  # }

  # --- Build display strings for categories and priority ----------------------
  mycat_display <- toupper(paste(category, collapse = ", "))
  mycat_display <- gsub("ALL", "Cross TA", mycat_display)
  mypriority_display <- paste(priority, collapse = ", ")

  # --- Copy Rmd template to a temp directory ----------------------------------
  # This avoids write-permission issues in the project directory and mirrors
  # the approach used in the Shiny app (app.R lines 634-635).
  temp_rmd <- file.path(tempdir(), "Emei.Rmd")
  file.copy(rmd_path, temp_rmd, overwrite = TRUE)

  # --- Assemble the params list expected by the Rmd template ------------------
  params <- list(
    all_rec     = all_rec,
    data.path   = trimws(data_path),
    checks.path = file.path(getwd(), "checks"),
    mystudy     = trimws(mystudy),
    mycat       = trimws(mycat_display),
    mypriority  = trimws(mypriority_display),
    myserver    = server_name,
    cwd         = getwd()
  )

  # --- Resolve the output file to an absolute path ----------------------------
  if (!grepl("^([A-Za-z]:|/|\\\\)", output_file)) {
    output_file <- file.path(getwd(), output_file)
  }

  # --- Render the PDF ---------------------------------------------------------
  message("Rendering PDF report to: ", output_file)

  render_env <- new.env(parent = globalenv())
  render_env$metads <- metads

  if (exists("nickname", where = asNamespace("Emei"))) {
    render_env$nickname <- get("nickname", envir = asNamespace("Emei"))
  }

  rmarkdown::render(
    input       = temp_rmd,
    output_file = output_file,
    params      = params,
    envir       = render_env
  )

  # rmarkdown::render(
  #   input       = temp_rmd,
  #   output_file = output_file,
  #   params      = params,
  #   envir       = new.env(parent = globalenv())
  # )

  message("PDF report generated successfully: ", output_file)
  invisible(output_file)
}


# =============================================================================
# Example usage (uncomment and adapt to run)
# =============================================================================
#
# all_rec <- readRDS("path/to/sdtmchecks_results.rds")
# generate_pdf_report(
#   all_rec     = all_rec,
#   output_file = paste0("sdtmchecks_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"),
#   mystudy     = "GO12345",
#   category    = c("ALL", "ONC"),
#   priority    = c("High", "Medium"),
#   data_path   = "/path/to/sdtmv/data",
#   server_name = "Local"
# )
