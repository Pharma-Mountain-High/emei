#' Merge SUPP with main SDTM domain; return `data` when SUPP is empty
#'
#' This function merges a supplemental qualifiers dataset (SUPPxx) into its
#' corresponding parent SDTM domain. If `supp` has 0 rows, the original `data`
#' is returned unchanged.
#'
#' Required columns in `supp`:
#' - STUDYID, RDOMAIN, USUBJID, QNAM, QVAL
#' - For non-DM domains also require: IDVAR, IDVARVAL (used to recover SEQ)
#'
#' If `QLABEL` exists in `supp`, variable labels are applied to the new columns.
#'
#' @param data data frame. Parent SDTM domain (e.g., AE, CM, DM, ...)
#' @param supp data frame. Supplemental qualifiers for the domain (e.g., SUPPAE)
#'
#' @return data frame. Merged dataset; or original `data` when `supp` is empty
#'
#' @examples
#' # Example 1: Empty SUPP → returns original data
#' dm <- data.frame(STUDYID = "S1", USUBJID = c("01", "02"), DOMAIN = "DM")
#' suppdm <- dm[0, ] # 0-row SUPPDM
#' merge_supp(dm, suppdm)
#'
#' # Example 2: Non-DM with minimal SUPP
#' ae <- data.frame(
#'   STUDYID = "S1",
#'   USUBJID = c("01", "01"),
#'   DOMAIN = "AE",
#'   AESEQ = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#' suppae <- data.frame(
#'   STUDYID = "S1",
#'   RDOMAIN = "AE",
#'   USUBJID = c("01", "01"),
#'   IDVAR = "AESEQ",
#'   IDVARVAL = c("1", "2"),
#'   QNAM = c("AESOC", "AESEV"),
#'   QVAL = c("SOC1", "MILD"),
#'   stringsAsFactors = FALSE
#' )
#' merge_supp(ae, suppae)
#'
#' @export
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom labelled var_label

merge_supp <- function(data, supp) {
  # Early return if SUPP is empty
  if (is.null(supp) || nrow(supp) == 0) {
    return(data)
  }

  # Detect domain from data with basic sanity checks
  data_domain_vals <- unique(data$DOMAIN)
  data_domain_vals <- data_domain_vals[!is.na(data_domain_vals)]
  if (length(data_domain_vals) != 1) {
    stop(sprintf(
      "`data$DOMAIN` 应为单一取值，当前检测到: %s",
      paste(data_domain_vals, collapse = ", ")
    ))
  }
  data_domain <- data_domain_vals

  # Required columns check on non-empty SUPP
  required_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "QNAM", "QVAL")
  missing_cols <- setdiff(required_cols, names(supp))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "SUPP 缺少关键列: %s；请确保包含列: %s",
      paste(missing_cols, collapse = ", "),
      paste(required_cols, collapse = ", ")
    ))
  }
  if (data_domain != "DM") {
    required_non_dm <- c("IDVAR", "IDVARVAL")
    missing_ndm <- setdiff(required_non_dm, names(supp))
    if (length(missing_ndm) > 0) {
      stop(sprintf(
        "SUPP（非 DM）缺少关键列: %s；请确保包含列: %s",
        paste(missing_ndm, collapse = ", "),
        paste(c(required_cols, required_non_dm), collapse = ", ")
      ))
    }
  }

  # Check domain consistency
  supp_domain_vals <- unique(supp$RDOMAIN)
  supp_domain_vals <- supp_domain_vals[!is.na(supp_domain_vals)]
  if (length(supp_domain_vals) != 1) {
    stop(sprintf(
      "`supp$RDOMAIN` 应为单一取值，当前检测到: %s",
      paste(supp_domain_vals, collapse = ", ")
    ))
  }
  supp_domain <- supp_domain_vals
  if (data_domain != supp_domain) {
    stop(sprintf(
      "域不匹配：主域为 %s，但 SUPP 的 RDOMAIN 为 %s",
      data_domain, supp_domain
    ))
  }

  # Reshape SUPP to wide and prepare for merge
  if (data_domain == "DM") {
    wide_data <- tidyr::pivot_wider(
      data = supp,
      id_cols = tidyselect::all_of(c("STUDYID", "RDOMAIN", "USUBJID")),
      names_from = tidyselect::all_of("QNAM"),
      values_from = tidyselect::all_of("QVAL")
    )
  } else {
    seqvar_vals <- unique(supp$IDVAR)
    seqvar_vals <- seqvar_vals[!is.na(seqvar_vals)]
    if (length(seqvar_vals) != 1) {
      stop(sprintf(
        "`supp$IDVAR` 应为单一取值以确定序列变量，当前检测到: %s",
        paste(seqvar_vals, collapse = ", ")
      ))
    }
    SEQVAR <- seqvar_vals

    wide_data <- tidyr::pivot_wider(
      data = supp,
      id_cols = tidyselect::all_of(c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL")),
      names_from = tidyselect::all_of("QNAM"),
      values_from = tidyselect::all_of("QVAL")
    )
    wide_data[[SEQVAR]] <- as.double(wide_data[["IDVARVAL"]])
    wide_data[["IDVAR"]] <- NULL
    wide_data[["IDVARVAL"]] <- NULL
  }

  # Apply labels when QLABEL exists
  if ("QLABEL" %in% names(supp)) {
    labels_df <- unique(supp[, c("QNAM", "QLABEL")])
    labels_dict <- stats::setNames(labels_df[["QLABEL"]], labels_df[["QNAM"]])
    vars_to_label <- intersect(names(labels_dict), names(wide_data))
    if (length(vars_to_label) > 0) {
      labelled::var_label(wide_data[vars_to_label]) <- labels_dict[vars_to_label]
    }
  }

  # Merge back to parent domain
  if (data_domain == "DM") {
    merge_data <- dplyr::left_join(data, wide_data, by = c("STUDYID", "USUBJID"))
  } else {
    merge_data <- dplyr::left_join(data, wide_data, by = c("STUDYID", "USUBJID", paste0(data_domain, "SEQ")))
  }

  return(merge_data)
}
