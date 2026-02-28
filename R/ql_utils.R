#' @title Utility function to display SEQ variable
#'
#' @description This function directly displays the xxSEQ variable value in a SEQ column.
#' The function finds the --SEQ variable from the specified domains and copies its value to the SEQ column.
#'
#' @param dts SDTM dataframe - e.g., AE
#' @param domains domains you wish to identify a xxSEQ variable from
#'
#' @return dataframe with SEQ column
#'
#'
#' @author Hao
#' @export
#' @keywords internal
#' @examples
#'
#' AE <- data.frame(
#'   STUDY = c(rep("1", 6)),
#'   DOMAIN = c(rep("AE", 6)),
#'   USUBJID = c(rep("PT1", 6)),
#'   AESEQ = c(1, 2, 3, 4, 5, 6),
#'   AETERM = rep("AE Raw Term", 6),
#'   AEDECOD = rep("AE Preferred Term", 6),
#'   AESPID = c(
#'     "FORMNAME-R:13/L:13XXXX",
#'     "FORMNAME-R:16/L:16XXXX",
#'     "FORMNAME-R:2/L:2XXXX",
#'     "FORMNAME-R:19/L:19XXXX",
#'     "FORMNAME-R:5/L:5XXXX",
#'     "FORMNAME-R:20/L:20XXXX"
#'   ),
#'   AESTDTC = c(rep("2020-01-01", 6)),
#'   stringsAsFactors = FALSE
#' )
#'
#' ql_derive_seq(AE)
#'
ql_derive_seq <- function(dts, domains = c("ae", "ce", "cm", "ds", "lb", "mh", "pr", "rs", "ss", "tr", "tu")) {
  myvec <- paste0(toupper(unlist(domains)), "SEQ")

  thevar <- intersect(names(dts), myvec) # get --SEQ variable of interest

  if (length(thevar) == 1) { # Only create SEQ column if there is a --SEQ variable

    # Directly display the --SEQ variable value
    dts[["SEQ"]] <- dts[[thevar]]
    attr(dts[["SEQ"]], "label") <- "Sequence Number"

    return(dts)
  } else {
    (
      return(dts)
    )
  }
}
