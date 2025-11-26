#' @title Check consistency of RANDC/RANDR randomization variables in DM
#'
#' @param DM DM Data frame. Must contain `USUBJID` and, if present, randomization
#'   variables like `RANDC[1..N]` and `RANDR[1..N]`.
#' @description automatically detects randomization variables such as `RANDC1`,
#' `RANDR1`, ... in the DM dataset and compares values for each corresponding
#' pair. If any inconsistency is found, the function returns a failure with
#' detailed records.
#' @return  Logical scalar. On success, returns `TRUE` produced by `pass()`; on
#'   failure, returns `FALSE` produced by `fail()` with `msg` and `data`
#'   attributes containing the failure message and a data frame of inconsistent
#'   records.
#' @export
#'
#' @examples
#'
#'
#' DM <- data.frame(
#'   USUBJID = c("01", "02", "03"),
#'   RANDC1 = c("A", "B", "C"),
#'   RANDR1 = c("A", "C", "C"),
#'   stringsAsFactors = FALSE
#' )
#' check_dm_randc_randr(DM)
#'
#'
#' DM2 <- data.frame(
#'   USUBJID = c("01", "02"),
#'   RANDC1 = c("A", "B"),
#'   RANDR1 = c("A", "B"),
#'   stringsAsFactors = FALSE
#' )
#' check_dm_randc_randr(DM2)
#'
check_dm_randc_randr <- function(DM) {
  # Define the expected randomization variable patterns
  rand_pattern <- "^RAND[C|R][0-9]+$" # Regex pattern for RANDCN/RANDRN

  # Find all variables matching the randomization pattern
  all_vars <- names(DM)
  rand_vars_found <- grep(rand_pattern, all_vars, value = TRUE, ignore.case = TRUE)

  # Initialize result list
  result <- list(
    all_randomization_vars = rand_vars_found,
    has_randomization_vars = length(rand_vars_found) > 0,
    message = if (length(rand_vars_found) > 0) {
      "Randomization variables found in DM dataset"
    } else {
      "No randomization variables found matching RANDCN/RANDRN pattern"
    },
    inconsistency_count = 0,
    inconsistent_records = data.frame(
      USUBJID = character(0),
      RANDC_VAR = character(0),
      RANDC_VALUE = character(0),
      RANDR_VAR = character(0),
      RANDR_VALUE = character(0),
      stringsAsFactors = FALSE
    )
  )

  # Check for RANDCN and RANDRN inconsistency if both exist
  randc_vars <- grep("^RANDC[0-9]+$", rand_vars_found, value = TRUE, ignore.case = TRUE)
  randr_vars <- grep("^RANDR[0-9]+$", rand_vars_found, value = TRUE, ignore.case = TRUE)

  if (length(randc_vars) > 0 && length(randr_vars) > 0) {
    # Check for inconsistency between corresponding RANDCN and RANDRN
    for (i in seq_along(randc_vars)) {
      randc_var <- randc_vars[i]
      randr_var <- randr_vars[i]

      if (i > length(randr_vars)) break # In case numbers don't match

      # Find rows where values don't match
      inconsistent_rows <- which(DM[[randc_var]] != DM[[randr_var]])

      if (length(inconsistent_rows) > 0) {
        result$inconsistency_count <- result$inconsistency_count + length(inconsistent_rows)

        # Store inconsistent records in a data frame
        new_records <- data.frame(
          USUBJID = DM$USUBJID[inconsistent_rows],
          RANDC_VAR = randc_var,
          RANDC_VALUE = DM[[randc_var]][inconsistent_rows],
          RANDR_VAR = randr_var,
          RANDR_VALUE = DM[[randr_var]][inconsistent_rows],
          stringsAsFactors = FALSE
        )

        result$inconsistent_records <- rbind(result$inconsistent_records, new_records)
      }
    }
  }

  mydf <- result$inconsistent_records

  if (result$inconsistency_count == 0) {
    pass()
  } else {
    fail(paste("DM has ", nrow(mydf), "  entries where RANDCN  !=  RANDRN. ",
      sep = ""
    ), mydf)
  }

  # return(result)
}
