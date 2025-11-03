# check_ds_sc_strat

#' @title check missing stratification factors in randomized DM
#'
#' @param DM Data frame containing `USUBJID` and, if applicable, randomization
#'   variables named like `RANDC1`, `RANDC2`, ..., and `RANDR1`, `RANDR2`, ...
#' @description Detects randomization/stratification variables in DM whose names
#'   match `RANDC[1..N]` and `RANDR[1..N]`. For randomized studies, flags
#'   subjects where any of these variables are missing. If no such variables are
#'   found in DM, the check returns a failure indicating that it is only
#'   applicable when randomization variables exist.
#'
#' @return Logical scalar. `pass()` returns `TRUE` when no records with missing
#'   stratification factors are found. `fail()` returns `FALSE` and carries
#'   `msg` and `data` attributes with the failure message and the subset of
#'   inconsistent records.
#' @export
#' @importFrom dplyr %>% select group_by mutate filter across all_of if_else
#' @examples
#' # Example: failing case (missing stratification values)
#' DM <- data.frame(
#'   USUBJID = c("01", "02", "03"),
#'   RANDC1 = c("A", NA, "C"),
#'   RANDR1 = c("A", "B", NA),
#'   stringsAsFactors = FALSE
#' )
#' check_dm_strat(DM)
#'
#' # Example: passing case (no missing stratification values)
#' DM2 <- data.frame(
#'   USUBJID = c("01", "02"),
#'   RANDC1 = c("A", "B"),
#'   RANDR1 = c("A", "B"),
#'   stringsAsFactors = FALSE
#' )
#' check_dm_strat(DM2)
#'
check_dm_strat <- function(DM) {
  rand_pattern <- "^RAND[C|R][0-9]+$" # Regex pattern for RANDCN/RANDRN

  # Find all variables matching the randomization pattern
  all_vars <- names(DM)
  rand_vars_found <- grep(rand_pattern, all_vars,
    value = TRUE,
    ignore.case = TRUE
  )
  if (length(rand_vars_found) == 0) {
    fail("Only applicable for randomized studies. Based on DM.RAANDC1, ……,
         DM.RAANDRN, DM.RAANDR1, ……, DM.RAANDRN no records were
         found indicating randomized patients.")
  } else {
    mydf1 <- DM %>%
      select(USUBJID, all_of(rand_vars_found)) %>%
      group_by(USUBJID)
    mydf1 <- mydf1 %>%
      mutate(MISFLAG = if_else(Reduce(`|`, across(
        all_of(rand_vars_found),
        is.na
      )), 1, 0)) %>%
      filter(MISFLAG == 1) %>%
      select(-MISFLAG)




    if ((nrow(mydf1) == 0)) {
      pass()
    } else {
      fail(paste0(length(unique(mydf1$USUBJID)), " patient(s) for randomized
                  study where stratification factors are missing. "), mydf1)
    }
  }
}
