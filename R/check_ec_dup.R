#' @title Check for duplicate EC records
#'
#' @description This check looks for duplicate treatment records in EC
#'
#' @param EC EC SDTM dataset with variables USUBJID, ECTRT, ECDOSE, ECSTDTC.
#'   VISIT is optional.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author 1
#'
#' @examples
#'
#' # FAIL: duplicate EC records
#' EC <- data.frame(
#'   USUBJID = rep(1, 2),
#'   ECTRT = rep("药物A", 2),
#'   ECDOSE = rep(10, 2),
#'   ECSTDTC = rep("2020-01-01", 2),
#'   ECOCCUR = "是",
#'   stringsAsFactors = FALSE
#' )
#' check_ec_dup(EC)
#'
#' # PASS: no duplicates
#' EC2 <- data.frame(
#'   USUBJID = 1:2,
#'   ECTRT = c("药物A", "药物B"),
#'   ECDOSE = c(10, 20),
#'   ECSTDTC = c("2020-01-01", "2020-01-02"),
#'   ECOCCUR = "是",
#'   stringsAsFactors = FALSE
#' )
#' check_ec_dup(EC2)
#'
#' # FAIL: duplicates created by rbind
#' EC3 <- rbind(EC2, EC2)
#' check_ec_dup(EC3)
#'
#' # missing required variable
#' EC4 <- EC2
#' EC4$ECTRT <- NULL
#' check_ec_dup(EC4)
#'
check_ec_dup <- function(EC) {
  # Keep only records where dosing actually occurred
  if ("ECOCCUR" %in% names(EC)) {
    EC <- EC %>% filter(ECOCCUR == "是")
  }

  ### Check that required variables exist and return a message if they don't
  if (EC %lacks_any% c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC")) {
    fail(lacks_msg(EC, c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC")))
  } else {
    # Keep only key variables for duplicate check
    ec0 <- subset(EC, select = c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC"))

    ec1 <- with(ec0, ec0[order(USUBJID, ECTRT, ECDOSE, ECSTDTC), ])

    # Identify duplicated records
    dups <- subset(ec1, duplicated(ec1), c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC"))
    rownames(dups) <- NULL

    n0 <- paste("There are ", nrow(dups), " duplicated EC records. ", sep = "")

    if (nrow(dups) > 0) {
      # Append VISIT info if available
      if (EC %lacks_any% "VISIT") {
        fail(
          msg = n0,
          data = dups
        )
      } else {
        ec2 <- subset(EC, select = c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC", "VISIT"))
        ec2s <- with(ec2, ec2[order(USUBJID, ECTRT, ECDOSE, ECSTDTC, VISIT), ])

        dupswithvisit <- merge(x = dups, y = ec2s, by = c("USUBJID", "ECTRT", "ECDOSE", "ECSTDTC"), all.x = TRUE)
        dupswithvisit <- unique(dupswithvisit)
        fail(
          msg = n0,
          data = dupswithvisit
        )
      }
    } else if (nrow(dups) == 0) {
      pass()
    }
  }
}
