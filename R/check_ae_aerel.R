#' @title Check for AERELx when AEREL is missing and when AEREL is unexpected
#'
#' @description Flag if patient has a record with null value of AEREL
#'              but AERELx (x = 1, 2, ..., or A, B, ...) contain "是"/"否"/"不适用",
#'              so a likely mapping issue,
#'              or if AEREL is missing and there is no AERELx variable,
#'              or if AEREL has unexpected value
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AESEQ, AETERM,
#' AESTDTC, AEREL, AERELx (optional), AESPID (if present)
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#'
#' @author 1
#'
#' @examples
#'
#' AE <- data.frame(
#'   STUDYID = 1001,
#'   USUBJID = c(1, 2, 3, 1, 2, 3),
#'   AESTDTC = rep("2020-05-05", 6),
#'   AETERM = c("abc Covid-19", "covid TEST POSITIVE", "CHILLS"),
#'   AESEQ = c(1, 1, 1, 2, 2, 2),
#'   AEREL = c("是", "否", "不适用", "否", "否", "是"),
#'   AEREL1 = c("是", "否", "不适用", "否", "不适用", "是"),
#'   AEREL2 = c("是", "否", "不适用", "否", "否", "否"),
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aerel(AE)
#'
#' AE1 <- data.frame(
#'   STUDYID = 1001,
#'   USUBJID = c(1, 2, 3, 1, 2, 3),
#'   AESTDTC = rep("2020-05-05", 6),
#'   AETERM = c("abc Covid-19", "covid TEST POSITIVE", "CHILLS"),
#'   AESEQ = c(1, 1, 1, 2, 2, 2),
#'   AEREL = c("是", "否", "否", "否", "否", "否"),
#'   AEREL1 = c("是", "否", "不适用", "否", "否", ""),
#'   AEREL2 = c("是", "否", " ", "否", "否", " "),
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aerel(AE1)
#' check_ae_aerel(AE1, preproc = ql_derive_seq)
#'
#' AE2 <- data.frame(
#'   STUDYID = 1001,
#'   USUBJID = c(1, 2, 3, 1, 2, 3),
#'   AESTDTC = rep("2020-05-05", 6),
#'   AETERM = c("abc Covid-19", "covid TEST POSITIVE", "CHILLS"),
#'   AESEQ = c(1, 1, 1, 2, 2, 2),
#'   AEREL = c("是", "否", " ", "否", "否", " "),
#'   AEREL1 = c("不适用", "否", "不适用", "是", "否", " "),
#'   AEREL2 = c("是", "否", " ", "否", "否", " "),
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aerel(AE2)
#' check_ae_aerel(AE2, preproc = ql_derive_seq)
#'
#' AE3 <- data.frame(
#'   STUDYID = 1001,
#'   USUBJID = c(1, 2, 3, 1, 2, 3),
#'   AESTDTC = rep("2020-05-05", 6),
#'   AETERM = c("abc Covid-19", "covid TEST POSITIVE", "CHILLS"),
#'   AESEQ = c(1, 1, 1, 2, 2, 2),
#'   AEREL = c("是", " ", " ", "否", " ", "不适用"),
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aerel(AE3)
#' check_ae_aerel(AE3, preproc = ql_derive_seq)
#'
#'
#' AE4 <- data.frame(
#'   STUDYID = 1001,
#'   USUBJID = c(1, 2, 3, 4, 5, 6),
#'   AESTDTC = rep("2020-05-05", 6),
#'   AETERM = c("abc Covid-19", "covid TEST POSITIVE", "CHILLS"),
#'   AESEQ = c(1, 2, 3, 4, 5, 6),
#'   AEREL = c("是", "是", "否", "", "是", "不适用"),
#'   AEREL1 = "",
#'   AEREL2 = "",
#'   AESPID = "FORMNAME-R:13/L:13XXXX",
#'   stringsAsFactors = FALSE
#' )
#'
#' check_ae_aerel(AE4)
#' check_ae_aerel(AE4, preproc = ql_derive_seq)
#'
check_ae_aerel <- function(AE, preproc = identity, ...) {
  ### Keep only AEREL, AEREL1 - AERELN
  all_aerel <- setdiff(names(AE)[grep("AEREL", names(AE))], names(AE)[grep("AERELNS", names(AE))])

  ### First check that required variables exist and return a message if they don't
  if (AE %lacks_any% c("USUBJID", "AESTDTC", "AETERM", "AEREL")) {
    fail(lacks_msg(AE, c("USUBJID", "AESTDTC", "AETERM", "AEREL")))
  } else {
    # Apply company specific preprocessing function
    AE <- preproc(AE, ...)
    AE <- AE %>%
      select(any_of(c("USUBJID", "AESTDTC", "AETERM", "AEGRPID", "AESPID", "AESEQ", all_aerel))) # [,intersect(names(AE), c("USUBJID","AESTDTC","AETERM","RAVE", all_aerel))]

    mydf_sub <- AE

    mydf_miss <- mydf_sub %>%
      filter(is_sas_na(AE$AEREL) & AE$AEREL != "不适用")

    # mydf_nmiss <- rbind(filter(mydf_sub, !is_sas_na(AE$AEREL)), filter(mydf_sub, AE$AEREL == "NA"))
    mydf_nmiss <- rbind(filter(mydf_sub, !is_sas_na(AE$AEREL)))

    # Calculating number of columns without AEREL-AEREL[n]
    n_col <- mydf_nmiss %>%
      select(!any_of(all_aerel)) %>%
      ncol() %>%
      as.numeric() %>%
      +(2)

    if (as.numeric(length(all_aerel)) > 1) {
      index_y <- as.data.frame(sapply(n_col:ncol(mydf_nmiss), function(x) mydf_nmiss[, x] == "是"))
      index_n <- as.data.frame(sapply(n_col:ncol(mydf_nmiss), function(x) mydf_nmiss[, x] == "否"))
      index_na <- as.data.frame(sapply(n_col:ncol(mydf_nmiss), function(x) mydf_nmiss[, x] == "不适用"))
      index_m <- as.data.frame(sapply(n_col:ncol(mydf_nmiss), function(x) mydf_nmiss[, x] == ""))

      ## For which row the condition is true for all columns
      y <- apply(index_y, 1, any)
      na <- apply(index_na, 1, all)
      n1 <- apply(index_n, 1, any)
      m <- apply(index_m, 1, all)

      n <- n1 != y & n1 == TRUE

      ### Check if there is any unexpected AEREL
      mydf_y <- mydf_nmiss[mydf_nmiss$AEREL == "是" & !y, ]
      mydf_n <- mydf_nmiss[mydf_nmiss$AEREL == "否" & !n, ]
      mydf_na <- mydf_nmiss[mydf_nmiss$AEREL == "不适用" & !na, ]
      mydf_m <- mydf_nmiss[mydf_nmiss$AEREL == "" & !m, ]

      if (nrow(mydf_miss) > 0) {
        index_all <- as.data.frame(rbind(sapply(
          n_col:ncol(mydf_miss),
          function(x) {
            mydf_miss[, x] == "是" |
              mydf_miss[, x] == "不适用" |
              mydf_miss[, x] == "否" |
              mydf_miss[, x] == ""
          }
        )))
        all <- apply(index_all, 1, any)

        mydf_all <- mydf_miss[all, ]

        mydf <- rbind(mydf_y, mydf_n, mydf_m, mydf_all)
        # mydf <- rbind(mydf_y, mydf_na, mydf_n, mydf_m, mydf_all)
      } else {
        mydf <- rbind(mydf_y, mydf_na, mydf_n)
      }
    } else {
      mydf <- mydf_miss
    }

    rownames(mydf) <- NULL

    if (nrow(mydf) == 0) {
      pass()
    } else if (nrow(mydf) == 1) {
      fail(msg = "There is one observation with missing AEREL but one of AERELx is equal to 是/否/不适用, or AEREL has unexpected value, or AERELx missing. ", mydf)
    } else {
      fail(paste("AE has", nrow(mydf), "observations where AEREL is missing but one of AERELx is equal to 是/否/不适用, or AEREL has an unexpected value, or AERELx missing. "), mydf)
    }
  }
}
