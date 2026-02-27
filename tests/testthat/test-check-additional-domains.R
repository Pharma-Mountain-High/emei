# Tests for additional domain check functions (EC, TR, TU, RS, SS, TS)
library(testthat)
library(Emei)
library(dplyr)

# Tests for check_ec_dup() ----
test_that("check_ec_dup returns pass when no duplicate EC records exist", {
  EC <- data.frame(
    USUBJID = 1:5,
    ECTRT = "DRUG A",
    ECDOSE = 10:14,
    ECSTDTC = paste0("2020-01-0", 1:5),
    VISIT = paste0("VISIT ", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_ec_dup(EC)
  expect_true(result)
})

test_that("check_ec_dup returns fail when duplicate EC records exist", {
  EC <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    ECTRT = c("DRUG A", "DRUG A", "DRUG A", "DRUG A", "DRUG A"),
    ECDOSE = c(10, 10, 10, 10, 10),
    ECSTDTC = c("2020-01-01", "2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"),
    VISIT = c("VISIT 1", "VISIT 1", "VISIT 1", "VISIT 1", "VISIT 1"),
    stringsAsFactors = FALSE
  )
  result <- check_ec_dup(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "duplicate")
})

test_that("check_ec_dup handles multiple visits correctly", {
  EC <- data.frame(
    USUBJID = c(1, 1, 2, 2),
    ECTRT = "DRUG A",
    ECDOSE = c(10, 20, 10, 20),
    ECSTDTC = c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01"),
    VISIT = c("VISIT 1", "VISIT 2", "VISIT 1", "VISIT 2"),
    stringsAsFactors = FALSE
  )
  result <- check_ec_dup(EC)
  expect_true(result)
})

test_that("check_ec_dup fails when required variables are missing", {
  EC <- data.frame(
    USUBJID = 1:5,
    ECSTDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_ec_dup(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_tr_dup() ----
test_that("check_tr_dup returns pass when no duplicate TR records exist", {
  TR <- data.frame(
    USUBJID = 1:5,
    TRCAT = "RECIST 1.1",
    TRTESTCD = "LDIAM",
    TRLNKID = paste0("LESION", 1:5),
    TRDTC = paste0("2020-01-0", 1:5),
    TRSTRESC = c("10", "15", "20", "25", "30"),
    VISIT = paste0("VISIT ", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_tr_dup(TR)
  expect_true(result)
})

test_that("check_tr_dup returns fail when duplicate TR records exist", {
  TR <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    TRCAT = c("RECIST 1.1", "RECIST 1.1", "RECIST 1.1", "RECIST 1.1", "RECIST 1.1"),
    TRTESTCD = c("LDIAM", "LDIAM", "LDIAM", "LDIAM", "LDIAM"),
    TRLNKID = c("L1", "L1", "L2", "L3", "L4"),
    TRDTC = c("2020-01-01", "2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"),
    TRSTRESC = c("10", "10", "15", "20", "25"),
    VISIT = c("BASELINE", "BASELINE", "BASELINE", "BASELINE", "BASELINE"),
    stringsAsFactors = FALSE
  )
  result <- check_tr_dup(TR)
  expect_false(result)
  expect_match(attr(result, "msg"), "duplicate")
})

test_that("check_tr_dup handles different lesions correctly", {
  TR <- data.frame(
    USUBJID = c(1, 1, 1),
    TRCAT = "RECIST 1.1",
    TRTESTCD = c("LDIAM", "LDIAM", "LDIAM"),
    TRLNKID = c("L1", "L2", "L3"),
    TRDTC = c("2020-01-01", "2020-01-01", "2020-01-01"),
    TRSTRESC = c("10", "15", "20"),
    VISIT = c("BASELINE", "BASELINE", "BASELINE"),
    stringsAsFactors = FALSE
  )
  result <- check_tr_dup(TR)
  expect_true(result)
})

test_that("check_tr_dup fails when required variables are missing", {
  TR <- data.frame(
    USUBJID = 1:5,
    TRDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_tr_dup(TR)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_tu_tuloc_missing() ----
test_that("check_tu_tuloc_missing returns pass when all TULOC values are present", {
  TU <- data.frame(
    USUBJID = 1:5,
    TUDTC = paste0("2020-01-0", 1:5),
    VISIT = "BASELINE",
    TUORRES = c("TARGET", "TARGET", "NON-TARGET", "TARGET", "TARGET"),
    TULOC = c("LUNG", "LIVER", "BRAIN", "BONE", "LYMPH"),
    TUMETHOD = "CT",
    stringsAsFactors = FALSE
  )
  result <- check_tu_tuloc_missing(TU)
  expect_true(result)
})

test_that("check_tu_tuloc_missing returns fail when TULOC has missing values", {
  TU <- data.frame(
    USUBJID = 1:5,
    TUDTC = paste0("2020-01-0", 1:5),
    VISIT = "BASELINE",
    TUORRES = c("TARGET", "TARGET", "NON-TARGET", "TARGET", "TARGET"),
    TULOC = c("LUNG", NA, "BRAIN", "", "LYMPH"),
    TUMETHOD = "CT",
    stringsAsFactors = FALSE
  )
  result <- check_tu_tuloc_missing(TU)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing.*TULOC")
})

test_that("check_tu_tuloc_missing handles empty strings as missing", {
  TU <- data.frame(
    USUBJID = 1:5,
    TUDTC = paste0("2020-01-0", 1:5),
    VISIT = "BASELINE",
    TUORRES = c("TARGET", "TARGET", "NON-TARGET", "TARGET", "TARGET"),
    TULOC = c("LUNG", "LIVER", "", "BONE", "LYMPH"),
    TUMETHOD = "CT",
    stringsAsFactors = FALSE
  )
  result <- check_tu_tuloc_missing(TU)
  expect_false(result)
})

test_that("check_tu_tuloc_missing fails when required variables are missing", {
  TU <- data.frame(
    USUBJID = 1:5,
    TUTESTCD = "TUMIDENT",
    stringsAsFactors = FALSE
  )
  result <- check_tu_tuloc_missing(TU)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_rs_rscat_rsscat() ----
test_that("check_rs_rscat_rsscat returns pass when RSCAT and RSSCAT are consistent", {
  RS <- data.frame(
    USUBJID = 1:5,
    RSCAT = c("RECIST", "RECIST", "RECIST", "RECIST", "RECIST"),
    RSSCAT = c("TARGET", "TARGET", "NON-TARGET", "TARGET", "TARGET"),
    RSTESTCD = "OVRLRESP",
    stringsAsFactors = FALSE
  )
  result <- check_rs_rscat_rsscat(RS)
  expect_true(result)
})

test_that("check_rs_rscat_rsscat returns fail when RSCAT or RSSCAT are missing", {
  RS <- data.frame(
    USUBJID = 1:5,
    RSCAT = c("RECIST", NA, "RECIST", "RECIST", "RECIST"),
    RSSCAT = c("TARGET", "TARGET", "", "TARGET", "TARGET"),
    RSTESTCD = "OVRLRESP",
    stringsAsFactors = FALSE
  )
  result <- check_rs_rscat_rsscat(RS)
  expect_false(result)
  expect_match(attr(result, "msg"), "unpopulated")
})

test_that("check_rs_rscat_rsscat handles multiple response categories", {
  RS <- data.frame(
    USUBJID = 1:6,
    RSCAT = c("RECIST", "RECIST", "RECIST", "RANO", "RANO", "RANO"),
    RSSCAT = c("TARGET", "NON-TARGET", "NEW", "TARGET", "NON-TARGET", "NEW"),
    RSTESTCD = "OVRLRESP",
    stringsAsFactors = FALSE
  )
  result <- check_rs_rscat_rsscat(RS)
  expect_true(result)
})

test_that("check_rs_rscat_rsscat fails when required variables are missing", {
  RS <- data.frame(
    USUBJID = 1:5,
    RSCAT = c("RECIST", "RECIST", "RECIST", "RECIST", "RECIST"),
    stringsAsFactors = FALSE
  )
  result <- check_rs_rscat_rsscat(RS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ss_ssstat_ssorres() ----
test_that("check_ss_ssstat_ssorres returns pass when SSSTAT and SSORRES are consistent", {
  SS <- data.frame(
    USUBJID = 1:5,
    VISIT = paste0("VISIT ", 1:5),
    SSSTAT = c("", "", "", "未查", "未查"),
    SSORRES = c("存活", "死亡", "未知", "", ""),
    SSDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssstat_ssorres(SS)
  expect_true(result)
})

test_that("check_ss_ssstat_ssorres returns fail when SSSTAT is NOT DONE but SSORRES exists", {
  SS <- data.frame(
    USUBJID = 1:5,
    VISIT = paste0("VISIT ", 1:5),
    SSSTAT = c("", "", "", "未查", "未查"),
    SSORRES = c("存活", "死亡", "未知", "存活", ""),
    SSDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssstat_ssorres(SS)
  expect_false(result)
  expect_match(attr(result, "msg"), "未查")
})

test_that("check_ss_ssstat_ssorres returns fail when SSSTAT is missing but SSORRES is missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    VISIT = paste0("VISIT ", 1:5),
    SSSTAT = c("", "", "", "", ""),
    SSORRES = c("存活", "死亡", "", "存活", "死亡"),
    SSDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssstat_ssorres(SS)
  expect_true(result)
})

test_that("check_ss_ssstat_ssorres fails when required variables are missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSSTAT = c("", "", "", "未查", "未查"),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssstat_ssorres(SS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ts_aedict() ----
test_that("check_ts_aedict returns pass when AE dictionary version is populated", {
  # 2026年2月，应该使用 28.1 (2025年11月后的版本)
  TS <- data.frame(
    TSPARMCD = c("AEDICT", "STUDYID", "SDATE"),
    TSVAL = c("MEDDRA 28.1", "ABC123", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_aedict(TS)
  expect_true(result)
})

test_that("check_ts_aedict returns fail when AEDICT value is missing", {
  TS <- data.frame(
    TSPARMCD = c("AEDICT", "STUDYID", "SDATE"),
    TSVAL = c("", "ABC123", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_aedict(TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "AEDICT")
})

test_that("check_ts_aedict returns fail when AEDICT parameter is not present", {
  TS <- data.frame(
    TSPARMCD = c("STUDYID", "SDATE", "SPONSOR"),
    TSVAL = c("ABC123", "2020-01-01", "ACME"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_aedict(TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "AEDICT")
})

test_that("check_ts_aedict handles NA values in TSVAL", {
  TS <- data.frame(
    TSPARMCD = c("AEDICT", "STUDYID", "SDATE"),
    TSVAL = c(NA, "ABC123", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_aedict(TS)
  expect_false(result)
})

test_that("check_ts_aedict fails when required variables are missing", {
  TS <- data.frame(
    TSPARMCD = c("AEDICT", "STUDYID"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_aedict(TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ts_cmdict() ----
test_that("check_ts_cmdict returns pass when CM dictionary version is populated", {
  # 2026年2月，应该使用 SEPTEMBER 1, 2025 (2025年11月后的版本)
  TS <- data.frame(
    TSPARMCD = c("CMDICT", "STUDYID", "SDATE"),
    TSVAL = c("WHODRUG GLOBAL B3 SEPTEMBER 1, 2025", "ABC123", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_cmdict(TS)
  expect_true(result)
})

test_that("check_ts_cmdict returns fail when CMDICT value is missing", {
  TS <- data.frame(
    TSPARMCD = c("CMDICT", "STUDYID", "SDATE"),
    TSVAL = c("", "ABC123", "2020-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_cmdict(TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "CMDICT")
})

test_that("check_ts_cmdict returns fail when CMDICT parameter is not present", {
  TS <- data.frame(
    TSPARMCD = c("STUDYID", "SDATE", "SPONSOR"),
    TSVAL = c("ABC123", "2020-01-01", "ACME"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_cmdict(TS)
  expect_false(result)
})

test_that("check_ts_cmdict fails when required variables are missing", {
  TS <- data.frame(
    TSPARMCD = c("CMDICT", "STUDYID"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_cmdict(TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_cm_cmdecod() ----
test_that("check_cm_cmdecod returns pass when all CMDECOD values are present", {
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMTRT = c("ASPIRIN", "IBUPROFEN", "TYLENOL", "ADVIL", "MOTRIN"),
    CMDECOD = c("ASPIRIN", "IBUPROFEN", "ACETAMINOPHEN", "IBUPROFEN", "IBUPROFEN"),
    CMSTDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_cm_cmdecod(CM)
  expect_true(result)
})

test_that("check_cm_cmdecod returns fail when CMDECOD has missing values", {
  CM <- data.frame(
    USUBJID = 1:5,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMTRT = c("ASPIRIN", "IBUPROFEN", "TYLENOL", "ADVIL", "MOTRIN"),
    CMDECOD = c("ASPIRIN", NA, "ACETAMINOPHEN", "", "IBUPROFEN"),
    CMSTDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_cm_cmdecod(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_cm_cmdecod handles various missing value formats", {
  CM <- data.frame(
    USUBJID = 1:6,
    CMCAT = "CONCOMITANT MEDICATIONS",
    CMTRT = c("ASPIRIN", "IBUPROFEN", "TYLENOL", "ADVIL", "MOTRIN", "DRUG"),
    CMDECOD = c("ASPIRIN", "NA", ".", "", NA, "DRUG"),
    CMSTDTC = paste0("2020-01-0", 1:6),
    stringsAsFactors = FALSE
  )
  result <- check_cm_cmdecod(CM)
  expect_false(result)
})

test_that("check_cm_cmdecod fails when required variables are missing", {
  CM <- data.frame(
    USUBJID = 1:5,
    CMTRT = c("ASPIRIN", "IBUPROFEN", "TYLENOL", "ADVIL", "MOTRIN"),
    stringsAsFactors = FALSE
  )
  result <- check_cm_cmdecod(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_mi_mispec() ----
test_that("check_mi_mispec returns pass when MISPEC is populated for all records", {
  MI <- data.frame(
    USUBJID = 1:5,
    MISPEC = c("SERUM", "PLASMA", "BLOOD", "URINE", "CSF"),
    MITESTCD = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5"),
    stringsAsFactors = FALSE
  )
  result <- check_mi_mispec(MI)
  expect_true(result)
})

test_that("check_mi_mispec returns fail when MISPEC has missing values", {
  MI <- data.frame(
    USUBJID = 1:5,
    MISPEC = c("SERUM", NA, "BLOOD", "", "CSF"),
    MITESTCD = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5"),
    stringsAsFactors = FALSE
  )
  result <- check_mi_mispec(MI)
  expect_false(result)
  expect_match(attr(result, "msg"), "MISPEC")
})

test_that("check_mi_mispec handles various missing value formats", {
  MI <- data.frame(
    USUBJID = 1:5,
    MISPEC = c("SERUM", "NA", ".", "", "CSF"),
    MITESTCD = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5"),
    stringsAsFactors = FALSE
  )
  result <- check_mi_mispec(MI)
  expect_false(result)
})

test_that("check_mi_mispec fails when required variables are missing", {
  MI <- data.frame(
    USUBJID = 1:5,
    MITESTCD = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5"),
    stringsAsFactors = FALSE
  )
  result <- check_mi_mispec(MI)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ts_sstdtc_ds_consent() ----
test_that("check_ts_sstdtc_ds_consent returns pass when SSTDTC matches earliest consent date", {
  TS <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    TSVAL1 = "",
    TSVAL2 = "",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_true(result)
})

test_that("check_ts_sstdtc_ds_consent returns fail when SSTDTC does not match earliest consent", {
  TS <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2017-01-01",
    TSVAL1 = "",
    TSVAL2 = "",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "does not match earliest informed consent")
})

test_that("check_ts_sstdtc_ds_consent returns fail when SSTDTC parameter is missing", {
  TS <- data.frame(
    STUDYID = 2,
    TSPARMCD = "AEDICT",
    TSPARM = "Study Start Date",
    TSVAL = "MedDRA v23.0",
    TSVAL1 = "",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "not found")
})

test_that("check_ts_sstdtc_ds_consent returns fail when SSTDTC value is missing", {
  TS <- data.frame(
    STUDYID = 3,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ts_sstdtc_ds_consent returns fail for incomplete date", {
  TS <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01",
    TSVAL1 = "",
    TSVAL2 = "",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing complete")
})

test_that("check_ts_sstdtc_ds_consent fails when DS required variables are missing", {
  TS <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    TSPARM = "Study Start Date",
    TSVAL = "2020-01-02",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ts_sstdtc_ds_consent fails when TS required variables are missing", {
  TS <- data.frame(
    STUDYID = 1,
    TSPARMCD = "SSTDTC",
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3, 4),
    DSCAT = rep("方案里程碑", 5),
    DSSCAT = rep("方案里程碑", 5),
    DSDECOD = c("签署知情同意", "其他", "医生决定", "其他", "签署知情同意"),
    DSSTDTC = c("2021-01-01", "2021-01-02", "2021-01-02", "2021-01-02", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ts_sstdtc_ds_consent(DS, TS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_vs_vsdtc_after_dd() ----
test_that("check_vs_vsdtc_after_dd returns pass when no VS dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    VSTESTCD = letters[1:5],
    VSORRES = 1:5,
    VSSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_true(result)
})

test_that("check_vs_vsdtc_after_dd returns fail when VS dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    VSTESTCD = letters[1:5],
    VSORRES = 1:5,
    VSSTAT = "",
    stringsAsFactors = FALSE
  )
  VS$VSDTC[1] <- "2016-01-03"
  VS$USUBJID[1] <- VS$USUBJID[5]
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_false(result)
  expect_match(attr(result, "msg"), "after death date")
})

test_that("check_vs_vsdtc_after_dd handles VSSTAT filtering", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    VSTESTCD = letters[1:5],
    VSORRES = 1:5,
    VSSTAT = "",
    stringsAsFactors = FALSE
  )
  VS$VSDTC[1] <- "2016-01-03"
  VS$USUBJID[1] <- VS$USUBJID[5]
  VS$VSSTAT[1] <- "未查"
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_true(result)
})

test_that("check_vs_vsdtc_after_dd returns pass when no death dates", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = rep("", 5),
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    VSTESTCD = letters[1:5],
    VSORRES = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_true(result)
})

test_that("check_vs_vsdtc_after_dd fails when DM required variables are missing", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    VSTESTCD = letters[1:5],
    VSORRES = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_vs_vsdtc_after_dd fails when VS required variables are missing", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    VSDTC = rep("2015-12-31", 5),
    stringsAsFactors = FALSE
  )
  result <- check_vs_vsdtc_after_dd(DM, VS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_cm_missing_month() ----
test_that("check_cm_missing_month returns pass when no missing months", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    CMSTDTC = c("2017-01-01", "2017-02-01", "2017-01-02"),
    CMENDTC = c("2017-02-01", "2017-03-01", "2017-02-01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM)
  expect_true(result)
})

test_that("check_cm_missing_month returns fail when month is missing in CMSTDTC", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    CMSTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    CMENDTC = c("2017-02-01", "2017-03-01", "2017-02-01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_cm_missing_month returns fail when month is missing in CMENDTC", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    CMSTDTC = c("2017-01-01", "2017-02-01", "2017-01-02"),
    CMENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_cm_missing_month returns fail when month is missing in both dates", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    CMSTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    CMENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    CMSPID = "/F:XXX-D:12345-R:123",
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_cm_missing_month works with preproc parameter", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    CMSTDTC = c("2017-01-01", "2017-02-01", "2017-01-02"),
    CMENDTC = c("2017-02-01", "2017-03-01", "2017-02-01"),
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM, preproc = identity)
  expect_true(result)
})

test_that("check_cm_missing_month fails when required variables are missing", {
  CM <- data.frame(
    USUBJID = 1:3,
    CMSEQ = 11:13,
    CMTRT = c("CM1", "CM2", "CM3"),
    stringsAsFactors = FALSE
  )
  result <- check_cm_missing_month(CM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ec_ecdose_pos_ecoccur_no() ----
test_that("check_ec_ecdose_pos_ecoccur_no returns pass when ECOCCUR and ECDOSE consistent", {
  EC <- data.frame(
    USUBJID = 1:6,
    ECSTDTC = rep("2017-01-01", 6),
    ECTRT = c(rep("TRT A", 3), rep("TRT B", 3)),
    ECOCCUR = c("是", "否", "是", "是", "是", "否"),
    ECDOSE = c(10, 0, 5, 15, 8, 0),
    VISIT = "VISIT 1",
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdose_pos_ecoccur_no(EC)
  expect_true(result)
})

test_that("check_ec_ecdose_pos_ecoccur_no returns fail when ECDOSE>0 but ECOCCUR not 是", {
  EC <- data.frame(
    USUBJID = 1:6,
    ECSTDTC = rep("2017-01-01", 6),
    ECTRT = c(rep("TRT A", 3), rep("TRT B", 3)),
    ECOCCUR = c("是", "否", ".", "", "是", "否"),
    ECDOSE = c(0, 10, 5, 15, 0, 8),
    VISIT = "VISIT 1",
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdose_pos_ecoccur_no(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "positive dose")
})

test_that("check_ec_ecdose_pos_ecoccur_no handles drug parameter", {
  EC <- data.frame(
    USUBJID = 1:6,
    ECSTDTC = rep("2017-01-01", 6),
    ECTRT = c(rep("TRT A", 3), rep("TRT B", 3)),
    ECOCCUR = c("是", "否", ".", "", "是", "否"),
    ECDOSE = c(0, 10, 5, 15, 0, 8),
    VISIT = "VISIT 1",
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdose_pos_ecoccur_no(EC, drug = "TRT A")
  expect_false(result)
  expect_match(attr(result, "msg"), "TRT A")
})

test_that("check_ec_ecdose_pos_ecoccur_no fails when required variables are missing", {
  EC <- data.frame(
    USUBJID = 1:6,
    ECSTDTC = rep("2017-01-01", 6),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdose_pos_ecoccur_no(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ec_ecdosu() ----
test_that("check_ec_ecdosu returns pass when all ECDOSU present", {
  EC <- data.frame(
    USUBJID = 1:10,
    ECTRT = 1:10,
    ECSTDTC = 1:10,
    ECDOSE = 1:10,
    ECOCCUR = as.character(c(rep("是", 5), rep("否", 5))),
    ECDOSU = as.character(rep("mg", 10)),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdosu(EC)
  expect_true(result)
})

test_that("check_ec_ecdosu returns fail when ECDOSU missing", {
  EC <- data.frame(
    USUBJID = 1:10,
    ECTRT = 1:10,
    ECSTDTC = 1:10,
    ECDOSE = 1:10,
    ECOCCUR = as.character(c(rep("是", 5), rep("否", 5))),
    ECDOSU = as.character(rep("mg", 10)),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  EC$ECDOSU[1] <- ""
  EC$ECDOSU[2] <- "NA"
  EC$ECDOSU[3] <- NA
  result <- check_ec_ecdosu(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing dose units")
})

test_that("check_ec_ecdosu handles ECOCCUR filtering", {
  EC <- data.frame(
    USUBJID = 1:10,
    ECTRT = 1:10,
    ECSTDTC = 1:10,
    ECDOSE = 1:10,
    ECOCCUR = as.character(c(rep("是", 5), rep("否", 5))),
    ECDOSU = as.character(rep("mg", 10)),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  EC$ECDOSU[6] <- NA
  result <- check_ec_ecdosu(EC)
  expect_true(result)
})

test_that("check_ec_ecdosu fails when required variables are missing", {
  EC <- data.frame(
    USUBJID = 1:10,
    ECTRT = 1:10,
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecdosu(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ec_ecoccur_mis_ecdose_nonmis() ----
test_that("check_ec_ecoccur_mis_ecdose_nonmis returns pass when consistent", {
  EC <- data.frame(
    USUBJID = 1:4,
    ECSTDTC = rep("2017-01-01", 4),
    ECTRT = rep("TRT A", 4),
    ECOCCUR = c("是", "是", "是", "是"),
    ECDOSE = c(10, 15, 20, 5),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecoccur_mis_ecdose_nonmis(EC)
  expect_true(result)
})

test_that("check_ec_ecoccur_mis_ecdose_nonmis returns fail when ECOCCUR missing but ECDOSE present", {
  EC <- data.frame(
    USUBJID = 1:5,
    ECSTDTC = rep("2017-01-01", 5),
    ECTRT = c(rep("TRT A", 2), rep("TRT B", 3)),
    ECOCCUR = c("是", NA, ".", "", "否"),
    ECDOSE = c(10, 15, 0, 20, 5),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecoccur_mis_ecdose_nonmis(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "ECOCCUR missing")
})

test_that("check_ec_ecoccur_mis_ecdose_nonmis handles ECMOOD filtering", {
  EC <- data.frame(
    USUBJID = 1:5,
    ECSTDTC = rep("2017-01-01", 5),
    ECTRT = rep("TRT A", 5),
    ECOCCUR = c("是", "是", "是", "是", "是"),
    ECDOSE = c(10, 15, 20, 5, 8),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecoccur_mis_ecdose_nonmis(EC)
  expect_true(result)
})

test_that("check_ec_ecoccur_mis_ecdose_nonmis fails when required variables are missing", {
  EC <- data.frame(
    USUBJID = 1:5,
    ECSTDTC = rep("2017-01-01", 5),
    ECMOOD = "已执行",
    stringsAsFactors = FALSE
  )
  result <- check_ec_ecoccur_mis_ecdose_nonmis(EC)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ss_ssdtc_alive_dm() ----
test_that("check_ss_ssdtc_alive_dm returns pass when SSDTC before death date", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSSEQ = 1:5,
    SSDTC = "2020-01-02",
    SSTESTCD = "SURVSTAT",
    SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "生存随访",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-03",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_alive_dm(SS, DM)
  expect_true(result)
})

test_that("check_ss_ssdtc_alive_dm returns fail when SSDTC after death date", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSSEQ = 1:5,
    SSDTC = "2020-01-04",
    SSTESTCD = "SURVSTAT",
    SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "生存随访",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-04", "2020-01-05", "2020-01-03", "2020-01-04", "2020-01-05"),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_alive_dm(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "later than DM.DTHDTC")
})

test_that("check_ss_ssdtc_alive_dm works with preproc parameter", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSTESTCD = "SURVSTAT",
    SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "生存随访",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-03",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_alive_dm(SS, DM, preproc = identity)
  expect_true(result)
})

test_that("check_ss_ssdtc_alive_dm fails when SS required variables are missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-03",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_alive_dm(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ss_ssdtc_alive_dm fails when DM required variables are missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSTESTCD = "SURVSTAT",
    SSORRES = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "生存随访",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_alive_dm(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ss_ssdtc_dead_dthdtc() ----
test_that("check_ss_ssdtc_dead_dthdtc returns pass when SSDTC >= death date", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "DAY 10",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-02",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_dead_dthdtc(SS, DM)
  expect_true(result)
})

test_that("check_ss_ssdtc_dead_dthdtc returns fail when SSDTC < death date", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "FOLLOW-UP",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_dead_dthdtc(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "less than death date")
})

test_that("check_ss_ssdtc_dead_dthdtc filters only DEAD records", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC = c("存活", "存活", "存活", "存活", "存活"),
    VISIT = "DAY 10",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_dead_dthdtc(SS, DM)
  expect_true(result)
})

test_that("check_ss_ssdtc_dead_dthdtc fails when SS required variables are missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = "2020-01-02",
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_dead_dthdtc(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ss_ssdtc_dead_dthdtc fails when DM required variables are missing", {
  SS <- data.frame(
    USUBJID = 1:5,
    SSDTC = "2020-01-02",
    SSSTRESC = c("死亡", "死亡", "存活", "死亡", "存活"),
    VISIT = "DAY 10",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_ss_ssdtc_dead_dthdtc(SS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ce_missing_month() ----
test_that("check_ce_missing_month returns pass when no missing months", {
  CE <- data.frame(
    USUBJID = c(1, 2, 3, 4),
    CESEQ = 1:4,
    CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
    CESTDTC = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-10"),
    CEENDTC = c("2023-01-02", "2023-01-16", "2023-02-02", "2023-02-12"),
    stringsAsFactors = FALSE
  )
  result <- check_ce_missing_month(CE)
  expect_true(result)
})

test_that("check_ce_missing_month returns fail when month missing in CESTDTC", {
  CE <- data.frame(
    USUBJID = c(1, 2, 3, 4),
    CESEQ = 1:4,
    CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
    CESTDTC = c("2023---01", "2023-01-15", "2023-02-01", "2023-02-10"),
    CEENDTC = c("2023-01-02", "2023-01-16", "2023-02-02", "2023-02-12"),
    stringsAsFactors = FALSE
  )
  result <- check_ce_missing_month(CE)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_ce_missing_month returns fail when month missing in CEENDTC", {
  CE <- data.frame(
    USUBJID = c(1, 2, 3, 4),
    CESEQ = 1:4,
    CETERM = c("Headache", "Nausea", "Dizziness", "Fever"),
    CESTDTC = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-10"),
    CEENDTC = c("2023-01-02", "2023---01", "2023-02-02", "2023-02-12"),
    stringsAsFactors = FALSE
  )
  result <- check_ce_missing_month(CE)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_ce_missing_month fails when required variables are missing", {
  CE <- data.frame(
    USUBJID = c(1, 2, 3, 4),
    CESEQ = 1:4,
    stringsAsFactors = FALSE
  )
  result <- check_ce_missing_month(CE)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_mh_missing_month() ----
test_that("check_mh_missing_month returns pass when no missing months", {
  MH <- data.frame(
    USUBJID = LETTERS[1:5],
    MHTERM = LETTERS[5:1],
    MHSTDTC = c("2014-01-01", "2014-02-01", "2014-01", "", NA),
    stringsAsFactors = FALSE
  )
  result <- check_mh_missing_month(MH)
  expect_true(result)
})

test_that("check_mh_missing_month returns fail when month missing", {
  MH <- data.frame(
    USUBJID = LETTERS[1:5],
    MHTERM = LETTERS[5:1],
    MHSTDTC = c("2014", NA, "2014-01", "", "2014---02"),
    stringsAsFactors = FALSE
  )
  result <- check_mh_missing_month(MH)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_mh_missing_month works with preproc parameter", {
  MH <- data.frame(
    USUBJID = LETTERS[1:5],
    MHTERM = LETTERS[5:1],
    MHSTDTC = c("2014-01-01", "2014-02-01", "2014-01", "", NA),
    stringsAsFactors = FALSE
  )
  result <- check_mh_missing_month(MH, preproc = identity)
  expect_true(result)
})

test_that("check_mh_missing_month fails when required variables are missing", {
  MH <- data.frame(
    USUBJID = LETTERS[1:5],
    stringsAsFactors = FALSE
  )
  result <- check_mh_missing_month(MH)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_pr_missing_month() ----
test_that("check_pr_missing_month returns pass when no missing months", {
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name", "Procedure Name", "Procedure"),
    PRSTDTC = c("2017-01-01", "2017-02-01", "2017-01-02"),
    PRENDTC = c("2017-02-01", "2017-03-01", "2017-02-01"),
    stringsAsFactors = FALSE
  )
  result <- check_pr_missing_month(PR)
  expect_true(result)
})

test_that("check_pr_missing_month returns fail when month missing in PRSTDTC", {
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name", "Procedure Name", "Procedure"),
    PRSTDTC = c("2017-01-01", "2017---01", "2017-01-02"),
    PRENDTC = c("2017-02-01", "2017-03-01", "2017-02-01"),
    stringsAsFactors = FALSE
  )
  result <- check_pr_missing_month(PR)
  expect_false(result)
  expect_match(attr(result, "msg"), "unknown month")
})

test_that("check_pr_missing_month returns fail when month missing in PRENDTC", {
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name", "Procedure Name", "Procedure"),
    PRSTDTC = c("2017-01-01", "2017-02-01", "2017-01-02"),
    PRENDTC = c("2017-02-01", "2017-03-01", "2017---01"),
    stringsAsFactors = FALSE
  )
  result <- check_pr_missing_month(PR)
  expect_false(result)
  expect_match(attr(result, "msg"), "unknown month")
})

test_that("check_pr_missing_month fails when required variables are missing", {
  PR <- data.frame(
    USUBJID = 1:3,
    PRTRT = c("Surgery Name", "Procedure Name", "Procedure"),
    stringsAsFactors = FALSE
  )
  result <- check_pr_missing_month(PR)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_eg_egdtc_visit_ordinal_error() ----
test_that("check_eg_egdtc_visit_ordinal_error returns pass when dates in order", {
  EG <- data.frame(
    USUBJID = rep(c(101, 102), each = 5),
    EGDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("筛选期", "C1/D1", "C2/D1", "C3/D1", "计划外访视"), 2),
    EGSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_eg_egdtc_visit_ordinal_error(EG)
  expect_true(result)
})

test_that("check_eg_egdtc_visit_ordinal_error returns fail when dates out of order", {
  EG <- data.frame(
    USUBJID = rep(c(101, 102), each = 5),
    EGDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("筛选期", "C1/D1", "C2/D1", "C3/D1", "计划外访视"), 2),
    EGSTAT = "",
    stringsAsFactors = FALSE
  )
  EG$EGDTC[EG$USUBJID == 101 & EG$VISIT == "C3/D1"] <- "2017-01-10T08:25"
  result <- check_eg_egdtc_visit_ordinal_error(EG)
  expect_false(result)
  expect_match(attr(result, "msg"), "data entry error")
})

test_that("check_eg_egdtc_visit_ordinal_error handles duplicated dates", {
  EG <- data.frame(
    USUBJID = rep("101", 6),
    EGDTC = rep("2017-01-01T08:25", 6),
    VISITNUM = rep(1:2, 3),
    VISIT = rep("筛选期", 6),
    EGSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_eg_egdtc_visit_ordinal_error(EG)
  expect_true(result)
})

test_that("check_eg_egdtc_visit_ordinal_error fails when required variables are missing", {
  EG <- data.frame(
    USUBJID = rep(c(101, 102), each = 5),
    EGDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
    stringsAsFactors = FALSE
  )
  result <- check_eg_egdtc_visit_ordinal_error(EG)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})
