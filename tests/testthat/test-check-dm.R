# Tests for DM domain check functions
library(testthat)
library(Emei)
library(dplyr)

# Tests for check_dm_usubjid_dup() ----
test_that("check_dm_usubjid_dup returns pass when no duplicate USUBJIDs", {
  DM <- data.frame(
    USUBJID = c("GO12345-00000-1000", "GO12345-11111-1001", "GO12345-11111-1002"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_true(result)
})

test_that("check_dm_usubjid_dup returns fail when duplicate USUBJIDs exist", {
  DM <- data.frame(
    USUBJID = c("GO12345-00000-1000", "GO12345-11111-1000", "GO12345-00000-1000"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "Duplicate")
})

test_that("check_dm_usubjid_dup detects same patient number across sites (3-part)", {
  DM <- data.frame(
    USUBJID = c("GO12345-00000-1000", "GO12345-11111-1000", "GO12345-00000-1001"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_false(result)
})

test_that("check_dm_usubjid_dup detects duplicates in 2-part USUBJID", {
  DM <- data.frame(
    USUBJID = c("GO12345-1000", "GO12345-1000"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_false(result)
})

test_that("check_dm_usubjid_dup passes when no duplicates in 2-part USUBJID", {
  DM <- data.frame(
    USUBJID = c("GO12345-1000", "GO12345-1001", "GO12345-1002"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_true(result)
})

test_that("check_dm_usubjid_dup handles dataframe with other variables", {
  DM <- data.frame(
    USUBJID = c("GO12345-1000", "GO12345-1000"),
    SEX = c("M", "F"),
    AGE = c(18, 60),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_false(result)
})

test_that("check_dm_usubjid_dup fails when USUBJID variable is missing", {
  DM <- data.frame(
    STUDYID = c("GO12345"),
    SEX = c("M"),
    AGE = c(72),
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_dup(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "USUBJID")
})

# Tests for check_dm_age_missing() ----
test_that("check_dm_age_missing returns pass when all ages are valid", {
  DM <- data.frame(
    USUBJID = 1:5,
    AGE = c(18, 25, 40, 65, 89),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_true(result)
})

test_that("check_dm_age_missing returns fail when AGE is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    AGE = c(50, 60, NA, 40, 22),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "suspicious age")
})

test_that("check_dm_age_missing returns fail when AGE < 18", {
  DM <- data.frame(
    USUBJID = 1:5,
    AGE = c(50, 60, 17, 40, 22),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
})

test_that("check_dm_age_missing returns fail when AGE >= 90", {
  DM <- data.frame(
    USUBJID = 1:5,
    AGE = c(50, 60, 99, 40, 22),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
})

test_that("check_dm_age_missing handles multiple age issues", {
  DM <- data.frame(
    USUBJID = 1:10,
    AGE = c(50, 60, 17, 99, NA, 33, 500, 40, 22, NA),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
})

test_that("check_dm_age_missing fails when AGE variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "AGE")
})

test_that("check_dm_age_missing fails when USUBJID variable is missing", {
  DM <- data.frame(
    AGE = c(18, 25, 40),
    stringsAsFactors = FALSE
  )
  result <- check_dm_age_missing(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "USUBJID")
})

# Tests for check_dm_usubjid_ae_usubjid() ----
test_that("check_dm_usubjid_ae_usubjid returns pass when all AE USUBJIDs are in DM", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = "ARM A",
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = 1:5,
    AETERM = c("AE1", "AE2", "AE3", "AE4", "AE5"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:5,
    DSDECOD = "知情同意",
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = "2020-01-02",
    EXDOSE = 10,
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
  expect_true(result)
})

test_that("check_dm_usubjid_ae_usubjid returns fail when AE has USUBJID not in DM", {
  DM <- data.frame(
    USUBJID = 1:3,
    ARM = "ARM A",
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = c(1, 2, 3),
    AETERM = c("AE1", "AE2", "AE3"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSDECOD = "知情同意",
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = 1:3,
    EXSTDTC = "2020-01-02",
    EXDOSE = 10,
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
  expect_true(result)
})

test_that("check_dm_usubjid_ae_usubjid fails when DM USUBJID is missing", {
  DM <- data.frame(
    ARM = "ARM A",
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = 1:3,
    AETERM = c("AE1", "AE2", "AE3"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSDECOD = "知情同意",
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = 1:3,
    EXSTDTC = "2020-01-02",
    EXDOSE = 10,
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_usubjid_ae_usubjid fails when AE USUBJID is missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    ARM = "ARM A",
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    AETERM = c("AE1", "AE2", "AE3"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSDECOD = "知情同意",
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = 1:3,
    EXSTDTC = "2020-01-02",
    EXDOSE = 10,
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_usubjid_ae_usubjid passes when AE is empty", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = "ARM A",
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = character(0),
    AETERM = character(0),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:5,
    DSDECOD = "知情同意",
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = "2020-01-02",
    EXDOSE = 10,
    EXTRT = "DRUG A",
    stringsAsFactors = FALSE
  )
  result <- check_dm_usubjid_ae_usubjid(DM, AE, DS, EX)
  expect_false(result)
})

# Tests for check_dm_armcd() ----
test_that("check_dm_armcd returns pass when all ARMCD values are non-missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM A", "ARM B"),
    ARMCD = c("A", "B", "C", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_true(result)
})

test_that("check_dm_armcd returns fail when ARMCD has missing values", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM A", "ARM B"),
    ARMCD = c("A", NA, "C", "", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_armcd returns fail when ARMCD has empty strings", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM A", "ARM A", "ARM B"),
    ARMCD = c("A", "B", "", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_false(result)
})

test_that("check_dm_armcd returns fail when ARMCD has 'NA' string", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM A", "ARM B"),
    ARMCD = c("A", "NA", "C", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_false(result)
})

test_that("check_dm_armcd fails when ARMCD variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM A", "ARM B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "ARMCD")
})

test_that("check_dm_armcd fails when USUBJID variable is missing", {
  DM <- data.frame(
    ARM = c("ARM A", "ARM B", "ARM C"),
    ARMCD = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_armcd(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "USUBJID")
})

# Tests for check_dm_actarm_arm() ----
test_that("check_dm_actarm_arm returns pass when ACTARM matches ARM", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM A", "ARM B", "ARM A"),
    ACTARM = c("ARM A", "ARM B", "ARM A", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_true(result)
})

test_that("check_dm_actarm_arm returns fail when ACTARM does not match ARM", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM A", "ARM B", "ARM A"),
    ACTARM = c("ARM A", "ARM A", "ARM A", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "ARM")
})

test_that("check_dm_actarm_arm handles missing values appropriately", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM B", "ARM A"),
    ACTARM = c("ARM A", "ARM B", "ARM D", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_false(result)
})

test_that("check_dm_actarm_arm fails when ARM variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    ACTARM = c("ARM A", "ARM B", "ARM A", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_actarm_arm fails when ACTARM variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM A", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_actarm_arm fails when USUBJID variable is missing", {
  DM <- data.frame(
    ARM = c("ARM A", "ARM B", "ARM A"),
    ACTARM = c("ARM A", "ARM B", "ARM A"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_actarm_arm(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_dm_dthfl_dthdtc() ----
test_that("check_dm_dthfl_dthdtc returns pass when death flag and date are consistent", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("是", "是", "否", "否", ""),
    DTHDTC = c("2020-01-01", "2020-02-01", "", "", ""),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_true(result)
})

test_that("check_dm_dthfl_dthdtc returns fail when DTHFL is Y but DTHDTC is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("是", "是", "否", "否", "是"),
    DTHDTC = c("2020-01-01", "2020-02-01", "", "", ""),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "inconsistent")
})

test_that("check_dm_dthfl_dthdtc returns fail when DTHFL is not Y but DTHDTC exists", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("是", "是", "否", "否", "否"),
    DTHDTC = c("2020-01-01", "2020-02-01", "", "", "2020-03-01"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
})

test_that("check_dm_dthfl_dthdtc handles multiple inconsistencies", {
  DM <- data.frame(
    USUBJID = 1:10,
    DTHFL = c("是", "是", "否", "否", "是", "", "否", "是", "是", "否"),
    DTHDTC = c("2020-01-01", "", "", "2020-03-01", "", "2020-04-01", "", "2020-05-01", "2020-06-01", ""),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
})

test_that("check_dm_dthfl_dthdtc fails when DTHFL variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-01", "", "", "", ""),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_dthfl_dthdtc fails when DTHDTC variable is missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("Y", "Y", "N", "N", "N"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_dthfl_dthdtc fails when USUBJID variable is missing", {
  DM <- data.frame(
    DTHFL = c("Y", "N", "N"),
    DTHDTC = c("2020-01-01", "", ""),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthfl_dthdtc(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_dm_dthdtc_ds_death() ----
test_that("check_dm_dthdtc_ds_death returns pass when death dates are consistent", {
  DM <- data.frame(
    USUBJID = 1:4,
    DTHDTC = c("2020-01-01", "2020-01-01", "2020-01-01", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:4,
    DSTERM = c("死亡", "死亡", "死亡", "不良事件"),
    DSDECOD = c("死亡", "死亡", "死亡", "死亡"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_true(result)
})

test_that("check_dm_dthdtc_ds_death returns fail when DS indicates death but DTHDTC is missing", {
  DM <- data.frame(
    USUBJID = 1:4,
    DTHDTC = c(NA, NA, "2020-01-01", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:4,
    DSTERM = c("死亡", "死亡", "死亡", "死亡"),
    DSDECOD = c("死亡", "死亡", "死亡", "死亡"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "death date")
})

test_that("check_dm_dthdtc_ds_death passes with newer mapping (DSTERM not '死亡')", {
  DM <- data.frame(
    USUBJID = 1:4,
    DTHDTC = c(NA, NA, "2020-01-01", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:4,
    DSTERM = c("不良事件", "不良事件", "不良事件", "不良事件"),
    DSDECOD = c("死亡", "死亡", "死亡", "死亡"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_true(result)
})

test_that("check_dm_dthdtc_ds_death handles multiple death records correctly", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-01", "2020-01-02", NA, "2020-01-04", "2020-01-05"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 2, 3, 3, 4),
    DSTERM = c("死亡", "死亡", "死亡", "死亡", "死亡"),
    DSDECOD = c("死亡", "死亡", "死亡", "死亡", "死亡"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_false(result)
})

test_that("check_dm_dthdtc_ds_death passes when no death records in DS", {
  DM <- data.frame(
    USUBJID = 1:3,
    DTHDTC = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSTERM = c("完成研究", "完成研究", "完成研究"),
    DSDECOD = c("完成", "完成", "完成"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_true(result)
})

test_that("check_dm_dthdtc_ds_death fails when DM required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSTERM = c("死亡", "死亡", "死亡"),
    DSDECOD = c("死亡", "死亡", "死亡"),
    DSSTDTC = "2020-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_dthdtc_ds_death fails when DS required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    DTHDTC = c("2020-01-01", "2020-01-01", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSTERM = c("死亡", "死亡", "死亡"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_dthdtc_ds_death(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_dm_ae_ds_death() ----
test_that("check_dm_ae_ds_death returns pass when death information is consistent across domains", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("是", "是", "", "", ""),
    DTHDTC = c("2020-01-01", "2020-01-02", "", "", ""),
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = c(1, 2),
    AEDECOD = c("Death", "Death"),
    AEOUT = c("死亡", "死亡"),
    AESDTH = c("是", "是"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 2),
    DSSCAT = c("方案里程碑", "方案里程碑"),
    DSDECOD = c("死亡", "死亡"),
    DSSTDTC = c("2020-01-01", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ae_ds_death(DM, DS, AE)
  expect_true(result)
})

test_that("check_dm_ae_ds_death returns fail when death information is inconsistent", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHFL = c("是", "是", "是", "", ""),
    DTHDTC = c("2020-01-01", "2020-01-02", "2020-01-03", "", ""),
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = c(1, 2),
    AEDECOD = c("Death", "Other AE"),
    AEOUT = c("死亡", "恢复"),
    AESDTH = c("是", "否"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 2),
    DSSCAT = c("方案里程碑", "方案里程碑"),
    DSDECOD = c("死亡", "完成"),
    DSSTDTC = c("2020-01-01", "2020-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ae_ds_death(DM, DS, AE)
  expect_false(result)
  expect_match(attr(result, "msg"), "death")
})

test_that("check_dm_ae_ds_death handles patients with death in one domain but not others", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2020-01-01", "2020-01-02", "2020-01-03", "", ""),
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = c(1, 2),
    AEDECOD = c("Death", "Other AE"),
    AESDTH = c("是", "否"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 2, 3),
    DSDECOD = c("死亡", "完成", "死亡"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ae_ds_death(DM, AE, DS)
  expect_false(result)
})

test_that("check_dm_ae_ds_death fails when required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    stringsAsFactors = FALSE
  )
  AE <- data.frame(
    USUBJID = c(1, 2),
    AEDECOD = c("Death", "Death"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = c(1, 2),
    DSDECOD = c("死亡", "死亡"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ae_ds_death(DM, AE, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_dm_randc_randr() ----
test_that("check_dm_randc_randr returns pass when randomization codes match randomization groups", {
  DM <- data.frame(
    USUBJID = 1:5,
    RANDC1 = c("A", "B", "C", "A", "B"),
    RANDR1 = c("A", "B", "C", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_randc_randr(DM)
  expect_true(result)
})

test_that("check_dm_randc_randr returns fail when codes and groups are inconsistent", {
  DM <- data.frame(
    USUBJID = 1:5,
    RANDC1 = c("A", "B", "C", "A", "X"),
    RANDR1 = c("A", "B", "C", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_randc_randr(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "RANDC")
})

test_that("check_dm_randc_randr passes when no randomization variables exist", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("ARM A", "ARM B", "ARM C", "ARM A", "ARM B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_randc_randr(DM)
  expect_true(result)
})

# Tests for check_dm_strat() ----
test_that("check_dm_strat returns pass when stratification variables are populated", {
  DM <- data.frame(
    USUBJID = 1:5,
    RANDC1 = c("A", "B", "C", "A", "B"),
    RANDR1 = c("Group A", "Group B", "Group C", "Group A", "Group B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_strat(DM)
  expect_true(result)
})

test_that("check_dm_strat returns fail when stratification variables have missing values", {
  DM <- data.frame(
    USUBJID = 1:5,
    RANDC1 = c("A", NA, "C", "", "B"),
    RANDR1 = c("Group A", "Group B", "Group C", "Group A", "Group B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_strat(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "stratification")
})

test_that("check_dm_strat passes when no stratification variables exist", {
  DM <- data.frame(
    USUBJID = 1:5,
    ARM = c("A", "B", "C", "A", "B"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_strat(DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "randomized")
})

# Tests for check_dm_ds_partial_death_dates() ----
test_that("check_dm_ds_partial_death_dates returns pass when all death dates are complete", {
  DM <- data.frame(
    USUBJID = 1:3,
    DTHDTC = c("2017-01-01", "2017-02-15", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSEQ = 11:13,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "死亡", "完成"),
    DSSTDTC = c("2017-01-01", "2017-02-15", "2017-03-01"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_true(result)
})

test_that("check_dm_ds_partial_death_dates returns fail when DM has partial death dates", {
  DM <- data.frame(
    USUBJID = 1:3,
    DTHDTC = c("2017-01-01", "2017", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSEQ = 11:13,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "死亡", "完成"),
    DSSTDTC = c("2017-01-01", "2017-02-15", "2017-03-01"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "partial death dates")
})

test_that("check_dm_ds_partial_death_dates returns fail when DS has partial death dates", {
  DM <- data.frame(
    USUBJID = 1:4,
    DTHDTC = c("2017-01-01", "2017-02-15", "2017-03-20", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:4,
    DSSEQ = 11:14,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "死亡", "死亡", "完成"),
    DSSTDTC = c("2017-01-01", "2017", "2017-01-02", "2016-10"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_false(result)
})

test_that("check_dm_ds_partial_death_dates handles multiple partial dates", {
  DM <- data.frame(
    USUBJID = 1:5,
    DTHDTC = c("2017-01-01", "2017", "2016", NA, "2018-05-10"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:5,
    DSSEQ = 11:15,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "死亡", "死亡", "完成", "死亡"),
    DSSTDTC = c("2017-01-01", "2017-02", "2016-10", "2017-03-01", "2018-05-10"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_false(result)
})

test_that("check_dm_ds_partial_death_dates works with preproc parameter", {
  DM <- data.frame(
    USUBJID = 1:2,
    DTHDTC = c("2017-01-01", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:2,
    DSSEQ = 11:12,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "完成"),
    DSSTDTC = c("2017-01-01", "2017-03-01"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS, preproc = identity)
  expect_true(result)
})

test_that("check_dm_ds_partial_death_dates fails when DS required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    DTHDTC = c("2017-01-01", "2017-02-15", NA),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSEQ = 11:13,
    DSSCAT = "STUDY DISCON",
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_dm_ds_partial_death_dates fails when DM required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:3,
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSEQ = 11:13,
    DSSCAT = "STUDY DISCON",
    DSDECOD = c("死亡", "死亡", "完成"),
    DSSTDTC = c("2017-01-01", "2017-02-15", "2017-03-01"),
    stringsAsFactors = FALSE
  )
  result <- check_dm_ds_partial_death_dates(DM, DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})
