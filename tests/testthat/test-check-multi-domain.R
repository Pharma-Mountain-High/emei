# Tests for DS, LB, VS, and other key domain check functions
library(testthat)
library(Emei)
library(dplyr)

# Tests for check_ds_dsdecod_death() ----
test_that("check_ds_dsdecod_death returns pass when death has study discontinuation record", {
  DS <- data.frame(
    STUDYID = "XXX",
    USUBJID = 1:3,
    DSSEQ = 1:3,
    DSDECOD = c(NA, "死亡", NA),
    DSSTDTC = c(NA, "2020-01-01", NA),
    DSCAT = c("处置事件", "处置事件", "其他"),
    DSSCAT = c("研究结束", "研究结束", "研究结束"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_death(DS)
  expect_true(result)
})

test_that("check_ds_dsdecod_death returns fail when death missing study discontinuation", {
  DS <- data.frame(
    STUDYID = "XXX",
    USUBJID = 1:3,
    DSSEQ = 1:3,
    DSDECOD = c(NA, "死亡", NA),
    DSSTDTC = c(NA, "2020-01-01", NA),
    DSCAT = c("处置事件", "处置事件", "其他"),
    DSSCAT = c("研究结束", "治疗结束", "研究结束"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_death(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "STUDY DISCONTINUATION")
})

test_that("check_ds_dsdecod_death works with preproc parameter", {
  DS <- data.frame(
    USUBJID = 1:2,
    DSSEQ = 1:2,
    DSDECOD = c("死亡", NA),
    DSSCAT = c("研究结束", "研究结束"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_death(DS, preproc = identity)
  expect_true(result)
})

test_that("check_ds_dsdecod_death fails when required variables are missing", {
  DS <- data.frame(
    USUBJID = 1:3,
    DSDECOD = c(NA, "死亡", NA),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_death(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbstresu() ----
test_that("check_lb_lbstresu returns pass when all LBSTRESU are non-missing", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = "5",
    LBSTRESN = 1:5,
    LBORRES = "5",
    LBSTRESU = "g/L",
    LBTESTCD = "ALB",
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_true(result)
})

test_that("check_lb_lbstresu returns fail when LBSTRESU is missing with non-missing LBORRES", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = "5",
    LBSTRESN = 1:5,
    LBORRES = "5",
    LBSTRESU = c("g/L", "", "g/L", "g/L", "g/L"),
    LBTESTCD = "ALB",
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing lab units")
})

test_that("check_lb_lbstresu handles NA in LBSTRESU", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = "5",
    LBSTRESN = 1:5,
    LBORRES = "5",
    LBSTRESU = c("g/L", NA, "g/L", "g/L", "g/L"),
    LBTESTCD = "ALB",
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_false(result)
})

test_that("check_lb_lbstresu excludes PH and SPGRAV tests", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = "5",
    LBSTRESN = 1:5,
    LBORRES = c("7.4", "1.020", "5", "5", "5"),
    LBSTRESU = c("", "", "g/L", "g/L", "g/L"),
    LBTESTCD = c("PH", "SPGRAV", "ALB", "ALB", "ALB"),
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_true(result)
})

test_that("check_lb_lbstresu excludes qualitative results", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = c("阴性", "+", "-", "5", "5"),
    LBSTRESN = 1:5,
    LBORRES = c("阴性", "+", "-", "5", "5"),
    LBSTRESU = c("", "", "", "g/L", "g/L"),
    LBTESTCD = c("TEST1", "TEST2", "TEST3", "ALB", "ALB"),
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_true(result)
})

test_that("check_lb_lbstresu works with preproc parameter", {
  LB <- data.frame(
    USUBJID = 1:3,
    LBSTRESC = "5",
    LBSTRESN = 1:3,
    LBORRES = "5",
    LBSTRESU = "g/L",
    LBTESTCD = "ALB",
    LBDTC = paste0("2020-01-0", 1:3),
    LBSEQ = 1:3,
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB, preproc = identity)
  expect_true(result)
})

test_that("check_lb_lbstresu fails when required variables are missing", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBSTRESC = "5",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresu(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_vs_height() ----
test_that("check_vs_height returns pass when all patients have height records", {
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:5,
    RFSTDTC = 1:5,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:5,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = c(170, 165, 180, 175, 160),
    VISIT = "SCREENING",
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_true(result)
})

test_that("check_vs_height returns fail when patients in DM have no height", {
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:5,
    RFSTDTC = 1:5,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:3,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = c(170, 165, 180),
    VISIT = "SCREENING",
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "no recorded height")
})

test_that("check_vs_height handles missing VSSTRESN in height records", {
  DM <- data.frame(
    STUDYID = 1,
    USUBJID = 1:5,
    RFSTDTC = 1:5,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    STUDYID = 1,
    USUBJID = 1:5,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = c(170, NA, 180, "", "."),
    VISIT = "SCREENING",
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_false(result)
})

test_that("check_vs_height handles case insensitive VSTESTCD", {
  DM <- data.frame(
    USUBJID = 1:3,
    RFSTDTC = 1:3,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    USUBJID = 1:3,
    VSTEST = "Height",
    VSTESTCD = "height",
    VSSTRESN = c(170, 165, 180),
    VISIT = "SCREENING",
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_true(result)
})

test_that("check_vs_height fails when VS required variables are missing", {
  DM <- data.frame(
    USUBJID = 1:5,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    USUBJID = 1:5,
    VSTEST = "HEIGHT",
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_vs_height fails when DM required variables are missing", {
  DM <- data.frame(
    STUDYID = 1:5,
    stringsAsFactors = FALSE
  )
  VS <- data.frame(
    USUBJID = 1:5,
    VSTEST = "HEIGHT",
    VSTESTCD = "HEIGHT",
    VSSTRESN = c(170, 165, 180, 175, 160),
    stringsAsFactors = FALSE
  )
  result <- check_vs_height(VS, DM)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_vs_sbp_lt_dbp() ----
test_that("check_vs_sbp_lt_dbp returns pass when SBP >= DBP", {
  VS <- data.frame(
    USUBJID = c(1, 1, 2, 2, 3, 3),
    VSSPID = c("01", "01", "02", "02", "03", "03"),
    VSTESTCD = c("SYSBP", "DIABP", "SYSBP", "DIABP", "SYSBP", "DIABP"),
    VSSTRESN = c(120, 80, 130, 85, 110, 70),
    VSDTC = c("2020-01-01", "2020-01-01", "2020-01-02", "2020-01-02", "2020-01-03", "2020-01-03"),
    VISIT = c("SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING"),
    stringsAsFactors = FALSE
  )
  result <- check_vs_sbp_lt_dbp(VS)
  expect_true(result)
})

test_that("check_vs_sbp_lt_dbp returns fail when SBP < DBP", {
  VS <- data.frame(
    USUBJID = c(1, 1, 2, 2),
    VSSPID = c("01", "01", "02", "02"),
    VSTESTCD = c("SYSBP", "DIABP", "SYSBP", "DIABP"),
    VSSTRESN = c(80, 120, 130, 85),
    VSDTC = c("2020-01-01", "2020-01-01", "2020-01-02", "2020-01-02"),
    VISIT = c("SCREENING", "SCREENING", "WEEK 1", "WEEK 1"),
    stringsAsFactors = FALSE
  )
  result <- check_vs_sbp_lt_dbp(VS)
  expect_false(result)
  expect_match(attr(result, "msg"), "Systolic BP")
})

test_that("check_vs_sbp_lt_dbp handles missing blood pressure values", {
  VS <- data.frame(
    USUBJID = c(1, 1, 2, 2),
    VSSPID = c("01", "01", "02", "02"),
    VSTESTCD = c("SYSBP", "DIABP", "SYSBP", "DIABP"),
    VSSTRESN = c(120, NA, 130, 85),
    VSDTC = c("2020-01-01", "2020-01-01", "2020-01-02", "2020-01-02"),
    VISIT = c("SCREENING", "SCREENING", "WEEK 1", "WEEK 1"),
    stringsAsFactors = FALSE
  )
  result <- check_vs_sbp_lt_dbp(VS)
  expect_true(result)
})

test_that("check_vs_sbp_lt_dbp fails when required variables are missing", {
  VS <- data.frame(
    USUBJID = 1:5,
    VSTESTCD = c("SYSBP", "DIABP", "SYSBP", "DIABP", "SYSBP"),
    stringsAsFactors = FALSE
  )
  result <- check_vs_sbp_lt_dbp(VS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_exdose_exoccur() ----
test_that("check_ex_exdose_exoccur returns pass when dose and occurrence are consistent", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = "DRUG A",
    EXDOSE = c(100, 200, 100, 200, 100),
    EXOCCUR = c("是", "是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exdose_exoccur(EX)
  expect_true(result)
})

test_that("check_ex_exdose_exoccur returns pass when EXDOSE is zero and EXOCCUR is empty", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = "DRUG A",
    EXDOSE = c(100, 200, 100, 0, 100),
    EXOCCUR = c("是", "是", "是", "", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exdose_exoccur(EX)
  expect_true(result)
})

test_that("check_ex_exdose_exoccur returns fail when EXOCCUR is Y but EXDOSE is missing", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = "DRUG A",
    EXDOSE = c(100, 200, NA, 100, NA),
    EXOCCUR = c("是", "是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exdose_exoccur(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "EXDOSE")
})

test_that("check_ex_exdose_exoccur fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exdose_exoccur(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_qs_dup() ----
test_that("check_qs_dup returns pass when no duplicates exist", {
  QS <- data.frame(
    USUBJID = 1:5,
    QSCAT = "QUESTIONNAIRE",
    QSTESTCD = c("Q1", "Q2", "Q3", "Q4", "Q5"),
    QSDTC = rep("2020-01-01", 5),
    VISIT = "SCREENING",
    stringsAsFactors = FALSE
  )
  result <- check_qs_dup(QS)
  expect_true(result)
})

test_that("check_qs_dup returns fail when duplicates exist", {
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 3),
    QSCAT = "QUESTIONNAIRE",
    QSTESTCD = c("Q1", "Q1", "Q1", "Q2", "Q3"),
    QSDTC = c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-01", "2020-01-01"),
    VISIT = c("SCREENING", "SCREENING", "SCREENING", "SCREENING", "SCREENING"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_dup(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "Multiple")
})

test_that("check_qs_dup handles multiple visits correctly", {
  QS <- data.frame(
    USUBJID = c(1, 1, 2, 2),
    QSCAT = "QUESTIONNAIRE",
    QSTESTCD = c("Q1", "Q1", "Q2", "Q2"),
    QSDTC = c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01"),
    VISIT = c("SCREENING", "WEEK 1", "SCREENING", "WEEK 1"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_dup(QS)
  expect_true(result)
})

test_that("check_qs_dup fails when required variables are missing", {
  QS <- data.frame(
    USUBJID = 1:5,
    QSCAT = "QUESTIONNAIRE",
    stringsAsFactors = FALSE
  )
  result <- check_qs_dup(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_ae_discon() ----
test_that("check_ds_ae_discon returns pass when DS discon matches AE drug withdrawn", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    AEDECOD = c("头痛", "恶心", "皮疹"),
    AEACN = c("永久停药", "剂量不变", "不适用"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    DSCAT = rep("处置事件", 3),
    DSSCAT = rep("治疗结束", 3),
    DSDECOD = c("不良事件", "完成", "完成"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_true(result)
})

test_that("check_ds_ae_discon returns fail when DS discon due to AE but no matching AEACN", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    AEDECOD = c("头痛", "恶心", "皮疹"),
    AEACN = c("永久停药", "剂量不变", "不适用"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    DSCAT = rep("处置事件", 3),
    DSSCAT = rep("治疗结束", 3),
    DSDECOD = c("不良事件", "不良事件", "完成"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_false(result)
  expect_match(attr(result, "msg"), "drug withdrawn")
})

test_that("check_ds_ae_discon handles multiple AEACNx variables", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    AEDECOD = c("头痛", "恶心", "皮疹"),
    AEACN = rep("MULTIPLE", 3),
    AEACN1 = c("永久停药", "剂量不变", "不适用"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    DSCAT = rep("处置事件", 3),
    DSSCAT = rep("治疗结束", 3),
    DSDECOD = c("不良事件", "完成", "完成"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_true(result)
})

test_that("check_ds_ae_discon handles DSTERM variable", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    AEDECOD = c("头痛", "恶心", "皮疹"),
    AEACN = c("永久停药", "剂量不变", "不适用"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    DSCAT = rep("处置事件", 3),
    DSSCAT = rep("治疗结束", 3),
    DSDECOD = c("其他", "完成", "完成"),
    DSTERM = c("不良事件", "完成研究", "完成研究"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_true(result)
})

test_that("check_ds_ae_discon fails when AE required variables are missing", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    DSCAT = rep("处置事件", 3),
    DSSCAT = rep("治疗结束", 3),
    DSDECOD = c("不良事件", "完成", "完成"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ds_ae_discon fails when DS required variables are missing", {
  AE <- data.frame(
    USUBJID = 1:3,
    AESTDTC = "2017-01-01",
    AETERM = c("头痛", "恶心", "皮疹"),
    AEDECOD = c("头痛", "恶心", "皮疹"),
    AEACN = c("永久停药", "剂量不变", "不适用"),
    stringsAsFactors = FALSE
  )
  DS <- data.frame(
    USUBJID = 1:3,
    DSSTDTC = "2017-01-01",
    stringsAsFactors = FALSE
  )
  result <- check_ds_ae_discon(DS, AE)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbstresn_missing() ----
test_that("check_lb_lbstresn_missing returns pass when all LBSTRESN values are present", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBTESTCD = "ALB",
    LBORRES = c("5.0", "5.5", "4.8", "5.2", "4.9"),
    LBORRESU = "g/dL",
    LBSTRESN = c(5.0, 5.5, 4.8, 5.2, 4.9),
    LBSTRESC = c("5.0", "5.5", "4.8", "5.2", "4.9"),
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresn_missing(LB)
  expect_true(result)
})

test_that("check_lb_lbstresn_missing returns fail when LBSTRESN has missing values", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBTESTCD = "ALB",
    LBORRES = c("5.0", "5.5", "4.8", "5.2", "4.9"),
    LBORRESU = "g/dL",
    LBSTRESN = c(5.0, NA, 4.8, NA, 4.9),
    LBSTRESC = c("5.0", "", "4.8", "", "4.9"),
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresn_missing(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "result")
})

test_that("check_lb_lbstresn_missing excludes qualitative results", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBTESTCD = c("ALB", "GLU", "TEST1", "TEST2", "ALB"),
    LBORRES = c("5.0", "5.5", "阳性", "阴性", "4.9"),
    LBORRESU = c("g/dL", "mg/dL", "", "", "g/dL"),
    LBSTRESN = c(5.0, 5.5, NA, NA, 4.9),
    LBSTRESC = c("5.0", "5.5", "阳性", "阴性", "4.9"),
    LBDTC = paste0("2020-01-0", 1:5),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresn_missing(LB)
  expect_true(result)
})

test_that("check_lb_lbstresn_missing fails when required variables are missing", {
  LB <- data.frame(
    USUBJID = 1:5,
    LBTESTCD = "ALB",
    LBORRES = c("5.0", "5.5", "4.8", "5.2", "4.9"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresn_missing(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_extrt_exoccur() ----
test_that("check_ex_extrt_exoccur returns pass when treatment and occurrence are consistent", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = c("DRUG A", "DRUG B", "DRUG A", "DRUG B", "DRUG A"),
    EXDOSE = c(10, 20, 10, 20, 10),
    EXOCCUR = c("是", "是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_extrt_exoccur(EX)
  expect_true(result)
})

test_that("check_ex_extrt_exoccur returns fail when EXTRT missing but EXOCCUR is Y", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = c("DRUG A", "DRUG B", "", "DRUG B", NA),
    EXDOSE = c(10, 20, 0, 20, 0),
    EXOCCUR = c("是", "是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_extrt_exoccur(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "Missing")
})

test_that("check_ex_extrt_exoccur handles missing EXTRT with EXOCCUR N", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    EXTRT = c("DRUG A", "", "DRUG A", "DRUG B", NA),
    EXDOSE = c(10, 0, 10, 20, 0),
    EXOCCUR = c("是", "", "是", "是", ""),
    stringsAsFactors = FALSE
  )
  result <- check_ex_extrt_exoccur(EX)
  expect_true(result)
})

test_that("check_ex_extrt_exoccur fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 1:5,
    EXSTDTC = rep("2020-01-01", 5),
    stringsAsFactors = FALSE
  )
  result <- check_ex_extrt_exoccur(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_dsdecod_dsstdtc() ----
test_that("check_ds_dsdecod_dsstdtc returns pass when all death records have dates", {
  DS <- data.frame(
    STUDYID = rep(1, 4),
    USUBJID = c(1, 1, 2, 3),
    DSDECOD = c("死亡", "死亡相关", "完成", "不良事件"),
    DSSCAT = c("处置事件", "处置事件", "方案里程碑", "受试者分布事件"),
    DSSTDTC = c(NA, "2016-01-01", "2016-01-03", "2016-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_dsstdtc(DS)
  expect_true(result)
})

test_that("check_ds_dsdecod_dsstdtc returns fail when death record has no date", {
  DS <- data.frame(
    STUDYID = rep(1, 4),
    USUBJID = c(1, 1, 2, 3),
    DSDECOD = c("死亡", "死亡相关", "完成", "不良事件"),
    DSSCAT = c("处置事件", "处置事件", "方案里程碑", "受试者分布事件"),
    DSSTDTC = c(NA, NA, "2016-01-03", "2016-01-02"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_dsstdtc(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "no death date records")
})

test_that("check_ds_dsdecod_dsstdtc fails when required variables are missing", {
  DS <- data.frame(
    USUBJID = c(1, 1, 2, 3),
    DSDECOD = c("死亡", "死亡相关", "完成", "不良事件"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsdecod_dsstdtc(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_dsscat() ----
test_that("check_ds_dsscat returns pass when patients have unique study discontinuation", {
  DS <- data.frame(
    USUBJID = c(rep(1, 3), rep(2, 3), rep(3, 3)),
    DSSCAT = rep(c("研究结束", "不良事件", "方案"), 3),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsscat(DS)
  expect_true(result)
})

test_that("check_ds_dsscat returns fail when patient has duplicate study discontinuation", {
  DS <- data.frame(
    USUBJID = c(rep(1, 3), rep(2, 3), rep(3, 3)),
    DSSCAT = rep(c("研究结束", "不良事件", "方案"), 3),
    stringsAsFactors = FALSE
  )
  DS$DSSCAT[8] <- "研究结束"
  result <- check_ds_dsscat(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "more than one")
})

test_that("check_ds_dsscat fails when no study discontinuation records exist", {
  DS <- data.frame(
    USUBJID = c(1, 2, 3),
    DSSCAT = c("治疗结束", "不良事件", "方案"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsscat(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "no study discontinuation records")
})

test_that("check_ds_dsscat fails when required variables are missing", {
  DS <- data.frame(
    USUBJID = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- check_ds_dsscat(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_duplicate_randomization() ----
test_that("check_ds_duplicate_randomization returns pass when no duplicate randomization", {
  DS <- data.frame(
    USUBJID = c("ID1", "ID1", "ID2", "ID2", "ID3", "ID3"),
    DSDECOD = c("随机", "OTHER THING", "随机", "OTHER THING", "OTHER", "OTHER"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_duplicate_randomization(DS)
  expect_true(result)
})

test_that("check_ds_duplicate_randomization returns fail when patient has duplicate randomization", {
  DS <- data.frame(
    USUBJID = c("ID1", "ID1", "ID2", "ID2", "ID3", "ID3"),
    DSDECOD = c("随机", "OTHER THING", "随机", "OTHER THING", "随机", "随机"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_duplicate_randomization(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "duplicate randomization")
})

test_that("check_ds_duplicate_randomization returns pass when no randomization records", {
  DS <- data.frame(
    USUBJID = c("ID1", "ID1", "ID2", "ID2"),
    DSDECOD = c("OTHER THING", "ANOTHER", "THING", "STUFF"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_duplicate_randomization(DS)
  expect_true(result)
})

test_that("check_ds_duplicate_randomization fails when required variables are missing", {
  DS <- data.frame(
    USUBJID = c("ID1", "ID2", "ID3"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_duplicate_randomization(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_ex_after_discon() ----
test_that("check_ds_ex_after_discon returns pass when all exposure dates before discontinuation", {
  DS <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    DSSCAT = rep(c("研究结束", "入组信息"), 2),
    DSCAT = rep(c("处置事件", "其他"), 2),
    DSSTDTC = c("2019-12-29", "2019-12-20", "2019-12-10", "2019-12-01"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    EXSTDTC = c("2019-12-20", "2019-12-28", "2019-12-01", "2019-12-02"),
    EXENDTC = c("2019-12-10", "2019-12-23", "2019-12-05", "2019-12-08"),
    EXTRT = c(rep("药物A", 2), rep("安慰剂", 2)),
    EXDOSE = c(10, 10, 0, 0),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ex_after_discon(DS, EX)
  expect_true(result)
})

test_that("check_ds_ex_after_discon returns fail when exposure date after discontinuation", {
  DS <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    DSSCAT = rep(c("研究结束", "入组信息"), 2),
    DSCAT = rep(c("处置事件", "其他"), 2),
    DSSTDTC = c("2019-12-29", "2019-12-20", "2019-12-10", "2019-12-01"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    EXSTDTC = c("2019-12-20", "2019-12-28", "2019-12-26", "2019-12-27"),
    EXENDTC = c("2019-12-10", "2019-12-23", "2019-12-30", "2019-12-27"),
    EXTRT = c(rep("药物A", 2), rep("安慰剂", 2)),
    EXDOSE = c(10, 10, 0, 0),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ex_after_discon(DS, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "after treatment/study discontinuation")
})

test_that("check_ds_ex_after_discon handles EXOCCUR parameter", {
  DS <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    DSSCAT = rep(c("研究结束", "入组信息"), 2),
    DSCAT = rep(c("处置事件", "其他"), 2),
    DSSTDTC = c("2019-12-29", "2019-12-20", "2019-12-10", "2019-12-01"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = c(rep(1, 2), rep(2, 2)),
    EXSTDTC = c("2019-12-20", "2019-12-28", "2019-12-01", "2019-12-02"),
    EXENDTC = c("2019-12-10", "2019-12-23", "2019-12-05", "2019-12-08"),
    EXTRT = c(rep("药物A", 2), rep("安慰剂", 2)),
    EXDOSE = c(10, 10, 0, 0),
    EXOCCUR = c("是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ex_after_discon(DS, EX)
  expect_true(result)
})

test_that("check_ds_ex_after_discon fails when DS required variables are missing", {
  DS <- data.frame(
    USUBJID = c(1, 2),
    DSSCAT = c("研究结束", "研究结束"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = c(1, 2),
    EXSTDTC = c("2019-12-20", "2019-12-28"),
    EXENDTC = c("2019-12-10", "2019-12-23"),
    EXTRT = c("药物A", "药物A"),
    EXDOSE = c(10, 10),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ex_after_discon(DS, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_ds_ex_after_discon fails when EX required variables are missing", {
  DS <- data.frame(
    USUBJID = c(1, 2),
    DSSCAT = c("研究结束", "研究结束"),
    DSCAT = c("处置事件", "处置事件"),
    DSSTDTC = c("2019-12-29", "2019-12-10"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = c(1, 2),
    EXSTDTC = c("2019-12-20", "2019-12-28"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_ex_after_discon(DS, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ds_multdeath_dsstdtc() ----
test_that("check_ds_multdeath_dsstdtc returns pass when death dates match", {
  DS <- data.frame(
    STUDYID = rep(1, 6),
    USUBJID = c(1, 1, 1, 2, 1, 1),
    DSSEQ = c(1, 2, 3, 4, 5, 6),
    DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
    DSSCAT = LETTERS[1:6],
    DSSTDTC = c("", "2016-01-01", "", "", "2016-01-01", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_multdeath_dsstdtc(DS)
  expect_true(result)
})

test_that("check_ds_multdeath_dsstdtc returns fail when death dates do not match", {
  DS <- data.frame(
    STUDYID = rep(1, 6),
    USUBJID = c(1, 1, 1, 2, 1, 1),
    DSSEQ = c(1, 2, 3, 4, 5, 6),
    DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
    DSSCAT = LETTERS[1:6],
    DSSTDTC = c("", "2016-01-01", "", "", "2016-01-02", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_multdeath_dsstdtc(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "do not match")
})

test_that("check_ds_multdeath_dsstdtc handles partial dates", {
  DS <- data.frame(
    STUDYID = rep(1, 6),
    USUBJID = c(1, 1, 1, 2, 1, 1),
    DSSEQ = c(1, 2, 3, 4, 5, 6),
    DSDECOD = c("死亡", "死亡", rep("", 2), "死亡", "死亡"),
    DSSCAT = LETTERS[1:6],
    DSSTDTC = c("", "2016-01", "", "", "2016-01-01", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_multdeath_dsstdtc(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "do not match")
})

test_that("check_ds_multdeath_dsstdtc works with preproc parameter", {
  DS <- data.frame(
    USUBJID = c(1, 1, 2),
    DSDECOD = c("死亡", "死亡", "完成"),
    DSSTDTC = c("2016-01-01", "2016-01-01", "2016-01-03"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_multdeath_dsstdtc(DS, preproc = identity)
  expect_true(result)
})

test_that("check_ds_multdeath_dsstdtc fails when required variables are missing", {
  DS <- data.frame(
    USUBJID = c(1, 1, 2),
    DSDECOD = c("死亡", "死亡", "完成"),
    stringsAsFactors = FALSE
  )
  result <- check_ds_multdeath_dsstdtc(DS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbdtc_after_dd() ----
test_that("check_lb_lbdtc_after_dd returns pass when no LB dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  LB <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    LBDTC = rep("2015-12-31", 5),
    LBTESTCD = letters[1:5],
    LBORRES = 1:5,
    LBSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_after_dd(DM, LB)
  expect_true(result)
})

test_that("check_lb_lbdtc_after_dd returns fail when LB dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  LB <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    LBDTC = rep("2015-12-31", 5),
    LBTESTCD = letters[1:5],
    LBORRES = 1:5,
    LBSTAT = "",
    stringsAsFactors = FALSE
  )
  LB$LBDTC[1] <- "2016-01-03"
  LB$USUBJID[1] <- LB$USUBJID[5]
  result <- check_lb_lbdtc_after_dd(DM, LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "after death date")
})

test_that("check_lb_lbdtc_after_dd handles LBSTAT filtering", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  LB <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    LBDTC = rep("2015-12-31", 5),
    LBTESTCD = letters[1:5],
    LBORRES = 1:5,
    LBSTAT = "",
    stringsAsFactors = FALSE
  )
  LB$LBDTC[1] <- "2016-01-03"
  LB$USUBJID[1] <- LB$USUBJID[5]
  LB$LBSTAT[1] <- "未查"
  result <- check_lb_lbdtc_after_dd(DM, LB)
  expect_true(result)
})

test_that("check_lb_lbdtc_after_dd returns pass when no death dates", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = rep("", 5),
    stringsAsFactors = FALSE
  )
  LB <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    LBDTC = rep("2015-12-31", 5),
    LBTESTCD = letters[1:5],
    LBORRES = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_after_dd(DM, LB)
  expect_true(result)
})

test_that("check_lb_lbdtc_after_dd fails when required variables are missing", {
  DM <- data.frame(
    USUBJID = LETTERS[1:5],
    stringsAsFactors = FALSE
  )
  LB <- data.frame(
    USUBJID = LETTERS[1:5],
    LBDTC = rep("2015-12-31", 5),
    LBTESTCD = letters[1:5],
    LBORRES = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_after_dd(DM, LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbdtc_visit_ordinal_error() ----
test_that("check_lb_lbdtc_visit_ordinal_error returns pass when dates are in order", {
  LB <- data.frame(
    USUBJID = c(rep("101", 5), rep("102", 5)),
    LBCAT = "Hematology",
    LBDTC = rep(c(
      "2017-01-01T08:25",
      "2017-01-05T09:25",
      "2017-01-15T10:25",
      "2017-01-20T08:25",
      "2017-01-25T08:25"
    ), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "计划外访视", "C4/D1"), 2),
    LBSTAT = c(rep("", 9), "未查"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_visit_ordinal_error(LB)
  expect_true(result)
})

test_that("check_lb_lbdtc_visit_ordinal_error returns fail when dates are out of order", {
  LB <- data.frame(
    USUBJID = c(rep("101", 5), rep("102", 5)),
    LBCAT = "Hematology",
    LBDTC = rep(c(
      "2017-01-01T08:25",
      "2017-01-05T09:25",
      "2017-01-15T10:25",
      "2017-01-20T08:25",
      "2017-01-25T08:25"
    ), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "计划外访视", "C4/D1"), 2),
    LBSTAT = "",
    stringsAsFactors = FALSE
  )
  LB$LBDTC[LB$USUBJID == "101" & LB$VISIT == "C3/D1"] <- "2016-01-10T08:25"
  result <- check_lb_lbdtc_visit_ordinal_error(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "data entry error")
})

test_that("check_lb_lbdtc_visit_ordinal_error handles duplicated dates", {
  LB <- data.frame(
    USUBJID = c(rep("101", 5)),
    LBCAT = "Hematology",
    LBDTC = c(
      "2017-01-01T08:25",
      "2017-01-05T09:25",
      "2017-01-15T10:25",
      "2017-01-15T10:25",
      "2017-01-25T08:25"
    ),
    VISITNUM = 1:5,
    VISIT = c("C1/D1", "C2/D1", "C3/D1", "C4/D1", "C5/D1"),
    LBSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_visit_ordinal_error(LB)
  expect_false(result)
})

test_that("check_lb_lbdtc_visit_ordinal_error fails when required variables are missing", {
  LB <- data.frame(
    USUBJID = c(rep("101", 5)),
    LBDTC = c(
      "2017-01-01T08:25",
      "2017-01-05T09:25",
      "2017-01-15T10:25",
      "2017-01-20T08:25",
      "2017-01-25T08:25"
    ),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbdtc_visit_ordinal_error(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbstnrlo_lbstnrhi() ----
test_that("check_lb_lbstnrlo_lbstnrhi returns pass when reference ranges present", {
  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    LBSTRESN = 1:10,
    LBSTNRLO = 1:10,
    LBSTNRHI = 1:10,
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = "1",
    SITEID = "123456",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstnrlo_lbstnrhi(DM, LB)
  expect_true(result)
})

test_that("check_lb_lbstnrlo_lbstnrhi returns fail when reference ranges missing", {
  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    LBSTRESN = 1:10,
    LBSTNRLO = 1:10,
    LBSTNRHI = 1:10,
    stringsAsFactors = FALSE
  )
  LB$LBSTNRLO[1] <- ""
  LB$LBSTNRLO[2] <- "NA"
  LB$LBSTNRLO[3] <- NA
  LB$LBSTNRHI[3] <- ""
  LB$LBSTNRHI[4] <- "NA"
  LB$LBSTNRHI[5] <- NA
  DM <- data.frame(
    USUBJID = "1",
    SITEID = "123456",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstnrlo_lbstnrhi(DM, LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing reference range")
})

test_that("check_lb_lbstnrlo_lbstnrhi fails when LB required variables are missing", {
  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = "1",
    SITEID = "123456",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstnrlo_lbstnrhi(DM, LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

test_that("check_lb_lbstnrlo_lbstnrhi fails when DM required variables are missing", {
  LB <- data.frame(
    USUBJID = "1",
    LBTEST = "Albumin",
    LBSTRESN = 1:10,
    LBSTNRLO = 1:10,
    LBSTNRHI = 1:10,
    stringsAsFactors = FALSE
  )
  DM <- data.frame(
    USUBJID = "1",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstnrlo_lbstnrhi(DM, LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_lbstresc_char() ----
test_that("check_lb_lbstresc_char returns pass when no character issues", {
  LB <- data.frame(
    USUBJID = c("Patient 1", "Patient 2", "Patient 3"),
    LBTEST = "Test A",
    LBDTC = "2017-01-01",
    LBORRES = c("5", "3", "7"),
    LBORRESU = rep("mg", 3),
    LBSTRESC = c("5", "3", "7"),
    LBSTRESN = c(5, 3, 7),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresc_char(LB)
  expect_true(result)
})

test_that("check_lb_lbstresc_char returns fail when LBSTRESC has < or > characters", {
  LB <- data.frame(
    USUBJID = c("Patient 1", "Patient 2", "Patient 3"),
    LBTEST = rep("Test A", 3),
    LBDTC = "2017-01-01",
    LBORRES = c("5", "3", "<7"),
    LBORRESU = rep("mg", 3),
    LBSTRESC = c("5", "3", "<7"),
    LBSTRESN = c(5, 3, NA),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresc_char(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "beginning with character")
})

test_that("check_lb_lbstresc_char handles BLQ values correctly", {
  LB <- data.frame(
    USUBJID = c("Patient 1", "Patient 2", "Patient 3"),
    LBTEST = rep("Test A", 3),
    LBDTC = rep("2017-01-01", 3),
    LBORRES = c("5", "BLQ", "<7"),
    LBORRESU = rep("mg", 3),
    LBSTRESC = c("5", "BLQ", "<7"),
    LBSTRESN = c(5, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresc_char(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "beginning with character")
})

test_that("check_lb_lbstresc_char fails when required variables are missing", {
  LB <- data.frame(
    USUBJID = c("Patient 1", "Patient 2", "Patient 3"),
    LBTEST = "Test A",
    stringsAsFactors = FALSE
  )
  result <- check_lb_lbstresc_char(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_lb_missing_month() ----
test_that("check_lb_missing_month returns pass when no missing months", {
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1", "TEST2", "TEST3", "TEST3"),
    LBDTC = c("2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01"),
    VISIT = c("VISIT1", "VISIT2", "VISIT3", "VISIT3"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_missing_month(LB)
  expect_true(result)
})

test_that("check_lb_missing_month returns fail when month is missing", {
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1", "TEST2", "TEST3", "TEST3"),
    LBDTC = c("2017-01-01", "2017-02-01", "2017---01", "2017----01"),
    VISIT = c("VISIT1", "VISIT2", "VISIT3", "VISIT3"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_missing_month(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing month")
})

test_that("check_lb_missing_month works with preproc parameter", {
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1", "TEST2", "TEST3", "TEST3"),
    LBDTC = c("2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01"),
    VISIT = c("VISIT1", "VISIT2", "VISIT3", "VISIT3"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_missing_month(LB, preproc = identity)
  expect_true(result)
})

test_that("check_lb_missing_month fails when required variables are missing", {
  LB <- data.frame(
    USUBJID = 1:4,
    LBTEST = c("TEST1", "TEST2", "TEST3", "TEST3"),
    stringsAsFactors = FALSE
  )
  result <- check_lb_missing_month(LB)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_exoccur_exdose_exstdtc() ----
test_that("check_ex_exoccur_exdose_exstdtc returns pass when all doses and dates valid", {
  EX <- data.frame(
    USUBJID = LETTERS[1:6],
    VISIT = paste0("Visit ", 1:6),
    VISITNUM = 1:6,
    EXTRT = LETTERS[1:6],
    EXDOSE = 1:6,
    EXSTDTC = rep("2010-01-01", 6),
    EXENDTC = rep("2010-01-01", 6),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exoccur_exdose_exstdtc(EX)
  expect_true(result)
})

test_that("check_ex_exoccur_exdose_exstdtc returns fail when dose or date invalid", {
  EX <- data.frame(
    USUBJID = LETTERS[1:6],
    VISIT = paste0("Visit ", 1:6),
    VISITNUM = 1:6,
    EXTRT = LETTERS[1:6],
    EXDOSE = 1:6,
    EXSTDTC = c("2010-01-01", rep("", 5)),
    EXENDTC = c("2010-01-01", rep("", 5)),
    stringsAsFactors = FALSE
  )
  EX$EXSTDTC[2] <- "2011"
  EX$EXDOSE[1] <- 0
  result <- check_ex_exoccur_exdose_exstdtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "invalid dosing")
})

test_that("check_ex_exoccur_exdose_exstdtc handles EXOCCUR parameter", {
  EX <- data.frame(
    USUBJID = LETTERS[1:6],
    VISIT = paste0("Visit ", 1:6),
    VISITNUM = 1:6,
    EXTRT = LETTERS[1:6],
    EXDOSE = 1:6,
    EXSTDTC = rep("2010-01-01", 6),
    EXENDTC = rep("2010-01-01", 6),
    EXOCCUR = rep("是", 6),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exoccur_exdose_exstdtc(EX)
  expect_true(result)
})

test_that("check_ex_exoccur_exdose_exstdtc fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = LETTERS[1:6],
    EXTRT = LETTERS[1:6],
    stringsAsFactors = FALSE
  )
  result <- check_ex_exoccur_exdose_exstdtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_exstdtc_after_exendtc() ----
test_that("check_ex_exstdtc_after_exendtc returns pass when start dates before end dates", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    EXSTDTC = c("2017-01-01", "2017-01-03", "2017-01-01T14:26"),
    EXENDTC = c("2017-01-01", "2017-01-04", "2017-01-01T14:27"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_exendtc(EX)
  expect_true(result)
})

test_that("check_ex_exstdtc_after_exendtc returns fail when start date after end date", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    EXSTDTC = c("2017-01-01", "2017-01-03", "2017-01-01T14:26"),
    EXENDTC = c("2017-01-01", "2017-01-02", "2017-01-01T14:25"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_exendtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "after EXENDTC")
})

test_that("check_ex_exstdtc_after_exendtc handles partial dates correctly", {
  EX <- data.frame(
    USUBJID = "C",
    EXTRT = "SOME DRUG",
    EXSTDTC = "2017-01",
    EXENDTC = "2017-01-31",
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_exendtc(EX)
  expect_true(result)
})

test_that("check_ex_exstdtc_after_exendtc fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_exendtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_exstdtc_after_dd() ----
test_that("check_ex_exstdtc_after_dd returns pass when no EX dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    EXSTDTC = rep("2015-12-31", 5),
    EXTRT = LETTERS[1:5],
    EXDOSE = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_dd(DM, EX)
  expect_true(result)
})

test_that("check_ex_exstdtc_after_dd returns fail when EX dates after death", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = c(rep("", 4), "2016-01-02"),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    EXSTDTC = rep("2015-12-31", 5),
    EXTRT = LETTERS[1:5],
    EXDOSE = 1:5,
    stringsAsFactors = FALSE
  )
  EX$EXSTDTC[1] <- "2016-01-03"
  EX$USUBJID[1] <- EX$USUBJID[5]
  result <- check_ex_exstdtc_after_dd(DM, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "after death date")
})

test_that("check_ex_exstdtc_after_dd returns pass when no death dates", {
  DM <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    DTHDTC = rep("", 5),
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    STUDYID = 1:5,
    USUBJID = LETTERS[1:5],
    EXSTDTC = rep("2015-12-31", 5),
    EXTRT = LETTERS[1:5],
    EXDOSE = 1:5,
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_dd(DM, EX)
  expect_true(result)
})

test_that("check_ex_exstdtc_after_dd fails when required variables are missing", {
  DM <- data.frame(
    USUBJID = LETTERS[1:5],
    stringsAsFactors = FALSE
  )
  EX <- data.frame(
    USUBJID = LETTERS[1:5],
    EXSTDTC = rep("2015-12-31", 5),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_after_dd(DM, EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_infusion_exstdtc_exendtc() ----
test_that("check_ex_infusion_exstdtc_exendtc returns pass when infusion dates valid", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    EXROUTE = "INTRAVENOUS",
    EXSTDTC = c("2017-01-01", "2017-01-01T14:26", "2017-01-01T14:26"),
    EXENDTC = c("2017-01-01", "2017-01-01", "2017-01"),
    EXOCCUR = "是",
    VISIT = "CYCLE 1 DAY 1",
    stringsAsFactors = FALSE
  )
  result <- check_ex_infusion_exstdtc_exendtc(EX)
  expect_true(result)
})

test_that("check_ex_infusion_exstdtc_exendtc returns fail when dates different", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    EXROUTE = "INTRAVENOUS",
    EXSTDTC = c("2017-01-01", "2017-01-02", "2017-01-01T14:36"),
    EXENDTC = c("2017-01-01", "2017-01-03", "2017-01-01T14:35"),
    EXOCCUR = "是",
    VISIT = "CYCLE 1 DAY 1",
    stringsAsFactors = FALSE
  )
  result <- check_ex_infusion_exstdtc_exendtc(EX)
  expect_false(result)
})

test_that("check_ex_infusion_exstdtc_exendtc returns fail when missing dates", {
  EX <- data.frame(
    USUBJID = 1:4,
    EXTRT = "SOME DRUG",
    EXROUTE = "INTRAVENOUS",
    EXSTDTC = c("2017-01-03", "", "2017-02-01T14:26", ""),
    EXENDTC = c("", "2017-02-03", "", "2017-02-02T14:26:02"),
    EXOCCUR = "是",
    VISIT = "CYCLE 1 DAY 1",
    stringsAsFactors = FALSE
  )
  result <- check_ex_infusion_exstdtc_exendtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "issue")
})

test_that("check_ex_infusion_exstdtc_exendtc fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = "SOME DRUG",
    stringsAsFactors = FALSE
  )
  result <- check_ex_infusion_exstdtc_exendtc(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_exstdtc_visit_ordinal_error() ----
test_that("check_ex_exstdtc_visit_ordinal_error returns pass when dates in order", {
  EX <- data.frame(
    USUBJID = 101:102,
    EXTRT = rep(c("A", "B"), 5),
    EXSTDTC = rep(c(
      "2017-01-01T08:25", "2017-01-05T09:25",
      "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
    ), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "C4/D1", "计划外"), 2),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_visit_ordinal_error(EX)
  expect_true(result)
})

test_that("check_ex_exstdtc_visit_ordinal_error returns fail when dates out of order", {
  EX <- data.frame(
    USUBJID = 101:102,
    EXTRT = rep(c("A", "B"), 5),
    EXSTDTC = rep(c(
      "2017-01-01T08:25", "2017-01-05T09:25",
      "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"
    ), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "C4/D1", "计划外"), 2),
    stringsAsFactors = FALSE
  )
  EX$EXSTDTC[EX$USUBJID == 101 & EX$VISIT == "C4/D1"] <- "2017-01-10T08:25"
  result <- check_ex_exstdtc_visit_ordinal_error(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "data entry error")
})

test_that("check_ex_exstdtc_visit_ordinal_error fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 101:102,
    EXTRT = rep(c("A", "B"), 5),
    stringsAsFactors = FALSE
  )
  result <- check_ex_exstdtc_visit_ordinal_error(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_ex_visit() ----
test_that("check_ex_visit returns pass when all visits present", {
  EX <- data.frame(
    USUBJID = 1:2,
    EXTRT = c("A", "B"),
    EXSTDTC = c("2020-01-01", "2020-01-02"),
    EXOCCUR = c("是", "是"),
    VISIT = c("C1D1", "C2D1"),
    stringsAsFactors = FALSE
  )
  result <- check_ex_visit(EX)
  expect_true(result)
})

test_that("check_ex_visit returns fail when visits missing", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = 1:3,
    EXSTDTC = 1:3,
    EXOCCUR = "是",
    VISIT = NA,
    stringsAsFactors = FALSE
  )
  result <- check_ex_visit(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "Total number of records")
})

test_that("check_ex_visit handles missing EXOCCUR parameter", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = 1:3,
    EXSTDTC = 1:3,
    VISIT = NA,
    stringsAsFactors = FALSE
  )
  result <- check_ex_visit(EX)
  expect_false(result)
})

test_that("check_ex_visit fails when required variables are missing", {
  EX <- data.frame(
    USUBJID = 1:3,
    EXTRT = 1:3,
    stringsAsFactors = FALSE
  )
  result <- check_ex_visit(EX)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_qs_qsdtc_after_dd() ----
test_that("check_qs_qsdtc_after_dd returns pass when no QS dates after death", {
  DM <- data.frame(
    USUBJID = c(1, 2),
    DTHDTC = c("2016-01-01", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 2, 2),
    QSDTC = c("2015-06-30", "2015-09-30", "2015-12-30", "2015-06-30", "2015-09-30", "2015-12-30"),
    QSCAT = "A",
    QSTESTCD = "ECOG",
    QSORRES = LETTERS[1:6],
    QSSTAT = "",
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_after_dd(DM, QS)
  expect_true(result)
})

test_that("check_qs_qsdtc_after_dd returns fail when QS dates after death", {
  DM <- data.frame(
    USUBJID = c(1, 2),
    DTHDTC = c("2016-01-01", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 2, 2),
    QSDTC = c("2015-06-30", "2015-09-30", "2015-12-30", "2015-06-30", "2015-09-30", "2015-12-30"),
    QSCAT = "A",
    QSTESTCD = "ECOG",
    QSORRES = LETTERS[1:6],
    QSSTAT = "",
    stringsAsFactors = FALSE
  )
  QS$QSDTC[3:5] <- "2016-01-03"
  result <- check_qs_qsdtc_after_dd(DM, QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "after death date")
})

test_that("check_qs_qsdtc_after_dd handles QSSTAT filtering", {
  DM <- data.frame(
    USUBJID = c(1, 2),
    DTHDTC = c("2016-01-01", "2016-01-01"),
    stringsAsFactors = FALSE
  )
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 2, 2),
    QSDTC = c("2015-06-30", "2015-09-30", "2016-01-03", "2015-06-30", "2015-09-30", "2015-12-30"),
    QSCAT = "A",
    QSTESTCD = "ECOG",
    QSORRES = LETTERS[1:6],
    QSSTAT = "",
    stringsAsFactors = FALSE
  )
  QS$QSSTAT[3] <- "未查"
  result <- check_qs_qsdtc_after_dd(DM, QS)
  expect_true(result)
})

test_that("check_qs_qsdtc_after_dd returns pass when no death dates", {
  DM <- data.frame(
    USUBJID = c(1, 2),
    DTHDTC = c("", ""),
    stringsAsFactors = FALSE
  )
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 2, 2),
    QSDTC = c("2015-06-30", "2015-09-30", "2015-12-30", "2015-06-30", "2015-09-30", "2015-12-30"),
    QSCAT = "A",
    QSTESTCD = "ECOG",
    QSORRES = LETTERS[1:6],
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_after_dd(DM, QS)
  expect_true(result)
})

test_that("check_qs_qsdtc_after_dd fails when required variables are missing", {
  DM <- data.frame(
    USUBJID = c(1, 2),
    stringsAsFactors = FALSE
  )
  QS <- data.frame(
    USUBJID = c(1, 1, 1, 2, 2, 2),
    QSDTC = c("2015-06-30", "2015-09-30", "2015-12-30", "2015-06-30", "2015-09-30", "2015-12-30"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_after_dd(DM, QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_qs_qsdtc_visit_ordinal_error() ----
test_that("check_qs_qsdtc_visit_ordinal_error returns pass when dates in order", {
  QS <- data.frame(
    USUBJID = c(rep(101, 5), rep(102, 5)),
    QSCAT = "DLQI",
    QSTESTCD = "DLQI01",
    QSDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "计划外访视", "C4/D1"), 2),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_visit_ordinal_error(QS)
  expect_true(result)
})

test_that("check_qs_qsdtc_visit_ordinal_error returns fail when dates out of order", {
  QS <- data.frame(
    USUBJID = c(rep(101, 5), rep(102, 5)),
    QSCAT = "DLQI",
    QSTESTCD = "DLQI01",
    QSDTC = rep(c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-15T10:25", "2017-01-20T08:25", "2017-01-25T08:25"), 2),
    VISITNUM = rep(1:5, 2),
    VISIT = rep(c("C1/D1", "C2/D1", "C3/D1", "计划外访视", "C4/D1"), 2),
    stringsAsFactors = FALSE
  )
  QS$QSDTC[QS$USUBJID == 101 & QS$VISIT == "C3/D1"] <- "2017-01-01T06:25"
  result <- check_qs_qsdtc_visit_ordinal_error(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "data entry error")
})

test_that("check_qs_qsdtc_visit_ordinal_error handles duplicated dates", {
  QS <- data.frame(
    USUBJID = c(rep(101, 5)),
    QSCAT = "DLQI",
    QSTESTCD = "DLQI01",
    QSDTC = c("2017-01-01T08:25", "2017-01-05T09:25", "2017-01-05T09:25", "2017-01-20T08:25", "2017-01-25T08:25"),
    VISITNUM = 1:5,
    VISIT = c("C1/D1", "C2/D1", "C3/D1", "C4/D1", "C5/D1"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_visit_ordinal_error(QS)
  expect_false(result)
})

test_that("check_qs_qsdtc_visit_ordinal_error fails when required variables are missing", {
  QS <- data.frame(
    USUBJID = c(rep(101, 5)),
    QSCAT = "DLQI",
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsdtc_visit_ordinal_error(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})

# Tests for check_qs_qsstat_qsstresc() ----
test_that("check_qs_qsstat_qsstresc returns pass when QSSTAT and QSSTRESC consistent", {
  QS <- data.frame(
    STUDYID = 1,
    USUBJID = c(1, 1, 2, 2, 3, 3),
    QSSTRESC = c("1", "", "3", "", "5", ""),
    VISIT = c("V1", "V2", "V1", "V2", "V1", "V2"),
    QSSTAT = c("", "未查", "", "未查", "", "未查"),
    QSCAT = "PRO",
    QSDTC = "2016-01-01",
    QSTESTCD = "ECOG01",
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsstat_qsstresc(QS)
  expect_true(result)
})

test_that("check_qs_qsstat_qsstresc returns fail when QSSTAT=未查 but QSSTRESC present", {
  QS <- data.frame(
    STUDYID = 1,
    USUBJID = c(rep(1, 6), rep(2, 6)),
    QSSTRESC = 1:12,
    VISIT = c(rep(1, 3), rep(2, 3), rep(1, 3), rep(2, 3)),
    QSSTAT = rep(c("", "未查"), 6),
    QSCAT = rep(c("INDIVIDUAL", "OVERALL", "BFI"), 4),
    QSDTC = "2016-01-01",
    QSTESTCD = c(rep("QSALL", 6), rep("ECOG01", 6)),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsstat_qsstresc(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "non-missing QSSTRESC")
})

test_that("check_qs_qsstat_qsstresc handles QSTESTCD=QSALL", {
  QS <- data.frame(
    STUDYID = 1,
    USUBJID = c(1, 2),
    QSSTRESC = c("1", "2"),
    VISIT = c(1, 2),
    QSSTAT = c("", ""),
    QSCAT = c("INDIVIDUAL", "OVERALL"),
    QSDTC = "2016-01-01",
    QSTESTCD = c("QSALL", "QSALL"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsstat_qsstresc(QS)
  expect_false(result)
})

test_that("check_qs_qsstat_qsstresc fails when required variables are missing", {
  QS <- data.frame(
    USUBJID = c(1, 2),
    QSSTRESC = c("1", "2"),
    stringsAsFactors = FALSE
  )
  result <- check_qs_qsstat_qsstresc(QS)
  expect_false(result)
  expect_match(attr(result, "msg"), "missing")
})
