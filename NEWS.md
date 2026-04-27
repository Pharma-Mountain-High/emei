# Emei 0.6

### 新功能

* 实现核心`emei()`函数，提供一键式SDTM数据质量检查
* 支持14+个SDTM域的全面检查（DM、AE、VS、LB、EX、CM、EC、TR、TU、RS、SS、TS、QS等）
* 自动SUPP域合并功能
* Excel报告生成功能，自动生成格式化的检查报告
* 支持按优先级（High/Medium/Low）和类型（ALL/ONC/PRO）筛选检查项目

### 增强功能

* 自动读取SAS数据（.sas7bdat格式）
* 自动预处理DM、AE、VS域数据
* 提供详细的运行日志和进度信息
* 支持自定义报告输出目录
* 支持保存原始数据为RDS文件

### 测试

* 499个测试用例，100%通过率
* 覆盖所有核心检查函数

### Bug修复

* 修复`check_ex_extrt_exoccur`函数bug

# Emei 0.7

* 增加药物过量不良事件输出
* 增加PDF输出，超链接功能
