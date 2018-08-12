library(tidyverse)
library(data.table)
library(xgboost)
library(magrittr)
library(moments)
library(harrypotter)
library(randomForest)
library(lightgbm)
library(caret)
library(Matrix)
library(catboost)
library(tictoc)

# Retrieving Data ------------------------------------------------------------
tic()
tr               <- fread("data/application_train.csv") %>% sample_n(1e5)
te               <- fread("data/application_test.csv")
bureau           <- fread("data/bureau.csv")
bbalance         <- fread("data/bureau_balance.csv")
cc_balance       <- fread("data/credit_card_balance.csv")
pc_balance       <- fread("data/POS_CASH_balance.csv")
payments         <- fread("data/installments_payments.csv")
prev             <- fread("data/previous_application.csv")



# Preprocessing -----------------------------------------------------------

fn <- funs(mean, sd, min, max, sum, n_distinct, kurtosis, skewness, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_BUREAU) %>%
	summarise_all(fn) %>%
	data.table()
rm(bbalance); gc()

sum_bureau <- bureau %>%
	left_join(sum_bbalance, by = "SK_ID_BUREAU") %>%
	select(-SK_ID_BUREAU) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(fn) %>%
	data.table()
rm(bureau, sum_bbalance); gc()

sum_cc_balance <- cc_balance %>%
	select(-SK_ID_PREV) %>%
	mutate(AMT_LIMIT_EDGE = AMT_DRAWINGS_CURRENT/AMT_CREDIT_LIMIT_ACTUAL) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(fn) %>%
	data.table()
rm(cc_balance); gc()

sum_payments <- payments %>%
	select(-SK_ID_PREV) %>%
	mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
				 PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
				 DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
				 DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
				 DPD = ifelse(DPD > 0, DPD, 0),
				 DBD = ifelse(DBD > 0, DBD, 0)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(fn) %>%
	data.table()
rm(payments); gc()

sum_pc_balance <- pc_balance %>%
	select(-SK_ID_PREV) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(fn) %>%
	data.table()
rm(pc_balance); gc()

sum_prev <- prev %>%
	select(-SK_ID_PREV) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
				 DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
				 DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
				 DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
				 DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
				 APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(fn) %>%
	data.table()
rm(prev); gc()



# Building Features -------------------------------------------------------

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>%
	select(-TARGET) %>%
	bind_rows(te) %>%
	data.table() %>%
	left_join(sum_bureau, by = "SK_ID_CURR") %>%
	left_join(sum_cc_balance, by = "SK_ID_CURR") %>%
	left_join(sum_payments, by = "SK_ID_CURR") %>%
	left_join(sum_pc_balance, by = "SK_ID_CURR") %>%
	left_join(sum_prev, by = "SK_ID_CURR") %>%
	select(-SK_ID_CURR) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	data.table()

tr_te[, COMMONAREA_MODE := log10(COMMONAREA_MODE)]
tmp <- mean(tr_te$COMMONAREA_MODE)
tr_te[, COMMONAREA_MODE := ifelse(is.na(COMMONAREA_MODE), tmp, COMMONAREA_MODE)]
tr_te[, COMMONAREA_MEDI := log10(COMMONAREA_MEDI)]
tmp <- mean(tr_te$COMMONAREA_MEDI)
tr_te[, COMMONAREA_MEDI := ifelse(is.na(COMMONAREA_MEDI), tmp, COMMONAREA_MEDI)]
tr_te[, COMMONAREA_AVG := log10(COMMONAREA_AVG)]
tmp <- mean(tr_te$COMMONAREA_AVG)
tr_te[, COMMONAREA_AVG := ifelse(is.na(COMMONAREA_AVG), tmp, COMMONAREA_AVG)]
tr_te[, COMMONAREA_AGG := (COMMONAREA_AVG + COMMONAREA_MEDI + COMMONAREA_MODE)/3]

tr_te[,COMMONAREA_AVG:=NULL]
tr_te[,COMMONAREA_MEDI:=NULL]
tr_te[,COMMONAREA_MODE:=NULL]
tr_te[,NONLIVINGAPARTMENTS_MEDI := ifelse(is.na(NONLIVINGAPARTMENTS_MEDI), 0, NONLIVINGAPARTMENTS_MEDI)]
tr_te[,NONLIVINGAPARTMENTS_AVG := ifelse(is.na(NONLIVINGAPARTMENTS_AVG), 0, NONLIVINGAPARTMENTS_AVG)]
tr_te[,NONLIVINGAPARTMENTS_MODE := ifelse(is.na(NONLIVINGAPARTMENTS_MODE), 0, NONLIVINGAPARTMENTS_MODE)]
tr_te[,NONLIVINGAPARTMENTS_AGG := (NONLIVINGAPARTMENTS_AVG + NONLIVINGAPARTMENTS_MEDI + NONLIVINGAPARTMENTS_MODE)/3]
m <- mean(sqrt(tr_te$NONLIVINGAPARTMENTS_AGG))
s <- sd(sqrt(tr_te$NONLIVINGAPARTMENTS_AGG))
tr_te[,NONLIVINGAPARTMENTS_AGG := (sqrt(NONLIVINGAPARTMENTS_AGG) - m)/s]
tr_te[,NONLIVINGAPARTMENTS_AGG :=  ifelse(NONLIVINGAPARTMENTS_AGG >= 4, 0, NONLIVINGAPARTMENTS_AGG)]
tr_te[,NONLIVINGAPARTMENTS_MEDI:=NULL]
tr_te[,NONLIVINGAPARTMENTS_MODE:=NULL]
tr_te[,NONLIVINGAPARTMENTS_AVG:=NULL]

toc()

	mutate(na = apply(., 1, function(x) sum(is.na(x))),
				 AMT_ANNUITY_LOG = log10(AMT_ANNUITY),
				 DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
				 DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
				 INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
				 INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
				 ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
				 ANNUITY_INCOME_PERC_LOG = log10(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
				 ANNUITY_PRESSURE = log10(log10(AMT_CREDIT) * AMT_ANNUITY),
				 LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
				 LOAN_INCOME_RATIO_LOG = log10(AMT_CREDIT / AMT_INCOME_TOTAL),
				 LOAN_INCOME_PROD = AMT_CREDIT * AMT_INCOME_TOTAL,
				 LOAN_INCOME_PROD_LOG = log10(AMT_CREDIT * AMT_INCOME_TOTAL),
				 ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
				 CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS,
				 AMT_GOODS_PRICE_LOG = log10(AMT_GOODS_PRICE),
				 CREDIT_TO_GOODS_RATIO_EXTRA = log10(AMT_CREDIT / log10(AMT_GOODS_PRICE)),
				 CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
				 INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
				 INC_PER_CHLD_LOG = log10(AMT_INCOME_TOTAL / (1 + CNT_CHILDREN)),
				 INCOME_RATE_PER_CHILD = log10(AMT_INCOME_TOTAL / ((AMT_CREDIT)*(1 + CNT_CHILDREN^2))),
				 SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
				 SOURCES_MEAN = ((EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3)/3)^2,
				 AGE_PRESSURE = log10(AMT_CREDIT*(-DAYS_BIRTH)/AMT_INCOME_TOTAL),
				 CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
				 CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
				 IMPULSIVITY = -DAYS_LAST_PHONE_CHANGE*log10(AMT_INCOME_TOTAL),
				 IMPULSIVITY_LOG = log10(-DAYS_LAST_PHONE_CHANGE*log10(AMT_INCOME_TOTAL)),
				 PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
				 PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED,
				 PHONE_TO_EMPLOY_RATIO_LOG = log10(DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED)) %>%
	# select(-PHONE_TO_EMPLOY_RATIO, - IMPULSIVITY, - SOURCES_PROD, - INC_PER_CHLD, - CREDIT_TO_GOODS_RATIO, -LOAN_INCOME_PROD, -ANNUITY_INCOME_PERC, -AMT_ANNUITY) %>%
	data.frame()

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>%
	group_by(ORGANIZATION_TYPE) %>%
	summarise(m = median(AMT_INCOME_TOTAL)) %$%
	setNames(as.list(m), ORGANIZATION_TYPE)

rm(tr, te, fn, sum_bureau, sum_cc_balance,
	 sum_payments, sum_pc_balance, sum_prev); gc()

tr_te %<>%
	as_tibble() %>%
	mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
				 DOC_IND_SKEW = apply(tr_te[, docs], 1, skewness),
				 LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
				 NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
				 NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
				 NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd),
				 NEW_SCORES_KURT = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, kurtosis),
				 NEW_SCORES_SKEW = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, skewness))%>%
	mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
	mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>%
	data.matrix()
