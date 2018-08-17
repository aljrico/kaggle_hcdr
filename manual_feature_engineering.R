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


# Custom Functions --------------------------------------------------------

na_replace <- function(x){
	if(is.vector(x) & !is.list(x)){
		new_x <- x
		w <- which(is.na(x))
		y <- x[!is.na(x)]
		for(i in w) new_x[i] <- sample(x = y, size = 1, replace = TRUE); cat(paste0("... ", floor(i/length(w)*100), "% ... \n"))
		return(new_x)
	}else if(is.data.frame(x)){
		df <- as.data.frame(x)
		ncols <- ncol(df)
		for(i in 1:ncols){
			cat(paste0("... ", floor(i/ncols*100), "% ... \n"))
			x <- df[i]
			if(sum(is.na(x)) > 0){
				new_x <- x
				w <- which(is.na(x))
				y <- x[!is.na(x)]
				for(k in w) new_x[k,] <- sample(x = y, size = 1, replace = TRUE)
				df[i] <- new_x
			}
		}
		return(df)
	}else if(is.list(x)){
		stop("A list can not be evaluated. Please introduce a vector instead.")
	}else{stop("Unrecognized Format.")}
}


# Retrieving Data ------------------------------------------------------------
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

tic()
# COMMONAREA
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

# NONLIVINGAPARTMENTS
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

# LIVINGAPARTMENTS
tr_te[,LIVINGAPARTMENTS_MODE := sqrt(LIVINGAPARTMENTS_MODE)]
tr_te[,LIVINGAPARTMENTS_MEDI := sqrt(LIVINGAPARTMENTS_MEDI)]
tr_te[,LIVINGAPARTMENTS_AVG := sqrt(LIVINGAPARTMENTS_AVG)]

tmp <- median(tr_te$LIVINGAPARTMENTS_MODE, na.rm = TRUE)
tr_te[,LIVINGAPARTMENTS_MODE := ifelse(is.na(LIVINGAPARTMENTS_MODE), tmp, LIVINGAPARTMENTS_MODE)]

tmp <- median(tr_te$LIVINGAPARTMENTS_MEDI, na.rm = TRUE)
tr_te[,LIVINGAPARTMENTS_MEDI := ifelse(is.na(LIVINGAPARTMENTS_MEDI), tmp, LIVINGAPARTMENTS_MEDI)]

tmp <- median(tr_te$LIVINGAPARTMENTS_AVG, na.rm = TRUE)
tr_te[,LIVINGAPARTMENTS_AVG := ifelse(is.na(LIVINGAPARTMENTS_AVG), tmp, LIVINGAPARTMENTS_AVG)]

tr_te[,LIVINGAPARTMENTS_AGG := LIVINGAPARTMENTS_AVG * LIVINGAPARTMENTS_MEDI * LIVINGAPARTMENTS_MODE]

tr_te[,LIVINGAPARTMENTS_AVG := NULL]
tr_te[,LIVINGAPARTMENTS_MODE := NULL]
tr_te[,LIVINGAPARTMENTS_MEDI := NULL]

# FLOORS
tr_te$FLOORSMIN_AVG <- tr_te$FLOORSMIN_AVG %>% na_replace()
tr_te$FLOORSMIN_MODE <- tr_te$FLOORSMIN_MODE %>% na_replace()
tr_te$FLOORSMIN_MEDI <- tr_te$FLOORSMIN_MEDI %>% na_replace()

tr_te[,FLOORSMIN_AGG := FLOORSMIN_AVG + FLOORSMIN_MODE + FLOORSMIN_MEDI]
tr_te[,FLOORSMIN_AVG := NULL]
tr_te[,FLOORSMIN_MODE := NULL]
tr_te[,FLOORSMIN_MEDI := NULL]

tr_te$FLOORSMAX_AVG <- tr_te$FLOORSMAX_AVG %>% na_replace()
tr_te$FLOORSMAX_MODE <- tr_te$FLOORSMAX_MODE %>% na_replace()
tr_te$FLOORSMAX_MEDI <- tr_te$FLOORSMAX_MEDI %>% na_replace()

tr_te[,FLOORSMAX_AGG := FLOORSMAX_AVG + FLOORSMAX_MODE + FLOORSMAX_MEDI]
tr_te[,FLOORSMAX_AVG := NULL]
tr_te[,FLOORSMAX_MODE := NULL]
tr_te[,FLOORSMAX_MEDI := NULL]

tr_te[,FLOORS := FLOORSMAX_AGG * FLOORSMIN_AGG]

tr_te[,FLOORSMAX_AGG := NULL]
tr_te[,FLOORSMIN_AGG := NULL]


# YEARS_BUILD
tr_te[,YEARS_BUILD := ((YEARS_BUILD_AVG + YEARS_BUILD_MEDI + YEARS_BUILD_MODE)/3)^2]

tr_te[,YEARS_BUILD_AVG := NULL]
tr_te[,YEARS_BUILD_MODE := NULL]
tr_te[,YEARS_BUILD_MEDI := NULL]

tmp <- mean(tr_te$YEARS_BUILD, na.rm = TRUE)

tr_te[,YEARS_BUILD := ifelse(is.na(YEARS_BUILD), tmp, YEARS_BUILD)]


# OWN_CAR_AGE
tr_te[,OWN_CAR_AGE := sqrt(OWN_CAR_AGE)]

tmp <- median(tr_te$OWN_CAR_AGE)
tr_te[,OWN_CAR_AGE := ifelse(is.na(OWN_CAR_AGE), tmp, OWN_CAR_AGE)]


# LANDAREA
tr_te[,LANDAREA := (LANDAREA_MEDI + LANDAREA_MODE + LANDAREA_AVG)/3]
m <- mean(tr_te$LANDAREA, na.rm = TRUE)
s <- sd(tr_te$LANDAREA, na.rm = TRUE)
tr_te[,LANDAREA := sqrt(sqrt((LANDAREA - m)/s))]

tmp <- mean(tr_te$LANDAREA, na.rm = TRUE)

tr_te[,LANDAREA := ifelse(is.na(LANDAREA), tmp, LANDAREA)]

tr_te[,LANDAREA_MEDI := NULL]
tr_te[,LANDAREA_MODE := NULL]
tr_te[,LANDAREA_AVG := NULL]

rm(tmp, m, s)



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
