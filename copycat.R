library(tidyverse)
library(data.table)
library(xgboost)
library(magrittr)

dummify <- function(data){
	data <- as.data.frame(data)
	for(feature in colnames(data)){
		if ((class(data[[feature]])=="factor") & feature != "is_payer_365d") {
			for(level in unique(data[feature])){
				data[paste(feature, level, sep = "_")] <- ifelse(data[[feature]] == level, 1, 0)
			}
			data[feature] <- NULL
		}
	}
	return(data)
}

categorize <- function(data){
	features <- colnames(data)
	for(f in features) {
		if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
			levels <- unique(data[[f]])
			data[[f]] <- (factor(data[[f]], levels=levels))
		}
	}
	return(data)
}

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

na_clean <- function(x, limit = 0.1){
	if(is.data.frame(x)){
		df <- as.data.frame(x)
		ncols <-  ncol(df)
		empty_columns <- c()
		for(i in 1:ncols){
			y <- df[i]
			if(mean(is.na(y)) > limit) empty_columns <- c(empty_columns, i)
		}
	}else if(is.list(x)){
		stop("A list can not be evaluated. Please introduce a vector instead.")
	}else{stop("Unrecognized Format.")}

	return(df[-empty_columns])
}

preprocessing_functions <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))


# Retrieve Data -----------------------------------------------------------

cat("Loading data...\n")

train              <- fread("data/application_train.csv")
test               <- fread("data/application_test.csv")
bureau_raw         <- fread("data/bureau.csv")
bureau_balance_raw <- fread("data/bureau_balance.csv")
cred_card_bal      <- fread("data/credit_card_balance.csv")
pos_cash_bal       <- fread("data/POS_CASH_balance.csv")
payments           <- fread("data/installments_payments.csv")
prev               <- fread("data/previous_application.csv")


# Preprocessing -----------------------------------------------------------

cat("Preprocessing...\n")

bureau_balance_feats <- bureau_balance_raw %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_BUREAU) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(bureau_balance_raw); gc()

bureau_feats <- bureau_raw %>%
	merge(bureau_balance_feats, by = "SK_ID_BUREAU", all.x=TRUE) %>%
	select(-SK_ID_BUREAU) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(bureau_raw, bureau_balance_feats); gc()

cc_feats <- cred_card_bal %>%
	select(-SK_ID_PREV) %>%
	merge(bureau_feats, by = "SK_ID_CURR", all.y = TRUE) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(cred_card_bal, bureau_feats, cred_card_bal); gc()

payments_feats <- payments %>%
	select(-SK_ID_PREV) %>%
	merge(cc_feats, by = "SK_ID_CURR", all.y = TRUE) %>%
	mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
				 PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
				 DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
				 DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
				 DPD = ifelse(DPD > 0, DPD, 0),
				 DBD = ifelse(DBD > 0, DBD, 0)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(payments, cc_feats); gc()

pos_cash_bal_feats <- pos_cash_bal %>%
	select(-SK_ID_PREV) %>%
	merge(payments_feats, by = "SK_ID_CURR", all.y = TRUE) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(pos_cash_bal, payments_feats); gc()

prev_feats <- prev %>%
	select(-SK_ID_PREV) %>%
	merge(pos_cash_bal_feats, by = "SK_ID_CURR", all.y = TRUE) %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
				 DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
				 DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
				 DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
				 DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
				 APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(preprocessing_functions) %>%
	data.table()
rm(prev, pos_cash_bal_feats); gc()


tri <- 1:nrow(train)
y <- train$TARGET

tr_te_raw <- train %>%
	select(-TARGET) %>%
	bind_rows(test) %>%
	data.table() %>%
	merge(prev_feats, by = "SK_ID_CURR", all.x = TRUE) %>%
	select(-SK_ID_CURR) %>%
	data.table()

rm(prev_feats, train, test); gc()

tr_te <- tr_te_raw %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
	mutate(na = apply(., 1, function(x) sum(is.na(x))),
				 DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
				 DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
				 INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
				 INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
				 ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
				 LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
				 ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
				 CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS,
				 CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
				 INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
				 SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
				 CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
				 CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
				 PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
				 PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) %>%
	data.matrix()

rm(tr_te_raw); gc()

#---------------------------
cat("Preparing data...\n")

dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")

p <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "auc",
					nthread = 8,
					eta = 0.025,
					max_depth = 6,
					min_child_weight = 19,
					gamma = 0,
					subsample = 0.8,
					colsample_bytree = 0.632,
					alpha = 0,
					lambda = 0.05,
					nrounds = 2000)

p <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "auc",
					nthread = 4,
					eta = 0.05,
					max_depth = 6,
					min_child_weight = 30,
					gamma = 0,
					subsample = 0.85,
					colsample_bytree = 0.7,
					colsample_bylevel = 0.632,
					alpha = 0,
					lambda = 0,
					nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)

xgb.importance(cols, model=m_xgb) %>%
	xgb.plot.importance(top_n = 30)

#---------------------------
read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(m_xgb, dtest)) %>%
	write_csv(paste0("submit/xgb_", round(m_xgb$best_score, 4), ".csv"))
