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


# Retrieving Data ------------------------------------------------------------

cat("Loading data...\n")

tr               <- fread("data/application_train.csv") %>% sample_n(1e5)
te               <- fread("data/application_test.csv")
bureau           <- fread("data/bureau.csv")
bbalance         <- fread("data/bureau_balance.csv")
cc_balance       <- fread("data/credit_card_balance.csv")
pc_balance       <- fread("data/POS_CASH_balance.csv")
payments         <- fread("data/installments_payments.csv")
prev             <- fread("data/previous_application.csv")

#---------------------------
cat("Preprocessing...\n")

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



# Random Forest -----------------------------------------------------------
#
# test_df <- tr_te[-tri, ]
# train_df <- tr_te
# train_df[is.na(train_df)] <- 0
# test_df[is.na(test_df)] <- 0
# rf_model <- randomForest(y = as.factor(y[tri]), x = train_df[tri,] , importance = TRUE)
#
#
# # Show model error
# plot(rf_model, ylim=c(0,0.36))
# legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
#
#
# # Get importance
# importance    <- importance(rf_model)
# varImportance <- data.frame(Variables = row.names(importance),
# 														Importance = round(importance[ ,'MeanDecreaseGini'],2))
#
#
# # Use ggplot2 to visualize the relative importance of variables
# varImportance %>%
# 	dplyr::select(Variables, Importance) %>%
# 	dplyr::arrange(desc(Importance)) %>%
# 	dplyr::top_n(50) %>%
# 	ggplot(aes(x = reorder(Variables, Importance),
# 						 y = Importance,
# 						 fill = Importance)) +
# 	geom_bar(stat='identity') +
# 	labs(x = 'Variables') +
# 	coord_flip() +
# 	theme_bw()
#
# read_csv("data/sample_submission.csv") %>%
# 	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
# 				 TARGET = predict(rf_model, test_df)) %>%
# 	write_csv(paste0("submit/rf_model", ".csv"))
#




# Preparing Data ----------------------------------------------------------

tri_val <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()


#XGB
dtest_xgb <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te_xgb <- tr_te[tri, ]
dtrain_xgb <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval_xgb <- xgb.DMatrix(data = tr_te_xgb[-tri_val, ], label = y[-tri_val])
cols_xgb <- colnames(tr_te)

#CatBoost
pool <- tr_te
pool[is.na(pool)] <- 0
train_pool <- catboost.load_pool(as.matrix(pool[tri, ]), label = y[tri])
test_pool <- catboost.load_pool(as.matrix(pool[-tri, ]))


#LGB
train_df <- (tr_te[tri, ])
train_df <- as.data.frame(train_df)
train_df$y <- c(y[tri])
train_df <- Matrix(as.matrix(train_df), sparse=TRUE)

dtrain_lgb = lgb.Dataset(data = train_df,
										 label = y[tri])

dtest_lgb = lgb.Dataset(data = tr_te[-tri, ],
										 label = y[-tri])

rm(tr_te, tri, y); gc()



# Catboost ----------------------------------------------------------------
params <- list(
	loss_function = 'Logloss',
	eval_metric = 'AUC',
	learning_rate = 0.05
	)

params <- list(
	iterations = 10000,
	thread_count = 10,
	loss_function = 'Logloss',
	border_count = 32,
	depth = 5,
	learning_rate = 0.03,
	l2_leaf_reg = 3.5,
	use_best_model = TRUE
							 )

catboost_model <- catboost.train(train_pool, test_pool, params)
catboost_prediction <- catboost.predict(catboost_model,
																				test_pool,
																				prediction_type = 'Class')

read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = catboost_prediction) %>%
	write_csv(paste0("submit/catboost", ".csv"))


# LGB ---------------------------------------------------------------------

params = list(objective = "binary",
							metric = "auc",
							learning_rate= 0.05,
							num_leaves= 7,
							max_depth= 3,
							min_child_samples= 100,
							max_bin= 100,
							subsample= 0.7,
							subsample_freq= 1,
							colsample_bytree= 0.7,
							min_child_weight= 0,
							min_split_gain= 0,
							scale_pos_weight=50)

model <- lgb.train(params, dtrain_lgb, valids = list(validation = dtest_lgb), nthread = 4,
									 nrounds = 1000, verbose= 1, early_stopping_rounds = 50, eval_freq = 50)


val_preds = predict(model, data = as.matrix(dtest_lgb), n = model$best_iter)




# XGB ---------------------------------------------------------------------

cat("Training model...\n")
p <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "logloss",
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

m_xgb <- xgb.train(p, dtrain_xgb, p$nrounds, list(val = dval_xgb), print_every_n = 50, early_stopping_rounds = 300)

xgb.importance(cols_xgb, model=m_xgb) %>%
	xgb.plot.importance(top_n = 50)

#---------------------------
read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(m_xgb, dtest_xgb)) %>%
	write_csv(paste0("submit/tidy_xgb_", round(m_xgb$best_score, 5), ".csv"))




# Hyperparameter Tuning ---------------------------------------------------
#---------------------------


parameterList <- expand.grid(subsample = seq(from = 0.5, to = 1, by = 0.25),
																colsample_bytree = seq(from = 0.4, to = 1, by = 0.2),
																lr = seq(from = 2, to = 10, by = 1),
																mtd = seq(from = 4, to = 10, by = 2))
ntrees <- 100

scores <- c()

rmseErrorsHyperparameters <- for(i in 1:nrow(parameterList)){

	#Extract Parameters to test
	currentSubsampleRate <- parameterList[["subsample"]][[i]]
	currentColsampleRate <- parameterList[["colsample_bytree"]][[i]]
	lr <- parameterList[["lr"]][[i]]
	mtd <- parameterList[["mtd"]][[i]]

	p <- list(objective = "binary:logistic",
						booster = "gbtree",
						eval_metric = "auc",
						nthread = 4,
						eta = lr/ntrees,
						max_depth = mtd,
						min_child_weight = 30,
						gamma = 0,
						subsample = currentSubsampleRate,
						colsample_bytree = currentColsampleRate,
						colsample_bylevel = 0.632,
						alpha = 0,
						lambda = 0,
						nrounds = ntrees)

	xgb_cv <- xgb.cv(p, dtrain_xgb, p$nrounds, print_every_n = 5, early_stopping_rounds = 25, nfold = 5)
	cat(paste0("... ", i/nrow(parameterList)*100, " (%)  ... \n"))
	scores[i] <- xgb_cv$evaluation_log$test_auc_mean %>% max()
}

m <- which.max(scores)
currentSubsampleRate <- parameterList[["subsample"]][[m]]
currentColsampleRate <- parameterList[["colsample_bytree"]][[m]]
lr <- parameterList[["lr"]][[m]]
mtd <- parameterList[["mtd"]][[m]]

ntrees <- 2000
p <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "auc",
					nthread = 4,
					eta = lr/ntrees,
					max_depth = mtd,
					min_child_weight = 30,
					gamma = 0,
					subsample = currentSubsampleRate,
					colsample_bytree = currentColsampleRate,
					colsample_bylevel = 0.632,
					alpha = 0,
					lambda = 0,
					nrounds = ntrees)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 500)
xgb.importance(cols, model=m_xgb) %>%
	xgb.plot.importance(top_n = 50)

read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(m_xgb, dtest)) %>%
	write_csv(paste0("submit/tuned_xgb_", round(m_xgb$best_score, 5), ".csv"))

