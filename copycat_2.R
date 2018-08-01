library(tidyverse)
library(data.table)
library(xgboost)
library(magrittr)
library(moments)
library(harrypotter)
library(randomForest)
set.seed(666)


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

fn <- funs(mean, sd, min, max, sum, n_distinct, kurtosis, skewness,  .args = list(na.rm = TRUE))

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
				 LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
				 ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
				 CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS,
				 CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
				 INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
				 SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
				 CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
				 CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
				 PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
				 PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED)

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>%
	group_by(ORGANIZATION_TYPE) %>%
	summarise(m = median(AMT_INCOME_TOTAL)) %$%
	setNames(as.list(m), ORGANIZATION_TYPE)

rm(tr, te, fn, sum_bureau, sum_cc_balance,
	 sum_payments, sum_pc_balance, sum_prev); gc()

tr_te %<>%
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

test_df <- tr_te[-tri, ]
train_df <- tr_te
train_df[is.na(train_df)] <- 0
test_df[is.na(test_df)] <- 0
rf_model <- randomForest(y = as.factor(y[tri]), x = train_df[tri,] , importance = TRUE)


# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
														Importance = round(importance[ ,'MeanDecreaseGini'],2))


# Use ggplot2 to visualize the relative importance of variables
varImportance %>%
	dplyr::select(Variables, Importance) %>%
	dplyr::arrange(desc(Importance)) %>%
	dplyr::top_n(50) %>%
	ggplot(aes(x = reorder(Variables, Importance),
						 y = Importance,
						 fill = Importance)) +
	geom_bar(stat='identity') +
	labs(x = 'Variables') +
	coord_flip() +
	theme_bw()

read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(rf_model, test_df)) %>%
	write_csv(paste0("submit/rf_model", ".csv"))



# XGB ---------------------------------------------------------------------

cat("Preparing data...\n")
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

cat("Training model...\n")
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

set.seed(0)
m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)

xgb.importance(cols, model=m_xgb) %>%
	xgb.plot.importance(top_n = 50)

#---------------------------
read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(m_xgb, dtest)) %>%
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

	xgb_model <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 5, early_stopping_rounds = 50)
	cat(paste0("... ", i/nrow(parameterList)*100, " (%)  ... \n"))
	scores[i] <- xgb_model$best_score
}

m <- which.max(scores)
currentSubsampleRate <- parameterList[["subsample"]][[m]]
currentColsampleRate <- parameterList[["colsample_bytree"]][[m]]
lr <- parameterList[["lr"]][[m]]
mtd <- parameterList[["mtd"]][[m]]

ntrees <- 3000
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

