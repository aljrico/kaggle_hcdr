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



# Retrieve Data -----------------------------------------------------------

cat("Loading data...\n")

tr <- read_csv("data/application_train.csv")
te <- read_csv("data/application_test.csv")

bureau <- fread("data/bureau.csv")
cred_card_bal <-  fread("data/credit_card_balance.csv")
pos_cash_bal <- fread("data/POS_CASH_balance.csv")
prev <- read_csv("data/previous_application.csv") %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer()))

#---------------------------
cat("Preprocessing...\n")

avg_bureau <- bureau %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(funs(mean), na.rm = TRUE) %>%
	mutate(buro_count = bureau %>%
				 	group_by(SK_ID_CURR) %>%
				 	count() %$% n)

avg_cred_card_bal <- cred_card_bal %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(funs(mean), na.rm = TRUE) %>%
	mutate(card_count = cred_card_bal %>%
				 	group_by(SK_ID_CURR) %>%
				 	count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(funs(mean), na.rm = TRUE) %>%
	mutate(pos_count = pos_cash_bal %>%
				 	group_by(SK_ID_PREV, SK_ID_CURR) %>%
				 	group_by(SK_ID_CURR) %>%
				 	count() %$% n)

avg_prev <- prev %>%
	group_by(SK_ID_CURR) %>%
	summarise_all(funs(mean), na.rm = TRUE) %>%
	mutate(nb_app = prev %>%
				 	group_by(SK_ID_CURR) %>%
				 	count() %$% n)

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>%
	select(-TARGET) %>%
	bind_rows(te) %>%
	left_join(avg_bureau, by = "SK_ID_CURR") %>%
	left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>%
	left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>%
	left_join(avg_prev, by = "SK_ID_CURR") %>%
	mutate_if(is.character, funs(factor(.) %>% as.integer())) %>%
	data.matrix()

rm(tr, te, prev, avg_prev, bureau, avg_bureau, cred_card_bal,
	 avg_cred_card_bal, pos_cash_bal, avg_pos_cash_bal)
gc()

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

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model=m_xgb) %>%
	xgb.plot.importance(top_n = 30)

#---------------------------
read_csv("data/sample_submission.csv") %>%
	mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
				 TARGET = predict(m_xgb, dtest)) %>%
	write_csv(paste0("submit/xgb_", round(m_xgb$best_score, 4), ".csv"))
0
