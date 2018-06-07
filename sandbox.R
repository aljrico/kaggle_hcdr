library(tidyverse)
library(data.table)
library(randomForest)
library(mice)

# Loading Data ------------------------------------------------------------

application_train <- fread("data/application_train.csv")
application_test <- fread("data/application_test.csv")
bureau <- fread("data/bureau.csv") %>% mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>% data.table()
bureau_balance <- fread("data/bureau_balance.csv")
credit_card_balance <- fread("data/credit_card_balance.csv")


nmax <- 1e5


# Complete database
train_df <- application_train %>%
	mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	mutate(TARGET = as.character(TARGET))
	# sample_n(nmax, replace = FALSE) %>%
	data.table()

train_df %>% inner_join(bureau, by = "SK_ID_CURR") %>% nrow()

train_df <- train_df[bureau, on = "SK_ID_CURR", nomatch=0L]
train_df <- train_df[bureau_balance, on = "SK_ID_BUREAU", nomatch=0L]

	merge(bureau, by = "SK_ID_CURR", all = FALSE) %>%
	merge(bureau_balance, by = "SK_ID_BUREAU", all = FALSE) %>%
	merge(credit_card_balance, by = "SK_ID_CURR", all = FALSE) %>%
	# dplyr::mutate(TARGET = (as.character(TARGET))) %>%
	as_tibble() %>%
	sample_n(nmax, replace = TRUE)

train

train_df2 <- train_df %>%
	select(-TARGET)


# Create Features
features <- colnames(train_df2)

for(f in features) {
	if ((class(train_df2[[f]])=="factor") || (class(train_df2[[f]])=="character")) {
		levels <- unique(train_df2[[f]])
		train_df2[[f]] <- as.numeric(factor(train_df2[[f]], levels=levels))
	}
}

train_df2$TARGET = NULL
train_df2$TARGET = as.factor(train_df$TARGET)
levels(train_df2$TARGET) = make.names(unique(train_df2$TARGET))

# Test
test_df <- application_test
# data.table() %>%
	# merge(bureau, by = "SK_ID_CURR") %>%
	# merge(bureau_balance, by = "SK_ID_BUREAU") %>%
	# merge(credit_card_balance, by = "SK_ID_CURR")


features <- colnames(test_df)

for (f in features) {
	if ((class(test_df[[f]])=="factor") || (class(test_df[[f]])=="character")) {
		levels <- unique(test_df[[f]])
		test_df[[f]] <- as.numeric(factor(test_df[[f]], levels=levels))
	}
}


# Empty memmory
rm(application_test,application_train,bureau,bureau_balance,credit_card_balance)

# Missing Values ----------------------------------------------------------






# Logistic Regression -----------------------------------------------------
formula = TARGET ~ .
glm(formula = formula, family=binomial(link='logit'),data=train_df2)


# Random Forest -----------------------------------------------------------
formula = TARGET ~ .
fit <- randomForest(formula = formula,
										data = train_df2,
										na.action = na.omit)

prediction <- predict(fit, test_df)

# Get importance
importance    <- importance(fit)
varImportance <- data.frame(Variables = row.names(importance),
														Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
	mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
	arrange(desc(Importance))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
													 y = Importance, fill = Importance)) +
	geom_bar(stat='identity') +
	geom_text(aes(x = Variables, y = 0.5, label = Rank),
						hjust=0, vjust=0.55, size = 4, colour = 'red') +
	labs(x = 'Variables') +
	coord_flip() +
	theme_minimal()


# Model 1 -------------------------------------------------------------------

formula = TARGET ~ .

fitControl <- trainControl(method="cv",number = 5,  classProbs = TRUE, summaryFunction = twoClassSummary)

xgbGrid <- expand.grid(nrounds = 100,
											 max_depth = 7,
											 eta = .05,
											 gamma = 0,
											 colsample_bytree = .8,
											 min_child_weight = 1,
											 subsample = 1)


XGBModel = train(formula, data = application_train2,
								 method = "xgbTree",trControl = fitControl,
								 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC"
)

XGBModel
