library(tidyverse)
library(data.table)
library(randomForest)
library(harrypotter)
source("na_replace.R")

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

application_raw <- fread("data/application_train.csv")
application_test <- fread("data/application_test.csv")
bureau <- fread("data/bureau.csv")
bureau_balance <- fread("data/bureau_balance.csv")
credit_card_balance <- fread("data/credit_card_balance.csv")
installments_payments <- fread("data/installments_payments.csv")
pos_cash_balance <- fread("data/POS_CASH_balance.csv")


# Preprocessing -----------------------------------------------------------------

cat("Vanilla \n")

application_train <- application_raw %>%
	na.omit() %>%
	data.table()

application_test$TARGET <- NA

features_df <- application_train %>%
	rbind(application_test) %>%
	dplyr::select(-SK_ID_CURR) %>%
	categorize() %>%
	dplyr::mutate(NAME_TYPE_SUITE = ifelse(NAME_TYPE_SUITE == "Spouse, partner", "Spouse", NAME_TYPE_SUITE)) %>%
	dplyr::mutate(NAME_EDUCATION_TYPE = ifelse(NAME_EDUCATION_TYPE == "Secondary / secondary special", "Secondary", NAME_EDUCATION_TYPE)) %>%
	dplyr::mutate(NAME_FAMILY_STATUS = ifelse(NAME_FAMILY_STATUS == "Single / not married", "Single", NAME_FAMILY_STATUS)) %>%
	dplyr::mutate(NAME_HOUSING_TYPE = ifelse(NAME_HOUSING_TYPE == "House / apartmtent", "House", NAME_HOUSING_TYPE)) %>%
	dplyr::mutate(OCCUPATION_TYPE = ifelse(OCCUPATION_TYPE == "Waiters/barmen staff", "Waiters", OCCUPATION_TYPE)) %>%
	dplyr::mutate(ORGANIZATION_TYPE = ifelse(ORGANIZATION_TYPE == "Self-employed", "selfemployed", ORGANIZATION_TYPE)) %>%
	dplyr::mutate(WALLSMATERIAL_MODE = ifelse(WALLSMATERIAL_MODE == "Stone, brick", "Stone", WALLSMATERIAL_MODE)) %>%
	dplyr::mutate_if(is.factor, funs(gsub(" ", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub(":", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub("/", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub(",", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub("-", "_", ., fixed = TRUE))) %>%
	categorize() %>%
	dummify() %>%
	data.table()

train_features_2 <- features_df %>%
	dplyr::filter(!is.na(TARGET)) %>%
	dplyr::select(-TARGET)

test_features_2 <- features_df %>%
	dplyr::filter(is.na(TARGET)) %>%
	dplyr::select(-TARGET)

train_df_2 <- train_features_2 %>% cbind((application_train[,"TARGET"]))
test_df_2 <- test_features_2 %>% cbind((application_test[,"SK_ID_CURR"]))

rf_model_2 <- randomForest(as.factor(TARGET) ~ ., data = train_df_2, importance = TRUE)


# Show model error
plot(rf_model_2, ylim=c(0,0.36))
legend('topright', colnames(rf_model_2$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model_2)
varImportance <- data.frame(Variables = row.names(importance),
														Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
	mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
rankImportance %>%
	dplyr::arrange(desc(Importance)) %>%
	dplyr::top_n(50) %>%
ggplot(aes(x = reorder(Variables, Importance),
					 y = Importance,
					 fill = Importance)) +
	geom_bar(stat='identity') +
	labs(x = 'Variables') +
	coord_flip() +
	scale_fill_hp(house = "Gryffindor") +
	theme_bw()

prediction_2 <- predict(rf_model_2, test_df_2, type = "prob")[,"1"]
prediction_2[is.na(prediction_2)] <- 0

to_save <- cbind(test_df_2$SK_ID_CURR, (prediction_2)) %>% data.table()

colnames(to_save) <- c("SK_ID_CURR", "TARGET")
to_save$SK_ID_CURR <- as.numeric(to_save$SK_ID_CURR)
to_save$TARGET <- as.numeric(to_save$TARGET)

threshold <- to_save$TARGET %>% mean()
to_save <- to_save %>%
	dplyr::mutate(TARGET = ifelse(TARGET >= threshold, 1, 0))

to_save$SK_ID_CURR <- as.integer(to_save$SK_ID_CURR)
to_save$TARGET <- as.numeric(to_save$TARGET)

fwrite(to_save, "submit/rf_1.csv", col.names = TRUE)

to_save %>% arrange(desc(TARGET))

cat("... SCORE 0.492 ... \n")

rm(application_raw)

# Adding Bureau -----------------------------------------------------------

cat("Bureau... \n")

bureau[is.na(bureau)] <- 0


avg_bureau <- bureau %>%
	# dplyr::mutate(CREDIT_ACTIVE = ifelse("Active", 1, 0)) %>%
	categorize() %>%
	# merge(bureau_balance, by = "SK_ID_BUREAU") %>%
	select(-SK_ID_BUREAU) %>%
	dplyr::mutate(SK_ID_CURR = as.numeric(SK_ID_CURR)) %>%
	dplyr::mutate(CREDIT_TYPE = ifelse("Cash loan (non-earmarked)", "Cash Loan", CREDIT_TYPE)) %>%
	dplyr::mutate(CREDIT_TYPE = ifelse("Loan for purchase of shares (margin lending)", "purchase of shares", CREDIT_TYPE)) %>%
	categorize() %>%
	dplyr::mutate_if(is.factor, funs(gsub(" ", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub(":", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub("/", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub(",", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub("-", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub("(", "_", ., fixed = TRUE))) %>%
	dplyr::mutate_if(is.factor, funs(gsub(")", "_", ., fixed = TRUE))) %>%
	categorize() %>%
	dummify() %>%
	dplyr::mutate(SK_ID_CURR = as.factor(as.character(SK_ID_CURR))) %>%
	data.table() %>%
	dplyr::group_by(SK_ID_CURR) %>%
	dplyr::mutate(n_credits = n()) %>%
	dplyr::summarise_all(funs(mean, sd, max, min), na.rm = TRUE) %>%
	data.table()

most_important_feats <- rankImportance %>%
	dplyr::arrange(desc(Importance)) %>%
	dplyr::top_n(50)

important_feats <- unique(most_important_feats$Variables) %>% c()

base_features <- features_df[,..important_feats] %>% data.table()

train_df <- base_features %>%
	dplyr::filter(!is.na(TARGET)) %>%
	cbind((application_train[,"SK_ID_CURR"])) %>%
	dplyr::mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	data.table() %>%
	merge(avg_bureau, by = c("SK_ID_CURR")) %>%
	select(-CREDIT_TYPE_min, -CREDIT_TYPE_sd, -CREDIT_TYPE_mean, -CREDIT_TYPE_max, -SK_ID_CURR) %>%
	na.omit() %>%
	data.table()

test_df <- base_features %>%
	dplyr::filter(is.na(TARGET)) %>%
	dplyr::select(-TARGET) %>%
	cbind((application_test[,"SK_ID_CURR"])) %>%
	dplyr::mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	data.table() %>%
	merge(avg_bureau, by = "SK_ID_CURR") %>%
	data.table()


rf_model <- randomForest(as.factor(TARGET) ~ ., data = train_df, importance = TRUE)


# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
														Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
	mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
rankImportance %>%
	dplyr::arrange(desc(Importance)) %>%
	dplyr::top_n(50) %>%
	ggplot(aes(x = reorder(Variables, Importance),
						 y = Importance,
						 fill = Importance)) +
	geom_bar(stat='identity') +
	labs(x = 'Variables') +
	coord_flip() +
	scale_fill_hp(house = "Gryffindor") +
	theme_bw()

prediction <- predict(rf_model, test_df, type = "prob")[,"1"]
prediction[is.na(prediction)] <- 0

to_save <- cbind(test_df$SK_ID_CURR, (prediction)) %>% data.table()

colnames(to_save) <- c("SK_ID_CURR", "TARGET")
to_save$SK_ID_CURR <- as.numeric(to_save$SK_ID_CURR)
to_save$TARGET <- as.numeric(to_save$TARGET)

threshold <- to_save$TARGET %>% mean()
to_save <- to_save %>%
	dplyr::mutate(TARGET = ifelse(TARGET >= threshold, 1, 0))

to_save$SK_ID_CURR <- as.integer(to_save$SK_ID_CURR)
to_save$TARGET <- as.numeric(to_save$TARGET)

fwrite(to_save, "submit/rf_2.csv", col.names = TRUE)





