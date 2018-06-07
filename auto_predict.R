library(tidyverse)
library(data.table)
library(randomForest)
library(mice)
library(mlr)
source("na_replace.R")



# Custom Function ---------------------------------------------------------

dummify <- function(data){


	# Create Features
	features <- colnames(data)

	for(f in features) {
		if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
			levels <- unique(data[[f]])
			data[[f]] <- as.numeric(factor(data[[f]], levels=levels))
		}
	}
	return(data)
}


# Loading Data ------------------------------------------------------------

application_train <- fread("data/application_train.csv") %>%
	mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	mutate(TARGET = as.factor(TARGET)) %>%
	na.omit() %>%
	data.table()

application_test <- fread("data/application_test.csv") %>%
	mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	na.omit() %>%
	data.table()


prediction <- rep(0, nrow(application_test))
n_ensemble <- 250
prop_features <- 0.4
prop_rows <- 0.2

# Feature Selection -----------------------------

faceless_train <- application_train %>%
	select(-TARGET, -SK_ID_CURR) %>%
	dummify() %>%
	data.table()

faceless_test <- application_test %>%
	select(-SK_ID_CURR) %>%
	dummify() %>%
	data.table()

face_test <- application_test %>%
	select(SK_ID_CURR)

face_train <- application_train %>%
	select(TARGET)

for(i in n_ensemble){

	features_num <- sample(1:length(faceless_train), length(faceless_train)*prop_features, replace = FALSE)

	to_train <- faceless_train[features_num] %>% cbind(face_train) %>% sample_n(nrow(faceless_train)*prop_rows)%>% data.table()
	to_test <- faceless_test[features_num] %>% cbind(face_test) %>% data.table()


	formula = TARGET ~ .
	fit <- fit <- glm(formula = TARGET ~.,
														 data = to_train,
														 na.action = na.omit,
														 family = binomial)

	partial_prediction <- as.numeric(predict(fit, to_test, type = "response"))

	prediction <- prediction + partial_prediction

}


solution <- data.frame(id = to_test$SK_ID_CURR, prediction = prediction/n_ensemble)



