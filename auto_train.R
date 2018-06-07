library(tidyverse)
library(data.table)
library(randomForest)
library(mice)
library(mlr)

# Loading Data ------------------------------------------------------------

application_train <- fread("data/application_train.csv")
application_test <- fread("data/application_test.csv")


# Processing Data ---------------------------------------------------------


train_df <- application_train %>%
	mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	mutate(TARGET = as.factor(TARGET)) %>%
	na.omit() %>%
	sample_n(floor(sqrt(nrow(application_train))))

train_df2 <- train_df %>%
	select(-TARGET, -SK_ID_CURR)

# Create Features
features <- colnames(train_df2)
cat_pos <- c()
for(i in 1:length(features)) if(class(train_df2[,i]) == "factor" | class(train_df2[,i]) == "character") cat_pos <- c(cat_pos, i)
train_df2 <- createDummyFeatures(train_df2, cols = NULL)


for(f in features) {
	if ((class(train_df2[[f]])=="factor") || (class(train_df2[[f]])=="character")) {
		levels <- unique(train_df2[[f]])
		train_df2[[f]] <- as.numeric(factor(train_df2[[f]], levels=levels))
	}
}

train_df2$TARGET = NULL
train_df2$TARGET = as.factor(train_df$TARGET)
# levels(train_df2$TARGET) = make.names(unique(train_df2$TARGET))

# Test
test_df <- application_test %>% na.omit()
features <- colnames(test_df)

for (f in features) {
	if ((class(test_df[[f]])=="factor") || (class(test_df[[f]])=="character")) {
		levels <- unique(test_df[[f]])
		test_df[[f]] <- as.numeric(factor(test_df[[f]], levels=levels))
	}
}


# Train -------------------------------------------------------------------

formula = TARGET ~ .
fit <- glm(formula = formula, family=binomial ,data=train_df2, na.action = na.omit)

formula = TARGET ~ .
fit <- randomForest(formula = TARGET ~.,
										data = train_df2,
										na.action = na.omit)

plot(fit)
prediction <- predict(fit, test_df)

predict(fit, test_df)
hist(prediction)
