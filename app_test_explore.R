library(tidyverse)
library(data.table)
library(randomForest)
library(mice)

# Loading Data ------------------------------------------------------------

application_train <- fread("data/application_train.csv") %>%
	mutate(TARGET = as.character(TARGET)) %>%
	mutate(SK_ID_CURR = as.character(SK_ID_CURR)) %>%
	as_tibble()


# Exploration -------------------------------------------------------------

df <- application_train %>%
	select(c("TARGET", "CODE_GENDER"))

x <- df$TARGET
y <- df$CODE_GENDER

chisq.test(x,y)
	mutate(rescale = (value)) %>%
	ggplot(aes(x = as.factor(CODE_GENDER), y = as.factor(TARGET))) +
	geom_tile(aes(fill = rescale))
