library(tidyverse)
library(randomForest)

df <- read_csv("weatherAus.csv") %>% na.omit() %>%
  mutate_if(is.character, factor)

set.seed(123)
inds <- sample(1:nrow(df), floor(0.8*nrow(df)))
set.seed(NULL)

train <- df[inds,]
test <- df[-inds,]

rf.mod <- randomForest(RainTomorrow ~ ., train)

preds <- predict(rf.mod, test, type = "response")

table(preds, test$RainTomorrow)
sum(preds == test$RainTomorrow)/nrow(test)
