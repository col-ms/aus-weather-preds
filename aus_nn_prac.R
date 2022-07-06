library(tidyverse)
library(caret)
library(neuralnet)

df <- read_csv("workingdata.csv", show_col_types = F)[,-1]


set.seed(404)
inds <- sample(1:nrow(df), floor(0.8*nrow(df)))
set.seed(NULL)

train.data <- df[inds,]
test.data <- df[-inds,]


nn <- neuralnet(RainTomorrow ~ ., data = train.data, hidden = c(4,2), 
                stepmax = 1e+6, lifesign = "full")

preds <- round(predict(nn, test.data))

plot(nn)
table(preds, test.data$RainTomorrow)


