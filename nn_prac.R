library(tidyverse)
library(neuralnet)
library(palmerpenguins)

df <- penguins
df <- df %>% na.omit()

set.seed(123)
inds <- sample(1:nrow(df), floor(0.8*nrow(df)))
set.seed(NULL)

df_train <- df[inds,]
df_test <- df[-inds,]

nn <- neuralnet(species ~ 
                bill_length_mm + bill_depth_mm, 
                data = df_train,
                linear.output = T)

plot(nn)

preds <- predict(nn, df_test)
table(apply(preds, 1, which.max), df_test$species)




# Split data
train_idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris_train <- iris[train_idx, ]
iris_test <- iris[-train_idx, ]

# Binary classification
nn.iris <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
pred.iris <- predict(nn.iris, iris_test)
table(iris_test$Species == "setosa", pred.iris[, 1] > 0.5)

# Multiclass classification
nn.iris2 <- neuralnet(
                (Species == "setosa") + 
                (Species == "versicolor") + 
                (Species == "virginica") ~ 
                Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
plot(nn.iris2)
pred.iris2 <- predict(nn.iris2, iris_test)
table(iris_test$Species, apply(pred.iris2, 1, which.max))

nn.iris2 <- neuralnet(
  (Species == "setosa") + 
    (Species == "versicolor") + 
    (Species == "virginica") ~ 
    Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
plot(nn.iris2)
pred.iris2 <- predict(nn.iris2, iris_test)
table(iris_test$Species, apply(pred.iris2, 1, which.max))