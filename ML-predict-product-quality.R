library(tidyverse)
library(caret)
library(pdp)

df <-read_csv("data_2025.csv")
mat_prop <- read_csv("mat_prop.csv")

## visualize check to clean data

ggplot(data=df_clean, aes(quality_1))+
  geom_histogram(bins = 45)+
  theme_minimal()

ggplot(data=df, aes(x=Ad_D, y=quality_1))+
  geom_point(alpha=0.2, size =2)

ggplot(data=df, aes(x=Ad_N, y=quality_1))+
  geom_point(alpha=0.2, size =2)

## cleansing data 

df_clean <- df %>% 
                distinct(LotNbr_Product, .keep_all = TRUE) %>%
                filter(IssueQty_Mat > 0,
                       Ad_D > 5 & Ad_D < 20,
                       Ad_N > 0,
                       quality_1 >= 450 & quality_1 <= 650) %>%
                left_join(mat_prop, by = c("LotNbr_Mat" = "Mat_Lot")) %>%
                drop_na() %>%
                arrange(LotNbr_Product)

# visual after cleaned

ggplot(data = df_clean, aes(Mat_pH, fill = RM_ItemName))+
  geom_histogram()+
  theme_minimal()+
  facet_wrap(~RM_ItemName, ncol =1)

ggplot() +
  geom_point(data = df_clean, aes(x=Ad_D, y=quality_1),color = "black", alpha = 0.2, size =2)+
  geom_point(data=df_clean, aes(x=Ad_N, y=quality_1), color = "blue", alpha = 0.2, size =2)+
  theme_minimal() +
  labs(title = "Ad_N and Ad_D vs quality_1", x = "Qty(L)", y = "quality_1(sec)") 

## train - test - split

split_data <- function(data){
              set.seed(42)
              n <- nrow(data)
              id <- sample(1:n, 0.8*n)
              train_df <- data[id, ]
              test_df <- data[-id, ]
              return(list(train = train_df, test = test_df))
}

# choose df
prep_data <- split_data(df_clean)

## modeling --> choose "rf_model" 

set.seed(42)

grid_k <- data.frame(k = c(3,5,7,9,11))

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

rf_model <- train(quality_1 ~ Mat_pH + Ad_D + Ad_N + IssueQty_Mat + RM_ItemName,
                  data = prep_data$train,
                  method = "rf",
                  trControl = ctrl)

varImp(rf_model)

partial(rf_model, pred.var = "Ad_D", plot = TRUE)
partial(rf_model, pred.var = "Ad_N", plot = TRUE)
partial(rf_model, pred.var = "Mat_pH", plot = TRUE)
partial(rf_model, pred.var = "IssueQty_Mat", plot = TRUE)
#-------------------------------------------------------------------------------
knn_model <- train(quality_1 ~ Mat_pH + Ad_D + Ad_N + IssueQty_Mat + RM_ItemName,
                   data = prep_data$train,
                   method = "knn",
                   trControl = ctrl,
                   tuneGrid = grid_k)

glmnet_model <- train(quality_1 ~ Mat_pH + Ad_D + Ad_N + IssueQty_Mat + RM_ItemName,
                      data = prep_data$train,
                      method = "glmnet",
                      trControl = ctrl,
                      tuneGrid = expand.grid(
                        alpha = c(0,1),           # alpha = 0 is ridge, alpha = 1 is lasso
                        lambda = c(0.01, 0.10)
                      ))
#-------------------------------------------------------------------------------

## scoring (test)

p_test_rf <- predict(rf_model, newdata = prep_data$test) 

## evaluate model

error_rf <- prep_data$test$quality_1 - p_test_rf

mae_rf <- mean(abs(error_rf))
mse_rf <- mean(error_rf**2)
rmse_rf <- sqrt(mean(error_rf**2))

## check result of evaluation or overfitting -> OK, not overfit

mae_rf
mse_rf
rmse_rf
