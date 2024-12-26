library(tidymodels)       # v1.1.1
library(caret)            # v6.0-94
library(dplyr)            # v1.1.4
library(recipes)          # v1.0.9
library(ranger)           # v0.16.0
library(randomForest)     # v4.7-1.1
library(rpart)            # v4.1.21
library(kernlab)          # v0.9-32
library(glmnet)           # v4.1-8
library(parsnip)          # v1.1.1
library(workflows)        # v1.1.3
library(workflowsets)     # v1.0.1
library(doParallel)       # v1.0.17
library(ggplot2)          # v3.5.0
library(ggpubr)           # v0.6.0
library(knitr)            # v1.46
library(gridExtra)        # v2.3
library(rules)            # v1.0.2

setwd('path_to_your_working_directory')

raw_data <- read.csv('Freshwater_wild_fish_all.csv', header = TRUE)
raw_data <- raw_data[, -c(2:6, 10, 11, 18)]

# Model training================================================================
set.seed(11)
rd_split <- raw_data %>%
  sample_n(nrow(.)) %>%
  initial_split(prop = 0.7, strata = log_mean_mehg, breaks = 5)
rd_train <- rd_split %>% training()
rd_test <- rd_split %>% testing()

set.seed(22)
rd_train_rsmp <- rd_train %>%
  vfold_cv(strata = log_mean_mehg, v = 10, breaks = 5, repeats = 3)
models <- c("rf", "cubist")
spec_rf <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune()
) %>%
  set_engine('ranger') %>%
  set_mode('regression')
spec_cubist <-
  cubist_rules(
    committees = tune(),
    neighbors = tune(),
    max_rules = tune()
  ) %>%
  set_engine("Cubist") %>%
  set_mode("regression")
recipe_log_mean_mehg <- recipe(formula = log_mean_mehg ~ ., data = rd_train)
wflowset_log_mean_mehg <- workflow_set(
  preproc = list(basic = recipe_log_mean_mehg),
  models = list(
    rf = spec_rf,
    cubist = spec_cubist
  )
)
ctrl <- trainControl(method = "cv", number = 10)
ctrl_lth <-
  control_grid(verbose = TRUE, parallel_over = 'everything',
               save_pred = TRUE, save_workflow = TRUE)
doParallel::registerDoParallel(cores = 6)
res_tune_log_mean_mehg_1 <-
  wflowset_log_mean_mehg %>%
  workflow_map(
    fn = 'tune_grid',
    verbose = TRUE, 
    seed = 1234,
    resamples = rd_train_rsmp,
    grid = 500,
    control = ctrl_lth
  )

set.seed(111)

rd_train_transformed <- recipe(log_mean_mehg ~., data = rd_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE, -all_outcomes()) %>%
  prep() %>% 
  bake (new_data=NULL)

rd_test_transformed <- recipe(log_mean_mehg ~., data = rd_test) %>%
  step_dummy(all_nominal(), one_hot = TRUE, -all_outcomes()) %>%
  prep() %>% 
  bake (new_data=NULL)

rd_train_transformed_rsmp <- rd_train_transformed %>%
  vfold_cv(strata = log_mean_mehg, v = 10, breaks = 5)

set.seed(222)
models <- c("svmrbf", "glmnet")
spec_svmrbf <- 
  svm_rbf(cost = tune(), 
          rbf_sigma = tune(), 
          margin = tune()
  ) %>%
  set_engine("kernlab") %>%
  set_mode("regression")
spec_glmnet <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
recipe_log_mean_mehg_1 <- recipe(formula = log_mean_mehg ~ ., data = rd_train_transformed) 
recipe_log_mean_mehg_2 <- recipe(formula = log_mean_mehg ~ ., data = rd_train_transformed) %>%
  step_normalize(all_numeric_predictors())

wflowset_log_mean_mehg_basic <- workflow_set(
  preproc = list(basic = recipe_log_mean_mehg_1),
  models = list(glm = spec_glmnet)
)

wflowset_log_mean_mehg_norm <-
  workflow_set(
    preproc = list(norm = recipe_log_mean_mehg_2),
    models = list(svmrbf = spec_svmrbf)
  )

wflowset_log_mean_mehg_all <-
  bind_rows(
    wflowset_log_mean_mehg_basic,
    wflowset_log_mean_mehg_norm
  )

doParallel::registerDoParallel(cores = 6)

res_tune_log_mean_mehg_2 <-
  wflowset_log_mean_mehg_all %>%
  workflow_map(
    fn = 'tune_grid',
    verbose = TRUE, 
    seed = 1234,
    resamples = rd_train_transformed_rsmp,
    grid = 500,
    control = ctrl_lth
  )

# Model performance plots=======================================================
## Random forest
best_rf <- res_tune_log_mean_mehg_1 %>%
  extract_workflow_set_result("basic_rf") %>%
  select_best(metric = "rmse")

rf_final_fit <- 
  wflowset_log_mean_mehg %>%
  extract_workflow("basic_rf") %>%
  finalize_workflow(best_rf) %>%
  fit(data=rd_train)

rf_pred <- predict(rf_final_fit, new_data = rd_test)

rf_plot <- rf_pred %>%
  cbind(observed = rd_test$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(hjust = 0.5, size = 14),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

rf_plot

## Cubist
best_cubist <- res_tune_log_mean_mehg_1 %>%
  extract_workflow_set_result("basic_cubist") %>%
  select_best(metric = "rmse")

cubist_final_fit <- 
  wflowset_log_mean_mehg %>%
  extract_workflow("basic_cubist") %>%
  finalize_workflow(best_cubist) %>%
  fit(data=rd_train)

cubist_pred <- predict(cubist_final_fit, new_data = rd_test)

cubist_plot <- cubist_pred %>%
  cbind(observed = rd_test$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.y = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

cubist_plot

## SVM-RBF
best_svmrbf <- res_tune_log_mean_mehg_2 %>%
  extract_workflow_set_result("norm_svmrbf") %>%
  select_best(metric = "rmse")

svmrbf_final_fit <- 
  wflowset_log_mean_mehg_all %>%
  extract_workflow("norm_svmrbf") %>%
  finalize_workflow(best_svmrbf) %>%
  fit(data=rd_train_transformed)

svmrbf_pred <- predict(svmrbf_final_fit, new_data = rd_test_transformed)

svmrbf_plot <- svmrbf_pred %>%
  cbind(observed = rd_test_transformed$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 14),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

svmrbf_plot

## GLM
best_glm <- res_tune_log_mean_mehg_2 %>%
  extract_workflow_set_result("basic_glm") %>%
  select_best(metric = "rmse")

glm_final_fit <- 
  wflowset_log_mean_mehg_all %>%
  extract_workflow("basic_glm") %>%
  finalize_workflow(best_glm) %>%
  fit(data=rd_train_transformed)

glm_pred <- predict(glm_final_fit, new_data = rd_test_transformed)

glm_plot <- glm_pred %>%
  cbind(observed = rd_test_transformed$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

glm_plot

## Linear regression
lm_fit <- lm(log_mean_mehg ~., data = rd_train)
lm_predict_test <- predict(lm_fit, newdata = rd_test)
lm_plot_data <- as.data.frame(cbind(observed = rd_test$log_mean_mehg, 
                                    predicted = lm_predict_test))
lm_plot <- lm_plot_data %>%
  ggplot(aes(x=observed, y=predicted))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  #xlim(c(0,3)) + 
  #ylim(c(0,3)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

lm_plot
rsq_lm <- cor(rd_test$log_mean_mehg, lm_predict_test)^2
rmse_lm <- sqrt(mean((rd_test$log_mean_mehg - lm_predict_test)^2))
rsq_lm
rmse_lm

# Model performance metrics=====================================================
## rsq and rmse on training set
model_names_tr <- c("glm_final_fit", "svmrbf_final_fit")

results_norm_tr <- data.frame(Model = character(), R_squared = numeric(), 
                              RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_tr)) {
  model_name <- model_names_tr[i]
  predicted_values <- predict(get(model_name), new_data = rd_train_transformed)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_train_transformed$log_mean_mehg - mean(rd_train_transformed$log_mean_mehg))^2)
  ss_residual <- sum((rd_train_transformed$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_train_transformed$log_mean_mehg - predicted_values)^2))
  results_norm_tr <- rbind(results_norm_tr, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

model_names_basic_tr <- c("rf_final_fit", "cubist_final_fit")

results_basic_tr <- data.frame(Model = character(), R_squared = numeric(), 
                               RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_basic_tr)) {
  model_name <- model_names_basic_tr[i]
  predicted_values <- predict(get(model_name), new_data = rd_train)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_train$log_mean_mehg - mean(rd_train$log_mean_mehg))^2)
  ss_residual <- sum((rd_train$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_train$log_mean_mehg - predicted_values)^2))
  results_basic_tr <- rbind(results_basic_tr, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

## rsq and rmse on test set
model_names <- c("glm_pred", "svmrbf_pred")

results_norm <- data.frame(Model = character(), R_squared = numeric(), 
                           RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names)) {
  model_name <- model_names[i]
  predicted_values <- get(model_name)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_test_transformed$log_mean_mehg - mean(rd_test_transformed$log_mean_mehg))^2)
  ss_residual <- sum((rd_test_transformed$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_test_transformed$log_mean_mehg - predicted_values)^2))
  results_norm <- rbind(results_norm, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

model_names_basic <- c("rf_pred", "cubist_pred")

results_basic <- data.frame(Model = character(), R_squared = numeric(), 
                            RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_basic)) {
  model_name <- model_names_basic[i]
  predicted_values <- get(model_name)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_test$log_mean_mehg - mean(rd_test$log_mean_mehg))^2)
  ss_residual <- sum((rd_test$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_test$log_mean_mehg - predicted_values)^2))
  results_basic <- rbind(results_basic, 
                         data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}




