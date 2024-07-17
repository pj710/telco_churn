# PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# H2O MODELING ----

# 1. Setup ----

# Load Libraries
library(h2o)
library(recipes)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)
library(rsample)
library(parsnip)
library(yardstick)
library(rpart.plot)
library(xgboost)
library(workflowsets)


source("00_Scripts/process_hr_tbl.R")




# Data ----
telco_churn_raw_tbl <- read_csv("00_Data/telco_churn_data.csv")

telco_processed_tbl <- processed_hr_tbl(telco_churn_raw_tbl) 

# ML Processing ----

recipe_obj <- recipe(Churn ~ ., data = telco_processed_tbl %>% select(-customerID)) %>%
    step_YeoJohnson(TotalCharges) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()

ml_processed_tbl <- bake(object = recipe_obj, telco_processed_tbl)

ml_processed_tbl <- ml_processed_tbl %>%
    select(Churn, everything())

# Modeling XGBOOST ----

split_obj <- rsample::initial_split(ml_processed_tbl,prop = 0.75)

train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)


train_tbl %>%
    dplyr::slice(1:100) %>%
    ggplot(aes(x = TotalCharges, y = tenure, color = Churn)) +
    geom_point()

set.seed(1234)
model_01_xgboost <- boost_tree(mode = "classification",
                               engine = 'xgboost', 
                               mtry = 30, 
                               trees = 200, 
                               min_n = 20, 
                               tree_depth = 5, 
                               learn_rate = 0.2) %>% 
                    fit(Churn ~ ., data = train_tbl %>%
                                                mutate_if(is.character, factor))


predictions <- predict(object = model_01_xgboost, new_data = test_tbl) %>%
    bind_cols(test_tbl)

# Extract the predicted class probabilities
probabilities <- as.data.frame(predict_classprob.model_fit(model_01_xgboost, new_data = test_tbl))

class_probability_tbl <- probabilities %>%
    bind_cols(predictions_tbl) %>%
    rename("pred_class" = ".pred_class") %>%
    as_tibble() 

class_probability_tbl%>%
    yardstick::metrics(truth = Churn, estimate = pred_class)

multi_metric <- metric_set(precision, recall, accuracy, confusion_matrix)

class_probability_tbl %>%
    multi_metric(truth = Churn, estimate = pred_class)

class_probability_tbl %>%
    confusionMatrix(data = class_probability_tbl$pred_class, 
                    reference = class_probability_tbl$Churn, 
                    positive = "Yes", 
                    dnn = c("Prediction", "Actual"), 
                    prevalence = 0.27, 
                    mode = "prec_recall")


model_01_xgboost$fit %>%
    xgboost::xgb.importance(model = .) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    mutate(Feature = as_factor(Feature) %>% fct_rev()) %>%
    ggplot(aes(Gain, Feature)) +
    geom_point() +
    labs(
        title = "XGBoost: Variable Importance",
        subtitle = "Model 01: XGBoost Model"
    )

model_01_xgboost$fit  %>%
   xgboost::xgb.plot.multi.trees(model = ., )

# ---- save model

fs::dir_create(path = "04_ Modeling/Models")

saveRDS(model_01_xgboost, file = "04_ Modeling/Models/model_01_xgboost") 

# Modeling Random Forest ----

model_02_randforest <- rand_forest(mode = "classification", engine = "randomForest", mtry = 23, trees = 200) %>%
                                 fit(Churn ~ ., data = train_tbl %>%  mutate_if(is.character, factor))


model_02_randforest %>%
    predict(.,new_data = test_tbl) %>%
    bind_cols(test_tbl)

