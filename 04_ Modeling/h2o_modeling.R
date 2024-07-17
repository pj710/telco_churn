# Predicting Telco Customer Churn ----
# H20 Modeling

# 1. Setup ----
# Load Libraries 

library(h2o)
library(recipes)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)

# Load Data
telco_raw_tbl <- read_csv("00_Data/telco_churn_data.csv")

# Split Data
h2o.init()

split_h2o <- h2o.splitFrame(data = as.h2o(telco_processed_tbl), ratios = 0.75, seed = 1234)

train_tbl <- split_h2o[[1]] %>% as_tibble()
test_tbl  <-  split_h2o[[2]] %>% as_tibble()

# Processing Pipeline
source("00_Scripts/process_hr_tbl.R")
telco_processed_tbl <- processed_hr_tbl(telco_raw_tbl)


# ML preprocessing

recipe_obj <- recipe(Churn ~ ., data = telco_processed_tbl %>% select(-customerID)) %>%
    step_YeoJohnson(TotalCharges) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()

ml_processed_tbl <- bake(recipe_obj, telco_processed_tbl)

ml_processed_tbl <- ml_processed_tbl %>%
    select(Churn, everything())

# 2. Modeling ----

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(ml_processed_tbl), ratios = 0.75, seed = 1234)

train_h2o <- split_h2o[[1]]
test_h2o  <- split_h2o[[2]]

# specify target and predictors
y <- "Churn"
x <- setdiff(names(train_h2o), y)

# Run Auto ML
auto_models_h2o <- h2o.automl(x = x,
                              y = y,
                              training_frame = train_h2o,
                              nfolds = 5, 
                              max_runtime_secs = 30)


# inspect automl output

h2o_leaderboard <- auto_models_h2o@leaderboard




# extracting models
h2o.getModel("StackedEnsemble_BestOfFamily_3_AutoML_1_20230530_110201")

extract_h2o_model_name_by_position <-  function(h2o_leaderboard, n = 1, verbose = TRUE) {
    
    model_name <-  h2o_leaderboard %>%
        as_tibble() %>%
        slice(n) %>%
        pull(model_id)
    
    if(verbose) message(model_name)
    
    return(model_name)
    
}

h2o_leaderboard %>%
    extract_h2o_model_name_by_position(1) %>%
    h2o.getModel()

# Making predictions ----

stacked_ensemble_01 <- h2o_leaderboard %>%
    extract_h2o_model_name_by_position(1) %>%
    h2o.getModel()

stacked_ensemble_01@parameters

stacked_ensemble_01_predictions <- h2o.predict(object = stacked_ensemble_01, newdata = test_h2o)

test_tbl <- test_h2o %>%
    as_tibble()

stacked_ensemble_01_predictions <- stacked_ensemble_01_predictions %>%
    as_tibble() %>%
    cross_join(test_tbl)

# 4. Assessing Model Performance ----

source("00_Scripts/h2o_performance")

stacked_ensemble_performance_h2o <- h2o.performance(model = stacked_ensemble_01, newdata = test_h2o)

stacked_ensemble_performance_h2o@metrics

stacked_ensemble_performance_tbl <- stacked_ensemble_performance_h2o %>%
    h2o.metric() %>%
    as_tibble()

# Precision vs Recall Graph
stacked_ensemble_performance_tbl %>%
    ggplot(aes(x = threshold)) +
    geom_line(aes(y = precision), color = 'blue', size = 1) +
    geom_line(aes(y = recall), color = 'red', size = 1) +
    geom_vline(xintercept = h2o.find_threshold_by_max_metric(stacked_ensemble_performance_h2o, "f1")) +
    theme_tq() +
    labs(
        title = "Precision vs Recall", y = "Value"
    )

plot_h2o_performance(h2o_leaderboard = h2o_leaderboard, newdata = test_h2o, max_models = 5, size = 1)

h2o_leaderboard %>%
    plot_h2o_leaderboard(order_by = "auc", n_max = 20)

dump(c("get_gain_lift", "get_model_performance", "plot_h2o_performance", "load_model_performance_metrics", "plot_h2o_leaderboard", "extract_h2o_model"), file = "00_Scripts/h2o_performance")
