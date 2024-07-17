# EVALUATION: EXPECTED VALUE OF RETENTION OFFER 
# Target top likely to churn 

# 1. Setup ----
# Load Libraries
library(tidyverse)
library(tidyquant)
library(recipes)
library(h2o)
library(cowplot)
library(glue)


# Load Data
telco_raw_tbl <- read_csv("00_Data/telco_churn_data.csv")

# Processing Pipeline
source("00_Scripts/process_hr_tbl.R")
source("00_Scripts/plot_cor.R")
source("00_Scripts/plot_ggpairs.R")
source("00_Scripts/plot_hist_facet.R")
source("00_Scripts/h2o_performance")


telco_processed_tbl <- processed_hr_tbl(telco_raw_tbl)

# Split Data 

h2o.init()

split_h2o <- h2o.splitFrame(data = as.h2o(telco_processed_tbl), ratios = 0.75, seed = 1234)

train_tbl <- split_h2o[[1]] %>% as_tibble()
test_tbl  <-  split_h2o[[2]] %>% as_tibble()

train_tbl <- train_tbl %>%
    mutate(annual_revenue_per_customer = MonthlyCharges * 12)

test_tbl <- test_tbl %>%
    mutate(annual_revenue_per_customer = MonthlyCharges * 12)

# Correlation Analysis

telco_processed_tbl %>%
    plot_cor(target = Churn, fct_reorder = TRUE, size = 3)

telco_processed_tbl %>%
    select(Contract, tenure, PaymentMethod, Churn) %>%
    plot_ggpairs(color = Churn)

telco_processed_tbl %>%
    plot_hist_facet()


telco_processed_tbl %>%
    skim()
# ML Processing ----

recipe_obj <- recipe(Churn ~ ., data = telco_processed_tbl %>% select(-customerID)) %>%
    step_YeoJohnson(TotalCharges) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()

train_ml_processed_tbl <- bake(object = recipe_obj, new_data = train_tbl %>% select(-customerID))

test_ml_processed_tbl <- bake(object = recipe_obj, new_data = test_tbl %>% select(-customerID))

means_tbl <- recipe_obj[["steps"]][[2]][["means"]]
stdevs_tbl <- recipe_obj[["steps"]][[3]][["sds"]]

# RUN AutoML
set.seed(1234)
train_h2o <- as.h2o(train_ml_processed_tbl)
test_h2o  <-  as.h2o(test_ml_processed_tbl)

y <- "Churn"
x <- setdiff(names(train_h2o), y)

automl_h2o_models <- h2o.automl(x = x,
                                y = y, 
                                training_frame = train_h2o,
                                nfolds = 5, 
                                max_runtime_secs = 30)

h2o_leaderboard <- automl_h2o_models@leaderboard

plot_h2o_leaderboard(h2o_leaderboard, n_max = 10)

automl_leader <- automl_h2o_models@leader

automl_leader@parameters

automl_leader_performance <- h2o.performance(automl_leader, newdata = test_h2o)

automl_leader_performance %>%
    h2o.confusionMatrix()

rates_by_threshold_tbl <- automl_leader_performance %>% h2o.metric() %>%
    as_tibble()


rates_by_threshold_tbl %>% 
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    pivot_longer(names_to = "key", values_to = "value", cols = c(tnr:tpr)) %>%
    mutate(key = as_factor(key)) %>%
    mutate(key = fct_reorder2(key, threshold, value)) %>%
    ggplot(aes(threshold, value, color = key)) +
    geom_point() +
    geom_smooth() +
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "right") +
    labs(
        title = "Expected Rates",
        y = "Value" , x = "Threshold"
    )

plot_h2o_performance(h2o_leaderboard, newdata = test_h2o, max_models = 5)

# 3. Expected Value ----

# 3.1 Calculating Expected Value based on probability of churn ----

automl_leader_predictions_tbl <- automl_leader %>%
                                        h2o.predict(newdata = test_h2o) %>%
                                        as_tibble() %>%
                                        bind_cols(test_tbl) %>%
                                        select(customerID, Churn, predict, No, Yes, MonthlyCharges)

ev_without_discount_tbl <- automl_leader_predictions_tbl %>%
    mutate(annual_revenue_per_customer = MonthlyCharges * 12,
           cost_of_discount            =  0) %>%
    mutate(expected_loss_of_revenue_by_churn  =  (Yes * (annual_revenue_per_customer - cost_of_discount)) + (No * cost_of_discount))
    

total_ev_without_discount_tbl <- ev_without_discount_tbl %>%
    summarise(
        total_expected_revenue_loss_00 = sum(expected_loss_of_revenue_by_churn)
    )

# Apply 15% discount to all customers ----

ev_with_discount_tbl <- automl_leader_predictions_tbl %>%
    mutate(annual_revenue_per_customer = MonthlyCharges * 12) %>%
    mutate(cost_of_discount            = annual_revenue_per_customer * 0.05) %>%
    mutate(expected_loss_of_revenue_by_churn  =  (Yes * (annual_revenue_per_customer - cost_of_discount)) + (No * cost_of_discount))

total_ev_with_discount_tbl <- ev_with_discount_tbl %>%
    summarise(
        total_expected_revenue_loss_01 = sum(expected_loss_of_revenue_by_churn)
    )

# Savings Calculations ----

total_ev_with_discount_tbl %>%
    bind_cols(total_ev_without_discount_tbl) %>%
    mutate(
        savings = total_expected_revenue_loss_00 - total_expected_revenue_loss_01,
        pct_savings = savings / total_expected_revenue_loss_00
    )


# 4.2 Calculating Expected Value With Discount Targeting - Target customers based on prob threshold and apply 5% discount

# Threshold by max_f1

max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr
threshold <- max_f1_tbl$threshold

# manipulate test_tbl to target customers for 5% discount based on threshold for max_f1 

test_targeted_discount_tbl <- test_tbl %>%
    add_column(Yes = automl_leader_predictions_tbl$Yes) %>%
    mutate(MonthlyCharges = case_when(Yes >= threshold ~ MonthlyCharges - (MonthlyCharges * 0.05), TRUE ~ MonthlyCharges)) %>%
    select(-Yes)

# get predictions for targeted test_tbl
ml_test_targeted_discount_tbl <- bake(recipe_obj, new_data = test_targeted_discount_tbl %>% select(-customerID))

predictions_targeted_tbl <- h2o.predict(object = automl_leader, newdata = as.h2o(ml_test_targeted_discount_tbl))

predictions_targeted_tbl <- predictions_targeted_tbl %>%
    as_tibble() %>%
    bind_cols(test_targeted_discount_tbl %>%
                  select(customerID, Churn, MonthlyCharges)) %>%
    select(customerID, Churn, predict, Yes, No, MonthlyCharges)

# calc expected value

ev_with_targeted_tbl <- predictions_targeted_tbl %>%
                                mutate(MonthlyCharges_00 = test_tbl %>% select(MonthlyCharges)) %>%
                                mutate(annual_revenue_per_customer_01 = MonthlyCharges * 12) %>%
                                mutate(annual_cost_of_discount = (MonthlyCharges_00 - MonthlyCharges) * 12 ) %>%
               
                                     # estimate cost/benefit
                mutate(
                cb_tn = annual_cost_of_discount,
                cb_fp = annual_cost_of_discount,
                cb_tp = annual_revenue_per_customer_01 - annual_cost_of_discount,
                cb_fn = annual_revenue_per_customer_01 - annual_cost_of_discount,
                expected_loss_of_revenue_by_churn = Yes * (tpr * cb_tp + fnr * cb_fn) + No * (tnr * cb_tn + fpr * cb_fp)
            )

total_ev_with_targeted_discount_tbl <- ev_with_targeted_tbl %>%
    summarise(
        total_expected_revenue_loss_03 = sum(expected_loss_of_revenue_by_churn)
    )

savings_targeted_tbl <- bind_cols(
    total_ev_without_discount_tbl, total_ev_with_targeted_discount_tbl,
) %>%
    mutate(
        savings = total_expected_revenue_loss_00 - total_expected_revenue_loss_03,
        pct_savings = savings / total_expected_revenue_loss_00
    )

data <- test_tbl
h2o_model <- automl_leader
discount_rate <- 0.05

# 5.1 Create calculate_savings_by_threshold() ----
calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0, discount_rate = 0.05,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
    
    
    data_0_tbl <- as_tibble(data)
    
    ml_data_0_tbl <- bake(object = recipe_obj, new_data = test_tbl %>% select(-customerID))
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value Without discount targeting
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(ml_data_0_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_0_tbl %>%
                    select(customerID, Churn, MonthlyCharges)) %>%
        select(customerID, Churn, predict, Yes, No, MonthlyCharges)
    
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            annual_revenue_per_customer = MonthlyCharges * 12
            ) %>%
        mutate(
            annual_cost_of_discount = 0
            ) %>%
        mutate(
            expected_revenue_loss = 
                Yes * (annual_revenue_per_customer - annual_cost_of_discount) +
                No  *  (annual_cost_of_discount)
            )
    
    
    total_ev_0_tbl <- ev_0_tbl %>%
        summarise(
            total_expected_revenue_loss_00 = sum(expected_revenue_loss)
        )
    
    # 4.2 Calculating Expected Value With Targeted OT
    
    data_1_tbl <- data_0_tbl %>%
        add_column(Yes = pred_0_tbl$Yes) %>%
        mutate(
            MonthlyCharges = case_when(
                Yes >= threshold ~ MonthlyCharges - (MonthlyCharges * discount_rate),
                TRUE ~ MonthlyCharges
            )
        ) %>%
        select(-Yes) 
    
    ml_data_1_tbl <- bake(recipe_obj, new_data = data_1_tbl %>% select(-customerID))
    
    pred_1_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(ml_data_1_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_1_tbl %>%
                select(customerID, Churn, MonthlyCharges)) %>%
        select(customerID, Churn, predict, Yes, No, MonthlyCharges)
        
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            annual_revenue_per_customer = MonthlyCharges * 12
        ) %>%
        mutate(
          annual_cost_of_discount = (data_0_tbl$MonthlyCharges - MonthlyCharges) * 12
        ) %>%
        mutate(
            cb_tn = annual_cost_of_discount,
            cb_fp = annual_cost_of_discount,
            cb_fn = annual_revenue_per_customer - annual_cost_of_discount,
            cb_tp = annual_revenue_per_customer - annual_cost_of_discount,
            expected_revenue_loss = Yes * (tpr * cb_tp + fnr * cb_fn) + No * (tnr * cb_tn + fpr * cb_fp)
        )
    
    
    total_ev_1_tbl <- ev_1_tbl %>%
        summarise(
            total_expected_revenue_loss_01 = sum(expected_revenue_loss)
        )
    
# 4.3 Savings Calculation
savings_tbl <- bind_cols(
        total_ev_0_tbl,
        total_ev_1_tbl
    ) %>%
        mutate(
            savings = total_expected_revenue_loss_00 - total_expected_revenue_loss_01,
            pct_savings = savings / total_expected_revenue_loss_00
        )

return(savings_tbl$savings)

}

calculate_savings_by_threshold(data = test_tbl, h2o_model = automl_leader, threshold = threshold, discount_rate = 0.05,tnr = tnr, fpr = fpr, fnr = fnr, tpr = tpr)

# Threshold @ Max f1

max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1))  

max_f1_savings <- calculate_savings_by_threshold(test_tbl,h2o_model = automl_leader,threshold = max_f1_tbl$threshold,
                                                 tnr = max_f1_tbl$tnr,
                                                 fnr = max_f1_tbl$fnr,
                                                 fpr = max_f1_tbl$fpr,
                                                 tpr = max_f1_tbl$tpr)

# No discount Policy 
test_tbl %>%
    calculate_savings_by_threshold(h2o_model = automl_leader, threshold = 0, discount_rate = 0,
                                   tnr = 0, fnr = 0,tpr = 1, fpr = 1)

# Discount to all 
test_tbl %>%
    calculate_savings_by_threshold(h2o_model = automl_leader, threshold = 1, discount_rate = 0.05,tnr = 0, fnr = 0,tpr = 1, fpr = 1)


# 5.2 Threshold Optimization ----

smpl = seq(1, 220, length.out = 20) %>% round(digits = 0)

partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader, discount_rate = 0.05)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
    select(threshold, tnr:tpr) %>%
    slice(smpl) %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                threshold = threshold,
                tnr = tnr,
                fnr = fnr,
                fpr = fpr,
                tpr = tpr
            ),
            .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
        )
    )

max_savings_tbl <- rates_by_threshold_optimized_tbl %>%
    filter(savings == max(savings))

rates_by_threshold_optimized_tbl %>%
    ggplot(aes(threshold, savings)) +
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    
    # Optimal Point
    geom_point(shape = 21, size = 5, color = palette_light()[[3]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(savings == max(savings))) +
    geom_vline(xintercept = max_savings_tbl$threshold,
               color = palette_light()[[5]], linewidth = 2) +
    geom_label(aes(label = scales::dollar(savings)),
           vjust = -0.5, color = palette_light()[[3]],
           data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +
    # F1 Max
    geom_label(aes(label = scales::dollar(savings)),
           vjust = -0.5, color = palette_light()[[3]],
           data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +
    annotate(geom = "label", label = scales::dollar(max_f1_savings),
             x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -0.5) +
    
    # No discount Targeting Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold))) +
    geom_label(aes(label = scales::dollar(savings)),
               vjust = -0.5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold))) +
    
    # Do Nothing Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold))) +
    geom_label(aes(label = scales::dollar(round(savings, 0))),
               vjust = -1, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold))) +
    # Aesthetics 
    theme_tq() +
    expand_limits(y = 2e4) +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq(0,1, by = 0.1)) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Optimization Results: Expected Savings Maximized at 33.9%",
        x = "Threshold (%)", y = "Savings"
    )

# 6.2 Sensitivity Analysis ----

max_savings_rates_tbl <- rates_by_threshold_optimized_tbl %>%
    filter(savings == max(savings))

max_savings_rates_tbl

calculate_savings_by_threshold(
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    fnr = max_savings_rates_tbl$fnr,
    tpr = max_savings_rates_tbl$tpr,
    fpr = max_savings_rates_tbl$fpr
)

# create preloaded partial function
calculate_savings_by_threshold_preloaded <- partial(
    calculate_savings_by_threshold,
    # function arguments
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    fnr = max_savings_rates_tbl$fnr,
    tpr = max_savings_rates_tbl$tpr,
    fpr = max_savings_rates_tbl$fpr
)

calculate_savings_by_threshold_preloaded(discount_rate = 0.15)




