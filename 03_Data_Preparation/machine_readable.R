# Data Preparation ----
# Machine Readable ----


# Libraries

library(tidyverse)
library(tidyquant)
library(recipes)

telco_churn_raw_tbl <- read_csv("00_Data/telco_churn_data.csv")

# Processing Pipeline ----
source("00_Scripts/data_processing_pipeline_rev1.R")
source("00_Scripts/plot_hist_facet.R")
source("00_Scripts/plot_cor.R")

telco_processed_tbl <- processed_hr_tbl(telco_churn_raw_tbl)

telco_processed_tbl %>%
    select(Churn, everything()) %>%
    plot_hist_facet(fct_reorder = F,fct_rev = F, bins = 10,ncol = 4)

# Data Preprocessing With Recipes ----

recipe_obj <- recipe(Churn ~ ., data = telco_processed_tbl %>% select(-customerID)) %>%
    step_YeoJohnson(TotalCharges) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    prep()

telco_machine_processed_tbl <- recipe_obj %>%
    bake(new_data = telco_processed_tbl)

# Correlation Analysis ----

telco_machine_processed_tbl %>%
    get_cor(target = Churn_Yes, fct_reorder = TRUE, fct_rev = TRUE) 

telco_machine_processed_tbl %>%
    plot_cor(target = Churn_Yes, fct_reorder = T, fct_rev = F, size = 2.5,
             include_lbl = TRUE, lbl_precision = 2)
    
