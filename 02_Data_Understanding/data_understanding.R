# Data Understanding ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

telco_churn_raw_tbl <- read_csv(file = "00_Data/telco_churn_data.csv")

# Inspect Data ----
telco_churn_raw_tbl %>%
    glimpse()



# Exploratory Data Analysis (EDA) ----

#step 1: Data Summarization ----

telco_churn_raw_tbl <- telco_churn_raw_tbl %>%
    mutate(SeniorCitizen = case_match(SeniorCitizen, 0 ~ "No", 1 ~ "Yes")) %>%
    mutate(MultipleLines = case_when(MultipleLines == "No phone service" ~ "No",
                                     TRUE ~ MultipleLines)) %>%
    mutate(OnlineSecurity = case_when(OnlineSecurity == "No internet service" ~ "No",
                                      TRUE ~ OnlineSecurity)) %>%
    mutate(OnlineBackup = case_when(OnlineBackup == "No internet service" ~ "No",
                                      TRUE ~ OnlineBackup)) %>%
    mutate(DeviceProtection = case_when(DeviceProtection == "No internet service" ~ "No",
                                      TRUE ~ DeviceProtection)) %>%
    mutate(TechSupport = case_when(TechSupport == "No internet service" ~ "No",
                                      TRUE ~ TechSupport)) %>%
    mutate(StreamingTV = case_when(StreamingTV == "No internet service" ~ "No",
                                      TRUE ~ StreamingTV)) %>%
    mutate(StreamingMovies = case_when(StreamingMovies == "No internet service" ~ "No",
                                      TRUE ~ StreamingMovies)) 
    
telco_churn_raw_tbl %>%
    skim()

# Character Data Types - 
# Explore character levels and population/size

telco_churn_raw_tbl %>%
    select_if(is.character) %>%
    map(~ table(.) %>% prop.table())


telco_churn_raw_tbl %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(desc(value))

#step 2: Data Visualization ----
telco_churn_raw_tbl %>%
    select(gender, SeniorCitizen, Partner, Dependents, tenure, MonthlyCharges, Churn) %>%
    plot_ggpairs(color = Churn)

# Explore Features by Category ----

# 1. Demographic Features: Gender, SeniorCitizen, Partner, Dependents, Tenure 
telco_churn_raw_tbl %>%
    select(gender, SeniorCitizen, Partner, Dependents, tenure, Churn) %>%
    plot_ggpairs(color = Churn)

# 2. Core Usage Features: Phone Service, Internet Service, Multiple Lines
telco_churn_raw_tbl %>%
    select(PhoneService, InternetService, MultipleLines, Churn) %>%
    plot_ggpairs(color = Churn) 

# 3. Service Add'ons:StreamingTV, StreamingMovies, Online Security, OnlineBackup, DeviceProtection, TechSupport
telco_churn_raw_tbl %>%
    select(OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Churn) %>%
    plot_ggpairs(color = Churn)


# 4. Billing Features: PaymentMethod, PaperlessBilling, Contract

telco_churn_raw_tbl %>%
    select(PaymentMethod, PaperlessBilling, Contract, Churn) %>%
    plot_ggpairs(color = Churn)

