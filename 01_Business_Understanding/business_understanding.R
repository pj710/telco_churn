# Import Libraries ----
library(tidyverse)
library(tidyquant)
library(dplyr)
library(ggplot2)

# Import Data ----
telco_churn_raw_tbl <- read_csv("00_Data/telco_churn_data.csv")

telco_churn_raw_tbl %>%
    glimpse()


# 1. Business Science Problem Framework ----

# 1A. View Business As Machine ----

# "Telco offers phone and internet services to customers from the United States."

# BSU's: Phone Service,  Internet Service
# Define Objectives: Minimize revenue loss from customer churn.
# Assess Problem: Telco's overall churn rate is 27% compared to industry average churn rate of 22%.

overall_churn_tbl <- telco_churn_raw_tbl %>%
    select(customerID, PhoneService, InternetService, MonthlyCharges, TotalCharges, Churn) %>%
    count(Churn, name = "churn_size") %>%
    mutate(churn_rate = churn_size / sum(churn_size))

    
# 1B. Understand The Drivers ----

# What is driving churn rate of 27% ?

# Investigate Objectives: 22% churn
# Hypothesize Drivers: Phone Service , Internet Service
# Assess outcome: High Churn rates for Fibre Optic and Phone users


# Churn vs Phone Service ----

telco_churn_raw_tbl <- telco_churn_raw_tbl %>%
    mutate(Churn = as_factor(Churn),
           PhoneService = as_factor(PhoneService),
           InternetService = as_factor(InternetService))

phone_churn_tbl <- telco_churn_raw_tbl %>%
    count(PhoneService, Churn, name = "churn_size") %>%
    
    arrange(desc(churn_size)) %>%
    filter(Churn == "Yes") %>% 
    mutate(churn_rate = churn_size / sum(churn_size))


# Churn vs Internet Service ----

internet_churn_tbl <- telco_churn_raw_tbl %>%
    select(customerID, PhoneService, InternetService, MonthlyCharges, TotalCharges, Churn) %>%
    count(InternetService, Churn, name = "churn_size") %>%
    
    arrange(desc(churn_size)) %>%
    filter(Churn == "Yes") %>% 
    mutate(churn_rate = churn_size / sum(churn_size))
    
    


# 1C. Measure The Drivers ----

# Collect Information on Customer Churn On going

# Develop KPI's: Industry KPIs: 22% churn rate

total_churn = table(telco_churn_raw_tbl$Churn)[2]

churn_rate_tbl <- telco_churn_raw_tbl %>%
    select(customerID, PhoneService, InternetService, MonthlyCharges, TotalCharges, Churn) %>%
 
    group_by(PhoneService, InternetService, Churn) %>%
    count(PhoneService, InternetService, Churn, name = "churn_size") %>%
    filter(Churn == "Yes") %>%
    group_by(PhoneService, InternetService) %>%
    mutate(churn_rate = churn_size/ total_churn) %>%
    ungroup() %>%
    arrange(desc(churn_rate))
    

    

# 1D. Uncover Problems and Opportunities ---- 

monthly_revenue_loss_to_churn_tbl <- telco_churn_raw_tbl %>%
    select(customerID, PhoneService, InternetService, MonthlyCharges, TotalCharges, Churn) %>%
    count(PhoneService,InternetService, Churn, wt = sum(MonthlyCharges), name = "Revenue_loss") %>%
    filter(Churn == "Yes") %>%
    arrange(desc(Revenue_loss))
    

total_revenue_loss_to_churn <- telco_churn_raw_tbl %>%
    select(customerID, PhoneService, InternetService, MonthlyCharges, TotalCharges, Churn) %>%
    count(PhoneService,InternetService, Churn, wt = sum(TotalCharges), name = "Revenue_loss") %>%
    filter(Churn == "Yes") %>%
    arrange(desc(Revenue_loss))  

expected_rev_loss_per_month <- monthly_revenue_loss_to_churn_tbl %>%
    inner_join(churn_rate_tbl) %>%
    mutate(expected_revenue_loss_per_month = Revenue_loss * churn_rate)

# Visualization of revenue loss and expected revenue from Churn ----

expected_rev_loss_per_month %>%
    ggplot(aes(x = InternetService, y = Revenue_loss, fill = PhoneService)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Estimated Revenue Loss Per Month", 
         y = "Revenue",
         x = "Internet Type")
    
    
    
    
    
 

    
    
