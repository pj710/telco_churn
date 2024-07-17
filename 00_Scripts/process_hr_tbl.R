processed_hr_tbl <-
function(data) {
    
    # Convert character variable to factors and adjust levels
    
    processed_hr_tbl <- data %>%
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
                                           TRUE ~ StreamingMovies)) %>%
        mutate_if(is.character, as_factor)
    
    return(processed_hr_tbl)
}
