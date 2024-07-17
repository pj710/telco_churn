# GRID SEARCH & CROSS VALIDATION ----

deeplearning_h2o <- h2o.loadModel("04_ Modeling/h2o_models/DeepLearning_grid_1_AutoML_2_20230530_113743_model_1")
deeplearning_h2o

# 1. check model performance on test_tbl ----
h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))

# 2. 
deeplearning_grid_01 <- h2o.grid(
                            algorithm = "deeplearning", 
                            grid_id = "deeplearning_grid_01",
                            
                            # h2o.deeplearning()
                            x = x,
                            y = y,
                            training_frame = train_h2o,
                            validation_frame = valid_h2o,
                            nfolds = 5,
                            hyper_params = list(
                                hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
                                epochs = c(10, 50, 100)
                            )
)
                                     
deeplearning_grid_01

h2o.getGrid(grid_id = "deeplearning_grid_01", sort_by = "auc", decreasing = TRUE)

deeplearning_grid_01_model_1 <- h2o.getModel("deeplearning_grid_01_model_1")

deeplearning_grid_01_model_1 %>% h2o.auc(train = T, valid = T, xval = T)

deeplearning_grid_01_model_1 %>% 
    h2o.performance(newdata = as.h2o(test_tbl))
    
