#----prep_model----
prep_model <- function(daily_fluxes, proportion){


    #split data
    split <- rsample::initial_split(daily_fluxes, prop = proportion, strata = "VPD_F")
    daily_fluxes_train <- rsample::training(split)
    daily_fluxes_test <- rsample::testing(split)

    # scale and center data, box-cox transform
    pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                          data = daily_fluxes_train |> drop_na()) |> 
      recipes::step_BoxCox(recipes::all_predictors()) |> 
      recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
      recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())
    
  
    
    
    # Fit KNN model
    knn_model <- caret::train(
      pp, 
      data = daily_fluxes_train |> drop_na(), 
      method = "knn",
      trControl = caret::trainControl(method = "cv", number = 10),
      tuneGrid = data.frame(k = 8),
      metric = "RMSE"
    )
    
    return(list(
      knn_model = knn_model,
      daily_fluxes_train = daily_fluxes_train,
      daily_fluxes_test = daily_fluxes_test
    ))
}

#----get_metrics----
get_metrics <- function(mod, testset, model_name, test_name) {
  testset <- testset |> drop_na()
  predictors <- predict(mod, newdata = testset)
  metrics <- yardstick::metrics(bind_cols(testset, fitted = predictors), truth = GPP_NT_VUT_REF, estimate = fitted)
  rmse <- metrics |> filter(.metric == "rmse") |> pull(.estimate)
  r2 <- metrics |> filter(.metric == "rsq")  |> pull(.estimate)
  tibble::tibble(Training_set = model_name, Test_set = test_name, RMSE = rmse, R2 = r2)
}
