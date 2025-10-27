# Test if important features are stable across cross-validation folds
analyze_feature_stability <- function(data, target_var, feature_cols, n_iterations = 50) {
  
  set.seed(123)
  stability_results <- list()
  data <- data%>%drop_na()
  
  for (i in 1:n_iterations) {
    # Create different train/test splits
    # train_idx <- createDataPartition(data[[target_var]], p = 0.8, list = FALSE)
    train_idx <- sample(x=c(1:nrow(data)),size=round(0.8*nrow(data)))
    train_data <- data[train_idx, ]
    
    # Train model and get feature importance
    model <- train(
      as.formula(paste(target_var, "~ .")),
      data = train_data %>% select(all_of(c(target_var, feature_cols))),
      method = "glmnet",
      trControl = trainControl(method = "none"),  # No CV within this iteration
      tuneGrid = data.frame(alpha = 0.5, lambda = 0.1)
    )
    
    # Get coefficients
    coefs <- coef(model$finalModel, s = 0.1)
    if (inherits(coefs, "dgCMatrix")) {
      feature_weights <- as.matrix(coefs)[-1, 1]  # Remove intercept
    } else {
      feature_weights <- coefs[-1, 1]
    }
    
    stability_results[[i]] <- data.frame(
      iteration = i,
      feature = names(feature_weights),
      weight = feature_weights,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine results
  all_results <- bind_rows(stability_results)
  
  # Calculate stability metrics
  feature_stability <- all_results %>%
    group_by(feature) %>%
    summarise(
      mean_weight = mean(weight, na.rm = TRUE),
      sd_weight = sd(weight, na.rm = TRUE),
      stability_index = 1 - (sd_weight / (abs(mean_weight) + 0.001)),  # Avoid division by zero
      selection_frequency = mean(weight != 0),
      .groups = 'drop'
    ) %>%
    arrange(desc(abs(mean_weight)))
  
  return(list(
    stability_data = all_results,
    feature_stability = feature_stability,
    top_stable_features = feature_stability %>% 
      filter(selection_frequency > 0.7, stability_index > 0.5) %>%
      head(10)
  ))
}
