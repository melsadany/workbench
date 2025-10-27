require(vip)
require(DALEX)
require(iml)
require(pdp)
require(glmnet)
require(caret)


# Comprehensive feature importance analysis
analyze_feature_importance <- function(data, target_var, feature_cols, model_type = "glmnet",feat.n = 10) {
  
  # Prepare data
  model_data <- data %>% 
    dplyr::select(all_of(c(target_var, feature_cols))) %>%
    na.omit()
  
  # Train model
  if (model_type == "glmnet") {
    model <- train(
      as.formula(paste(target_var, "~ .")),
      data = model_data,
      method = "glmnet",
      trControl = trainControl(method = "cv", number = 10),
      tuneLength = 10
    )
  } else if (model_type == "rf") {
    model <- train(
      as.formula(paste(target_var, "~ .")),
      data = model_data,
      method = "ranger",
      importance = 'permutation',
      trControl = trainControl(method = "cv", number = 10)
    )
  }
  
  # Method 1: Model-specific variable importance
  vip_model <- vip(model, num_features = 15)
  
  # Method 2: Permutation importance (model-agnostic)
  explainer <- DALEX::explain(model, data = model_data[, feature_cols], 
                              y = model_data[[target_var]])
  perm_importance <- model_parts(explainer, loss_function = loss_root_mean_square)
  
  # Method 3: SHAP values (if using tree-based model)
  if (model_type == "rf") {
    predictor <- Predictor$new(model, data = model_data[, feature_cols], 
                               y = model_data[[target_var]])
    shapley <- Shapley$new(predictor, x.interest = model_data[1, feature_cols])
    # Note: Full SHAP for all observations is computationally intensive
  }
  
  return(list(
    target = target_var,
    model = model,
    vip_plot = vip_model,
    permutation_importance = perm_importance,
    top_features = get_top_features(vip_model, perm_importance, n = feat.n)
  ))
}

# Extract top features across methods
get_top_features <- function(vip_obj, perm_importance, n = feat.n) {
  # From VIP
  vip_features <- vip_obj$data %>%
    arrange(desc(Importance)) %>%
    head(n) %>%
    pull(Variable)
  
  # From permutation importance
  perm_features <- perm_importance %>%
    group_by(variable) %>%
    summarise(mean_dropout = mean(dropout_loss)) %>%
    arrange(desc(mean_dropout)) %>%
    head(n) %>%
    pull(variable)
  
  # Consensus features
  consensus <- intersect(vip_features, perm_features)
  
  return(list(
    vip_top = vip_features,
    perm_top = perm_features,
    consensus = consensus
  ))
}
