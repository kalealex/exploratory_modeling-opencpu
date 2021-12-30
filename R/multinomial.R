multinomial_model_check <- function(spec, data) {
  
  n_draws <- 30

  # read data from json
  data <- fromJSON(data, simplifyVector = TRUE)
  
  # # catch values of negative inf on log transform
  # log_trans_vars_mu <- str_match_all(spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  # for (var_name in log_trans_vars_mu) {
  #   # replace log({{var}}) with log_{{var}} in spec
  #   spec <- str_replace_all(spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  # }
  
  # fit model
  spec <- as.formula(spec)
  model <- eval(bquote(brm(
    formula = bf(.(spec)),
    family = multinomial(),
    chains = 2,
    cores = 2,
    iter = 1000,
    warmup = 500,
    data = data
  )))
  
  # get predictive distribution (look how much easier this is with a Bayesian model)
  output <- data %>%
    add_predicted_draws(model, seed = 14, n = n_draws)

  return(list(message = "success", data = toJSON(output)))
}