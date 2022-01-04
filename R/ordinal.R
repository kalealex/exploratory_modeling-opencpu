ordinal_model_check <- function(mu_spec, disp_spec = "~1", data) {
  
  n_draws <- 30
  
  # read data from json
  data <- fromJSON(data, simplifyVector = TRUE)

  # # catch values of negative inf on log transform
  # log_trans_vars_mu <- str_match_all(mu_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  # for (var_name in log_trans_vars_mu) {
  #   # replace log({{var}}) with log_{{var}} in mu_spec
  #   mu_spec <- str_replace_all(mu_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  # }
  # log_trans_vars_disp <- str_match_all(disp_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  # for (var_name in log_trans_vars_disp) {
  #   # replace log({{var}}) with log_{{var}} in sigma_spec
  #   disp_spec <- str_replace_all(disp_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  # }
  disp_spec <- paste("disc", disp_spec)

  # get outcome variable and model names
  outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
  model_name <- sym(paste("normal", mu_spec, sigma_spec, sep = "| "))
  
  # fit model
  mu_spec <- as.formula(mu_spec)
  disp_spec <- as.formula(disp_spec)
  model <- eval(bquote(brm(
    formula = bf(
      .(mu_spec),
      .(disp_spec)),
    family = cumulative("probit"),
    chains = 2,
    cores = 2,
    iter = 1000,
    warmup = 500,
    data = data
  )))
  
  # get predictive distribution (look how much easier this is with a Bayesian model)
  output <- data %>%
    add_predicted_draws(model, seed = 14, n = n_draws) %>%
    rename(
      draw = .draw,
      data = !!outcome_name,
      !!model_name := .prediction
    ) %>%
    pivot_longer(
      cols = c("data", model_name),
      names_to = "modelcheck_group",
      values_to = as.character(outcome_name)
    )

  return(list(message = "success", data = toJSON(output)))
}