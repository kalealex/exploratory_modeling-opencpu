negbinomial_model_check <- function(mu_spec, sigma_spec = "~1", data) {
  
  # settings
  n_draws <- 5

  # read data from json
  data <- fromJSON(data, simplifyVector = TRUE)
  
  # # catch values of negative inf on log transform
  # log_trans_vars_mu <- str_match_all(mu_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  # for (var_name in log_trans_vars_mu) {
  #   # replace log({{var}}) with log_{{var}} in mu_spec
  #   mu_spec <- str_replace_all(mu_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  # }
  # log_trans_vars_sigma <- str_match_all(sigma_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  # for (var_name in log_trans_vars_sigma) {
  #   # replace log({{var}}) with log_{{var}} in sigma_spec
  #   sigma_spec <- str_replace_all(sigma_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  # }

  # get outcome variable and model names
  outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
  model_name <- sym(paste("negbinomial", mu_spec, sigma_spec, sep = "| "))
  
  # fit model
  mu_spec <- as.formula(mu_spec)
  sigma_spec <- as.formula(sigma_spec)
  model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data, family = NBII)))
  
  # get summary statistics describing model predictions
  pred.mu <- predict(model, se.fit = TRUE)
  pred.sigma <- predict(model, what = "sigma", se.fit = TRUE)
  output <- data %>%
    mutate(
      logmu.expectation = pred.mu$fit,                    # add fitted logmu and standard errors to dataframe
      logmu.se = pred.mu$se.fit,
      logsigma.expectation = pred.sigma$fit,              # add fitted logsigma and standard errors to dataframe 
      logsigma.se = pred.sigma$se.fit#,
      # df = df.residual(model),                            # get degrees of freedom
      # residual.se = sqrt(sum(residuals(model)^2) / df)    # get residual standard errors //todo: propagate
    )
  
  # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
  output <- output %>%
    mutate(
      draw = list(1:n_draws),                             # generate list of draw numbers
      t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into log mu
      t2 = map(df, ~rt(n_draws, .))#,                     # simulate draws from t distribution to transform into log sigma
      # x = map(df, ~rchisq(n_draws, .))                    # simulate draws from chi-squared distribution to transform into residual sigmas
    ) %>%
    unnest(cols = c("draw", "t1", "t2")) %>%
    mutate(
      logmu = t1 * logmu.se + logmu.expectation,          # scale and shift t to get a sampling distribution of log mu
      logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
      mu = exp(logmu),                                    # backtransform to sampling distributions of mu and sigma parameters
      sigma = exp(logsigma)#,
      # residual.sigma = sqrt(df * residual.se^2 / x)       # scale and take inverse of x to get a sampling distribution of sigmas
    ) %>%
    rowwise() %>%
    mutate(
      # compute predictive distribution of counts
      prediction = rNBII(1, mu, sigma) #+ rNBII(1, 0, residual.sigma)
    ) %>%
    rename(
      data = !!outcome_name,
      !!model_name := prediction
    ) %>%
    pivot_longer(
      cols = c("data", model_name),
      names_to = "modelcheck_group",
      values_to = as.character(outcome_name)
    )

  return(list(message = "success", data = toJSON(output)))
}