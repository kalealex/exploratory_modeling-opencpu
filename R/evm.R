add_model <- function(df, models, outcome_name, residuals = TRUE) {
  
    # settings
    n_draws <- 5
    # model check fuctions
    normal_model_check <- function(mu_spec, sigma_spec = "~1", data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
        model_name <- sym(paste("normal", mu_spec, sigma_spec, sep = "| "))
        
        # fit model
        mu_spec <- as.formula(mu_spec)
        sigma_spec <- as.formula(sigma_spec)
        model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data)))
        
        # get summary statistics describing model predictions
        pred.mu <- predict(model, se.fit = TRUE, type = "response")
        pred.sigma <- predict(model, what = "sigma", se.fit = TRUE)
        output <- data %>%
            mutate(
                mu.expectation = pred.mu$fit,                       # add fitted mu and its standard error to dataframe
                mu.se = pred.mu$se.fit,
                logsigma.expectation = pred.sigma$fit,              # add fitted logsigma and its standard error to dataframe 
                logsigma.se = pred.sigma$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                             # generate list of draw numbers
                t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into means
                t2 = map(df, ~rt(n_draws, .))                       # simulate draws from t distribution to transform into log sigma
            ) %>%
            unnest(cols = c("draw", "t1", "t2")) %>%
            mutate(
                mu = t1 * mu.se + mu.expectation,                   # scale and shift t to get a sampling distribution of means
                logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
                sigma = exp(logsigma)                               # backtransform to sampling distribution of sigma parameter
            ) %>%
            rowwise() %>%
            mutate(
                # compute predictive distribution
                prediction = rnorm(1, mu, sigma)
            ) %>%
            rename(
                data = !!outcome_name,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            ) %>%
            dplyr::select(-one_of("mu.expectation", "mu.se", "logsigma.expectation", "logsigma.se", "df", "t1", "t2", "mu", "logsigma", "sigma"))
    }
    lognormal_model_check <- function(mu_spec, sigma_spec = "~1", data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
        model_name <- sym(paste("lognormal", mu_spec, sigma_spec, sep = "| "))
        
        # compute log transform of outcome variable and add to dataframe
        data <- data %>%
            mutate(
                "log_{{outcome_name}}" := if_else({{outcome_name}}==0.0,
                                     log(0.001), # avoid -inf errors by fudging the zeros a bit
                                     log({{outcome_name}})
                )
            )
        # replace {{outcome_name}} with log_{{outcome_name}}
        mu_spec <- str_replace_all(mu_spec, as.character(outcome_name), paste("log", outcome_name, sep = "_"))
        
        # fit model
        mu_spec <- as.formula(mu_spec)
        sigma_spec <- as.formula(sigma_spec)
        model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data)))
        
        # get summary statistics describing model predictions
        pred.mu <- predict(model, se.fit = TRUE, type = "response")
        pred.sigma <- predict(model, what = "sigma", se.fit = TRUE)
        output <- data %>%
            mutate(
                mu.expectation = pred.mu$fit,                       # add fitted mu and its standard error to dataframe
                mu.se = pred.mu$se.fit,
                logsigma.expectation = pred.sigma$fit,              # add fitted logsigma and its standard error to dataframe 
                logsigma.se = pred.sigma$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                             # generate list of draw numbers
                t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into means
                t2 = map(df, ~rt(n_draws, .))                       # simulate draws from t distribution to transform into log sigma
            ) %>%
            unnest(cols = c("draw", "t1", "t2")) %>%
            mutate(
                mu = t1 * mu.se + mu.expectation,                   # scale and shift t to get a sampling distribution of means
                logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
                sigma = exp(logsigma)                               # backtransform to sampling distribution of sigma parameter
            ) %>%
            rowwise() %>%
            mutate(
                # compute predictive distribution in backtransformed units
                prediction = rlnorm(1, mu, sigma)
            ) %>%
            rename(
                data = !!outcome_name,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            ) %>%
            dplyr::select(-one_of("mu.expectation", "mu.se", "logsigma.expectation", "logsigma.se", "df", "t1", "t2", "mu", "logsigma", "sigma", paste("log", outcome_name, sep = "_")))
    }
    logitnormal_model_check <- function(mu_spec, sigma_spec = "~1", data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
        model_name <- sym(paste("logitnormal", mu_spec, sigma_spec, sep = "| "))
        
        # compute log transform of outcome variable and add to dataframe
        data <- data %>%
            mutate(
                "logit_{{outcome_name}}" := if_else({{outcome_name}}==0.0,
                                                  qlogis(0.001), # avoid -inf errors by fudging the zeros a bit
                                                  if_else({{outcome_name}}==1.0,
                                                          qlogis(0.999), # avoid inf errors by fudging the ones a bit
                                                          qlogis({{outcome_name}})
                                                  ),
                                                  
                )
            )
        # replace {{outcome_name}} with log_{{outcome_name}}
        mu_spec <- str_replace_all(mu_spec, as.character(outcome_name), paste("logit", outcome_name, sep = "_"))
        
        # fit model
        mu_spec <- as.formula(mu_spec)
        sigma_spec <- as.formula(sigma_spec)
        model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data)))
        
        # get summary statistics describing model predictions
        pred.mu <- predict(model, se.fit = TRUE, type = "response")
        pred.sigma <- predict(model, what = "sigma", se.fit = TRUE)
        output <- data %>%
            mutate(
                mu.expectation = pred.mu$fit,                       # add fitted mu and its standard error to dataframe
                mu.se = pred.mu$se.fit,
                logsigma.expectation = pred.sigma$fit,              # add fitted logsigma and its standard error to dataframe 
                logsigma.se = pred.sigma$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                             # generate list of draw numbers
                t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into means
                t2 = map(df, ~rt(n_draws, .))                       # simulate draws from t distribution to transform into log sigma
            ) %>%
            unnest(cols = c("draw", "t1", "t2")) %>%
            mutate(
                mu = t1 * mu.se + mu.expectation,                   # scale and shift t to get a sampling distribution of means
                logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
                sigma = exp(logsigma)                               # backtransform to sampling distribution of sigma parameter
            ) %>%
            rowwise() %>%
            mutate(
                # compute predictive distribution in backtransformed units
                prediction = plogis(rnorm(1, mu, sigma))
            ) %>%
            rename(
                data = !!outcome_name,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            ) %>%
            dplyr::select(-one_of("mu.expectation", "mu.se", "logsigma.expectation", "logsigma.se", "df", "t1", "t2", "mu", "logsigma", "sigma", paste("logit", outcome_name, sep = "_")))
    }
    logistic_model_check <- function(mu_spec, data) {
        
        # get outcome variable and model names
        outcome_name <- sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE))
        dummy_outcome <- sym(paste("dummy", outcome_name, sep = "_"))
        outcome_name <- sym(outcome_name)
        model_name <- sym(paste("logistic", mu_spec, sep = "| "))
        
        # make sure that outcome variable is coded as binary:
        # create lookups
        outcome_values <- data %>% 
          dplyr::select(outcome_name) %>% 
          distinct() %>% 
          arrange() %>% 
          as.vector()
        outcome_values <- outcome_values[[1]]
        dummy_values <- 0:(length(outcome_values) - 1)
        names(dummy_values) = outcome_values
        reverse_dummy_values <- names(dummy_values)
        names(reverse_dummy_values) = unname(dummy_values)
        # create dummy variable and add to model spec
        data <- data %>% 
          mutate(
            !!dummy_outcome := unname(dummy_values[!!outcome_name])
          )
        mu_spec <- gsub(as.character(outcome_name), as.character(dummy_outcome), mu_spec)
        
        # fit model
        mu_spec <- as.formula(mu_spec)
        model <- eval(bquote(gamlss(.(mu_spec), data = data, family = BI)))
        
        # get summary statistics describing model predictions
        pred <- predict(model, se.fit = TRUE)
        output <- data %>%
            mutate(
                logitmu.expectation = pred$fit,                     # add fitted logmu and standard errors to dataframe
                logitmu.se = pred$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                            # generate list of draw numbers
                t = map(df, ~rt(n_draws, .))                        # simulate draws from t distribution to transform into logit mu
            ) %>%
            unnest(cols = c("draw", "t")) %>%
            mutate(
                logitmu = t * logitmu.se + logitmu.expectation,     # scale and shift t to get a sampling distribution of logit mu
                mu = plogis(logitmu)                                # backtransform to sampling distribution of mean probability
            ) %>%
            rowwise() %>%
            mutate(
                prediction = rbinom(1, 1, mu)                       # compute predictive distribution of binary outcomes
            ) %>%
            rename(
                data = !!dummy_outcome,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(dummy_outcome)
            ) %>%
            mutate(
              !!outcome_name := unname(reverse_dummy_values[as.character(!!dummy_outcome)])
            ) %>%
            dplyr::select(-one_of("logitmu.expectation", "logitmu.se", "df", "t", "logitmu", "mu", as.character(dummy_outcome)))
    }
    poisson_model_check <- function(mu_spec, data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
        model_name <- sym(paste("poisson", mu_spec, sep = "| "))
        
        # fit model
        mu_spec <- as.formula(mu_spec)
        model <- eval(bquote(gamlss(.(mu_spec), data = data, family = PO)))
        
        # get summary statistics describing model predictions
        pred <- predict(model, se.fit = TRUE)
        output <- data %>%
            mutate(
                logmu.expectation = pred$fit,                       # add fitted predictions and standard errors to dataframe
                logmu.se = pred$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                            # generate list of draw numbers
                t = map(df, ~rt(n_draws, .))                        # simulate draws from t distribution to transform into log mean rates
            ) %>%
            unnest(cols = c("draw", "t")) %>%
            mutate(
                logmu = t * logmu.se + logmu.expectation,  # scale and shift t to get a sampling distribution of log mean rates
                mu = exp(logmu)                                     # backtransform to sampling distribution of mean rate parameter
            ) %>%
            rowwise() %>%
            mutate(
                prediction = rpois(1, mu)                           # compute predictive distribution of counts
            ) %>%
            rename(
                data = !!outcome_name,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            ) %>%
            dplyr::select(-one_of("logmu.expectation", "logmu.se", "df", "t", "logmu", "mu"))
    }
    negbinomial_model_check <- function(mu_spec, sigma_spec = "~1", data) {
        
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
                logsigma.se = pred.sigma$se.fit,
                df = df.residual(model)                             # get degrees of freedom
            )
        
        # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
        output <- output %>%
            mutate(
                draw = list(1:n_draws),                             # generate list of draw numbers
                t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into log mu
                t2 = map(df, ~rt(n_draws, .))                       # simulate draws from t distribution to transform into log sigma
            ) %>%
            unnest(cols = c("draw", "t1", "t2")) %>%
            mutate(
                logmu = t1 * logmu.se + logmu.expectation,          # scale and shift t to get a sampling distribution of log mu
                logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
                mu = exp(logmu),                                    # backtransform to sampling distributions of mu and sigma parameters
                sigma = exp(logsigma)
            ) %>%
            rowwise() %>%
            mutate(
                # compute predictive distribution of counts
                prediction = rNBII(1, mu, sigma)
            ) %>%
            rename(
                data = !!outcome_name,
                !!model_name := prediction
            ) %>%
            pivot_longer(
                cols = c("data", model_name),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            ) %>%
            dplyr::select(-one_of("logmu.expectation", "logmu.se", "logsigma.expectation", "logsigma.se", "df", "t1", "t2", "logmu", "mu", "logsigma", "sigma"))
    }
    multinomial_model_check <- function(spec, data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", spec, fixed = TRUE)))
        model_name <- sym(paste("multinomial", spec, sep = "| "))
        
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
            add_predicted_draws(model, seed = 14, n = n_draws) %>%
            dplyr::select(-one_of(c(".row", ".chain", ".iteration"))) %>%
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
    }
    ordinal_model_check <- function(mu_spec, disp_spec = "~1", data) {
        
        # get outcome variable and model names
        outcome_name <- sym(sub("\\~.*", "", gsub(" ", "", mu_spec, fixed = TRUE)))
        model_name <- sym(paste("ordinal", mu_spec, disp_spec, sep = "| "))
        
        disp_spec <- paste0("disc", disp_spec)
        
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
            dplyr::select(-one_of(c(".row", ".chain", ".iteration"))) %>%
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
    }
    # helper fuctions
    merge_modelchecks <- function(df_old, df_new) {
        
        # drop redundant rows from df_new
        df_new <- df_new %>% filter(modelcheck_group != "data")
        
        # bind together dataframes assuming they have identical column names 
        output <- rbind(df_old, df_new)
        
        # order by draw and modelcheck_group
        output <- output[order(output$modelcheck_group, output$draw),]
    }
    merge_modelchecks_array <- function(dfs) {
        
        # iterate through list of dataframes
        output <- dfs[[1]] # start output file with first dataframe
        dfs[- 1]           # remove first dataframe from initial list to avoid duplicating it
        lapply(dfs, function(df) {
            # drop redundant rows from df since we've already set aside the original data from the first df
            df <- df %>% filter(modelcheck_group != "data")
            
            # bind together dataframes assuming they have identical column names 
            output <- rbind(output, df)
        })
        
        # order by draw and modelcheck_group
        output <- output[order(output$modelcheck_group, output$draw),]
    }
    calc_residuals <- function(df, outcome_name, models) {
        
        # outcome name as symbol
        outcome_name <- sym(outcome_name)
        
        # get unique model names (for unnesting df_wide below)
        model_names_vect <- c("data", unique(models$name))
        
        # filter out rows where residuals have already been calculated
        df <- df %>% filter(!grepl("^res\\|.", modelcheck_group))
        
        # put data in wide format
        df_wide <- df %>%
          pivot_wider(names_from = modelcheck_group, values_from = !!outcome_name, values_fn = list) %>%
          unnest(cols = all_of(model_names_vect)) # needed to avoid nested lists of duplicates
        
        # get list of models whose predictions are included in this dataframe
        model_names <- setdiff(names(df_wide),c(names(df), "data"))
        
        # model and residual names as symbols
        residual_names <- lapply(model_names, function (model) { sym(paste("res", model, sep = "| ")) })
        model_names <- lapply(model_names, sym)
        
        # iterate through models
        for (i in 1:length(model_names)) {
            # index for current model
            residual_name <- residual_names[[i]]
            model_name <- model_names[[i]]
            
            # calculate residual
            if (startsWith(as.character(model_name), "logistic")) {
              # create lookup to transform outcome units
              outcome_values <- df %>% 
                dplyr::select(outcome_name) %>% 
                distinct() %>% 
                arrange() %>% 
                as.vector()
              outcome_values <- outcome_values[[1]]
              dummy_values <- 0:(length(outcome_values) - 1)
              names(dummy_values) = outcome_values
              
              df_wide <- df_wide %>%
                mutate(
                  !!residual_name := as.character(unname(dummy_values[data]) - unname(dummy_values[!!model_name]))
                )
            } else {
              df_wide <- df_wide %>%
                mutate(
                  !!residual_name := data - !!model_name
                )
            }
            
        }
        
        # put data back into long format for output
        output <- df_wide %>%
            pivot_longer(
                cols = c("data", sapply(model_names, as.character), sapply(residual_names, as.character)),
                names_to = "modelcheck_group",
                values_to = as.character(outcome_name)
            )
    }

    # read data from json
    df <- fromJSON(df, simplifyVector = TRUE)
    # # dev
    # df <- read_json("../opencpu/R/input.json", simplifyVector = TRUE)
    # # dev
    # # test
    # df <- read_json("../opencpu/R/testdata.json", simplifyVector = TRUE)
    # df <- read_json("input-fires.json", simplifyVector = TRUE)
    # residuals = TRUE
    # # test

    # isolate data from model predictions for modeling
    if ("modelcheck_group" %in% colnames(df)) {
        # drop model outputs from copy of data to use for modeling
        data <- df %>% 
            filter(modelcheck_group == "data", draw == 1) %>%
            dplyr::select(-one_of(c("modelcheck_group", "draw")))
        # Check input df to see which model predictions and residuals are already there
        cur_models <- unique(df$modelcheck_group)
        # filter out residual names from list of models already in input dataframe
        res_idx <- which(grepl("^res\\|.", cur_models))
        cur_models = cur_models[-res_idx]
    } else {
        data <- df
        # Create empty set of models.
        cur_models <- c("data")
    }

    #  Filter previously run models from set of models to run
    all_models <- fromJSON(models, simplifyVector = TRUE)
    # # dev
    # # we create this object in the web app
    # all_models <- tibble(
    #     name = c("normal| mpg ~ 1| ~1", "normal| mpg ~ cyl| ~cyl"),
    #     family = c("normal", "normal"),
    #     mu_spec = c("mpg ~ 1", "mpg ~ cyl"),
    #     sigma_spec = c("~1", "~cyl")
    # )
    # # dev
    # # test
    # all_models <- read_json("../opencpu/R/testmodels.json", simplifyVector = TRUE)
    # all_models <- tibble(
    #   name = c("poisson| relative_humidity ~ temperature", "negbinomial| relative_humidity ~ temperature| ~1"),
    #   family = c("poisson", "negbinomial"),
    #   mu_spec = c("relative_humidity ~ temperature", "relative_humidity ~ temperature"),
    #   sigma_spec = c(NA, "~1")
    # )
    # outcome_name <- "relative_humidity"
    # # test
    models <- all_models %>% filter(!name %in% cur_models)

    #  Call a list of model check functions for remaining models
    new_models <- list()
    for(i in 1:nrow(models)) {
        # run modelcheck depending on family
        if(models[[i, "family"]] == "normal") {
            new_models[[i]] <- normal_model_check(models[[i, "mu_spec"]], models[[i, "sigma_spec"]], data) 
        } else if(models[[i, "family"]] == "lognormal") {
            new_models[[i]] <- lognormal_model_check(models[[i, "mu_spec"]], models[[i, "sigma_spec"]], data)
        } else if(models[[i, "family"]] == "logitnormal") {
            new_models[[i]] <- logitnormal_model_check(models[[i, "mu_spec"]], models[[i, "sigma_spec"]], data)
        } else if(models[[i, "family"]] == "logistic") {
            new_models[[i]] <- logistic_model_check(models[[i, "mu_spec"]], data)
        } else if(models[[i, "family"]] == "poisson") {
            new_models[[i]] <- poisson_model_check(models[[i, "mu_spec"]], data)
        } else if(models[[i, "family"]] == "negbinomial") {
            new_models[[i]] <- negbinomial_model_check(models[[i, "mu_spec"]], models[[i, "sigma_spec"]], data) 
        } else if(models[[i, "family"]] == "multinormial") {
            new_models[[i]] <- multinomial_model_check(models[[i, "mu_spec"]], data)
        } else if(models[[i, "family"]] == "ordinal") {
            new_models[[i]] <- ordinal_model_check(models[[i, "mu_spec"]], models[[i, "sigma_spec"]], data)
        } 
    }

    #  Merge model check results together
    new_merged <- merge_modelchecks_array(new_models)
    if ("modelcheck_group" %in% colnames(df)) {
        output <- merge_modelchecks(df, new_merged)
    } else {
        output <- new_merged
    }

    #  Calculate residuals if they aren't already in the output
    if(residuals) {
        output <- calc_residuals(output, outcome_name, all_models)
    }

    return(list(message = "success", data = toJSON(output)))
}