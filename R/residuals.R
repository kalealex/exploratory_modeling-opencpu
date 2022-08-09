calc_residuals <- function(df, outcome_name) {

    # read data from json
    df <- fromJSON(df, simplifyVector = TRUE)

    # outcome name as symbol
    outcome_name <- sym(outcome_name)

    # filter out rows where residuals have already been calculated
    df <- df %>% filter(!grepl("^res\\|.", modelcheck_group))

    # put data in wide format
    df_wide <- df %>% pivot_wider(names_from = modelcheck_group, values_from = !!outcome_name)
    
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
        df_wide <- df_wide %>%
            mutate(
                !!residual_name := data - !!model_name
            )
    }

    # put data back into long format for output
    output <- df_wide %>%
        pivot_longer(
        cols = c("data", sapply(model_names, as.character), sapply(residual_names, as.character)),
        names_to = "modelcheck_group",
        values_to = as.character(outcome_name)
    )

    return(list(message = "success", data = toJSON(output)))
}