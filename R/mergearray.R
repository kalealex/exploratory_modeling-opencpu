merge_modelchecks_array <- function(dfs) {

    # read data from json
    dfs <- fromJSON(dfs, simplifyVector = TRUE)

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

    return(list(message = "success", data = toJSON(output)))
}