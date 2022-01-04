merge_modelchecks <- function(df_old, df_new) {

    # read data from json
    df_old <- fromJSON(df_old, simplifyVector = TRUE)
    df_new <- fromJSON(df_new, simplifyVector = TRUE)

    # drop redundant rows from df_new
    df_new <- df_new %>% filter(modelcheck_group != "data")

    # bind together dataframes assuming they have identical column names 
    output <- rbind(df_old, df_new)

    # order by draw and modelcheck_group
    output <- output[order(output$modelcheck_group, output$draw),]

    return(list(message = "success", data = toJSON(output)))
}