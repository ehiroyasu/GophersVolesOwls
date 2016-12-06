#'mysubset
#'
#'Subsets the dataframe from the df_sim function in the owls package. This allows for easier plotting in the plot_n function.
#'
#'@param df is the data frame from the df_sim function in the owls package. This further subsets the data frame for plotting purposes.
#'
#'@author Elizabeth Hiroyasu
#'

mysubset <- function(df, ...) {
  ssubset <- deparse(substitute(...))
  subset(df, eval(parse(text = ssubset)))
}