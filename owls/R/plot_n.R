#'plot_n
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the output from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value, it must be specified 
#'in both the r param and the ssubset parameter.
#'@param ssubset is a character expression (requires nested quotes) to specify the r value that the data is subsetted by.
#'
#'@author Elizabeth Hiroyasu
#'
#'


plot_n<-function(df, ssubset, r){
  ggplot(data=subset(df, eval(parse(text=ssubset))), aes(x=time, y=N, group=as.factor(P), color=as.factor(P))) + 
    geom_line(size=1.5)+ ggtitle(paste("r=", r)) + theme(plot.title=element_text(size=12)) + scale_colour_discrete(name="Predator Density") + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1))
}
