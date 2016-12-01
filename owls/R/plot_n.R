#'plot_n
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df
#'@param r is the r values specified in the parameters 
#'
#'@author Elizabeth Hiroyasu
#'
#'



#subset function doesn't work...

plot_n<-function(df, r){
  ggplot(data=subset(df, r==0.01), aes(x=time, y=N, group=as.factor(P), color=as.factor(P))) + 
    geom_line(size=1.5)+ ggtitle(paste("r=", r)) + scale_colour_discrete(name="Predator Density") + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1))
}
