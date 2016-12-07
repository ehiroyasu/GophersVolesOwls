#'plot_n
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param foo is the dataframe to feed into ggplot2, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value.
#'
#'@author Elizabeth Hiroyasu
#'
#'


plot_n<-function(df, r){
  
  ggplot(data=df, aes(x=time, y=N, group=as.factor(P), color=as.factor(P))) + 
    geom_line(size=1.1)+ ggtitle(paste("r=", r)) + theme(plot.title=element_text(size=12)) + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1),  
          legend.direction="vertical")+scale_colour_discrete(name="Predator Density")  
}


