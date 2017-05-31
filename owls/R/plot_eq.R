#'plot_eq
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the N_eq function in the 'owls' package.
#'
#'@author Elizabeth Hiroyasu
#'
#'

plot_eq<-function(df){
  ggplot(data=df, aes(x=D, y=Eq, group=as.factor(K_prey), color=as.factor(K_prey))) +
    geom_line(size=1.1)+
    labs(title=paste("Equilibrium prey density, Predator Density=", P), x="D (Individuals/ha)",
                             y="Equilibrium Prey Density (Individuals/ha")+
    theme(plot.title=element_text(size=12))+
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1), 
          legend.direction="vertical") + 
    scale_colour_discrete(name="Prey Carrying Capacity")+
    facet_grid(.~r)
}
