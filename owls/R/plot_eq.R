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
  ggplot(data=df, aes(x=K_prey, y=Eq, group=as.factor(P), color=as.factor(P))) +
    geom_line(size=1.1)+
    labs(title="Equilibrium prey density", x="Prey Carrying Capacity (Individuals/ha)",
                             y="Equilibrium Prey Density (Individuals/ha")+
    theme(plot.title=element_text(size=12))+
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1), 
          legend.direction="vertical") + 
    scale_colour_discrete(name="Predator Density")+
    facet_grid(.~r)
}
