D#'plot_eq
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the N_eq function in the 'owls' package.
#'
#'@author Elizabeth Hiroyasu
#'
#'

plot_eq<-function(N1, N2, P, i){
  ggplot(mapping=aes(D, Eq, group=as.factor(K_prey), color=as.factor(K_prey)))+
    geom_line(data=N1, size=1.1, linetype=1)+
    geom_line(data=N2, size=1.1, linetype=2)+
    labs(title=paste("Equilibrium prey density, Predator Density=", P[i]), x="D (Individuals/ha)",
         y="Equilibrium Prey Density (Individuals/ha")+
    theme(plot.title=element_text(size=12))+
    #scale_linetype_identity(name="Equilibria", guide=guide_legend(override.aes = list(linetype=c(1,2))),
    #                        labels=c("N1", "N2"), breaks=c("solid","dashed"))+
    scale_colour_discrete(name="Prey Carrying Capacity")+
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1), 
          legend.direction="vertical") +
    facet_grid(.~r)
}
