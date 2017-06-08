D#'plot_eq
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is a dataframe that contains variables P, r, D, K_prey, N, and Eq. Eq is the equilibrium N1 or N2 variable
#'@param P is a vector of predator densities, this is used in the title of the plots
#'@param i is an interator to print the title
#'
#'@author Elizabeth Hiroyasu
#'
#'

plot_eq<-function(df, P, i){
  ggplot()+
    geom_line(data=df, aes(D, Eq, linetype=(N), color=(K_prey)),size=1.1)+
    labs(title=paste("Equilibrium prey density, Predator Density=", P[i]), x="D (Individuals/ha)",
         y="Equilibrium Prey Density (Individuals/ha")+
    theme(plot.title=element_text(size=12))+
    scale_linetype_discrete(name="Equilibria")+
    scale_colour_discrete(name="Prey Carrying \nCapacity")+
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1), 
          legend.direction="vertical") +
    facet_grid(.~r)
  
}

