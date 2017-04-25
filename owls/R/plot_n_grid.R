#'plot_n_grid
#'
#'Uses the gridExtra package to plot the output plots from the plot_n function on one grid.
#'
#'@param df is the dataframe to feed into the plot_n function, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value.
#'@param N is the vector of initial N values set in the state variables.
#'@param K_prey is the carrying capacity of the population
#'@param i is the value of N to call to specify the title.
#'
#'@author Elizabeth Hiroyasu
#'
#'

plot_n_grid<-function(df, r, N, K_prey, i){
  p1<-plot_n(df[[1]], r[1])
  p2<-plot_n(df[[2]], r[2])
  p3<-plot_n(df[[3]], r[3])
  
  
  legend <- get_legend(p1)
  
  grid.arrange(grobs=list(p1+theme(legend.position="none"), 
                          p2+theme(legend.position="none"), 
                          p3+theme(legend.position="none"),
                          legend), nrow=2, 
               top=textGrob(paste("Prey Density Over Time, N=", unique(N)[i], "and K=", K_prey), gp=gpar(fontface="bold", fontsize=6, cex=3)))
  
}



