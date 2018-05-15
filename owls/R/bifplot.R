#'bifplot
#'
#'Uses ggplot2 package to plot the bifurcation plot of N* vs D.
#'
#'@param P_split is a list where each element is the dataframe for a different predator density.
#'The dataframe in each element contains a column for predator density, r, K_prey, equilibrium N, and a 
#'factor specifying N1 or N2 for equilibrium N.
#'@param K_prey is the carrying capacity of the prey population.
#'@param i is to specify the list element number to subset the P_split list by predator density.
#'
#'@author Elizabeth Hiroyasu
#'
#'
#'


bifplot<-function(P_split, i){
  SubP<-P_split[[i]]
  
  ggplot(SubP, aes(x=D, y=eq, group=N, color=N)) + 
    geom_point(size=1) + 
    ggtitle(paste("Bifurcation Plot of N* vs D, P=", SubP$P[1]))#+
    #facet_grid(r~K_prey, labeller=label_both)
}