#'plot_n
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value.
#'@param N is a vector of initial prey density values, in this function it is used in the title to indicate the starting value plotted
#'@param K_prey is the carrying capacity of the prey population.
#'@param i is the iterator used to cycle through the initial N values to be pasted into the title.
#'
#'@author Elizabeth Hiroyasu
#'
#'


plot_n<-function(df, r, N, K_prey, i){

  ggplot(data=df, aes(x=time, y=N, group=as.factor(P), color=as.factor(P))) + 
    geom_line(size=1.1)+ ggtitle(paste("N=", unique(N)[i], ", K=", K_prey)) + theme(plot.title=element_text(size=12)) + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1),  
          legend.direction="vertical")+ scale_colour_discrete(name="Predator Density") +
    facet_grid(.~r, labeller=label_both) + geom_hline(yintercept=K_prey, linetype="dashed") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, K_prey+5))
                       
  }


