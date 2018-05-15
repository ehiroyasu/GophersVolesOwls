#'plot_n
#'
#'Uses ggplot2 package to plot the output data from predprey_sim. Data must be processed into a dataframe using the lsplit
#'and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value.
#'@param N0 is a vector of initial prey density values, in this function it is used in the title to indicate the starting value plotted
#'@param K_prey is the carrying capacity of the prey population.
#'@param alphaP is the attack rate of the predator
#'
#'@author Elizabeth Hiroyasu
#'
#'


plot_n<-function(df, r, N0, K_prey, alphaP){

  ggplot(data=df, aes(x=time, y=N, group=as.factor(P), color=as.factor(P))) +
    geom_line(size=1)+
    scale_colour_discrete(name="Predator Density")+
    ggtitle(bquote(paste("K = ",.(K_prey),",", alpha, " = ",.(alphaP))))+
    labs(x="Time (Seasons)", y="Prey Density (individuals/ha)")+
    theme(plot.title=element_text(size=12, family="sans"), 
          panel.background = element_rect(colour = "black", fill=NA, size=0.5, linetype=1),
          legend.background=element_rect(fill="gray90", size=0.5, colour=1),
          legend.direction="vertical")+ 
    geom_hline(yintercept=K_prey, linetype="dashed", size=1.0) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, K_prey+10))+
    facet_grid(N0~r, labeller=label_bquote(rows=N[0]:~.(N0), cols=r:~.(r)))
  
  }


