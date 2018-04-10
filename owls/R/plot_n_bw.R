#'plot_n_bw
#'
#'Uses ggplot2 package to plot the output data from predprey_sim in black and white. 
#'Data must be processed into a dataframe using the lsplit and df_sim functions.
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param r is the r values specified in the parameters, because this function subsets by r value.
#'@param N is a vector of initial prey density values, in this function it is used in the title to indicate the starting value plotted
#'@param K_prey is the carrying capacity of the prey population.
#'@param alpha is the attack rate of the predator
#'@param i is the iterator used to cycle through the initial N values to be pasted into the title.
#'
#'@author Elizabeth Hiroyasu
#'
#'


plot_n_bw<-function(df, r, N, K_prey, alpha, i, j){

  ggplot(data=df, aes(x=time, y=N, group=as.factor(P), linetype=as.factor(P))) + 
    geom_line(size=1.3) +theme_bw()+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    scale_linetype_manual(values=c(1,3,4), name="Predator Density")+
    ggtitle(paste("N=", unique(N)[i], ", K=", K_prey, ", alpha = ", alpha)) + theme(plot.title=element_text(size=12, family="sans")) + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, colour=1),legend.direction="vertical")+ 
    scale_colour_grey(end=0.7, guide=FALSE) +
    facet_grid(.~r, labeller=label_both) + 
    geom_hline(yintercept=K_prey, linetype="dashed", size=1.0) +
    stat_ecdf(n = 500) + scale_y_continuous(expand = c(0, 0), limits = c(0, K_prey+10))
}


