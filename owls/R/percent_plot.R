#'Plot percent difference
#'
#'Plots the percent change in prey by different levels of predator density
#' 
#'@param sim is the output list from the function n.sim (which is modified from the predprey_sim output) 
#'@param r is a vector of population growth rates
#'
#'@author Elizabeth Hiroyasu
#'


percent_plot<-function(sim, r){
  diff.n.init<-NULL
  for(i in 1:length(sim)){
    diff.n.init[[i]]<-percent_diff(sim[[i]], r)
  }

  p<-NULL 
  for(i in 1:length(diff.n.init)){
    p[[i]]<-plot_diff(diff=diff.n.init[[i]], N=N, j=i)
  }

  legend<-get_legend(p[[1]])
  
  grid.arrange(grobs=list(p[[1]]+theme(legend.position="none", axis.title.y=element_blank()), 
                          p[[2]]+theme(legend.position="none", axis.title.y=element_blank()), 
                          p[[3]]+theme(legend.position="none", axis.title.y=element_blank()), 
                          p[[4]]+theme(legend.position="none", axis.title.y=element_blank()),
                          p[[5]]+theme(legend.position="none", axis.title.y=element_blank()),
                          p[[6]]+theme(legend.position="none", axis.title.y=element_blank()),
                          legend), 
               nrow=4, top=textGrob("Percent Change by Predator Density", gp=gpar(fontface="bold", fontsize=6, cex=3)),
               left=textGrob("Percent Difference", rot=90, vjust=1),
               layout_matrix = rbind(c(1,2), c(3,4), c(5,6), c(7,7)), heights = c(2, 2, 2, 0.3),
               clip=FALSE)
}



