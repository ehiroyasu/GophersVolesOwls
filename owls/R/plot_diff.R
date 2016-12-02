#'Plotting change in populations
#'
#'This function can be used to plot the total percent change in populations over time. The x-axis should be predator density 
#'and the y axis is percent change in population of the prey species. 
#'
#'@param diff is a matrix of differences by varying state variables; it is the output from the percent_diff function
#'@param N is the vector of input N values
#'@param j is a number calling which N value is being expressed
#'
#'@author Elizabeth Hiroyasu
#'


plot_diff<-function(diff, N, j){
  diff<-melt(diff)
  diff<-rename(diff, c("variable"="P", "value"="diff"))
  ggplot(diff, aes(x=P, y=diff, group=r, color=r)) + geom_line(size=1.1) + 
    ggtitle(paste("N=", unique(N)[j])) + 
    theme(legend.background=element_rect(fill="gray90", size=0.5, linetype="solid", colour=1))+ 
    ylab("Percent Difference") + xlab("Predator Density")
  
}
