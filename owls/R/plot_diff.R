#'Plotting change in populations
#'
#'This function can be used to plot the total percent change in populations over time. The x-axis should be predator 
#'and the y axis is percent change in population of the prey species. 
#'
#'@param state is the state variables N for the prey population and P for the predator population; 
#'measured in individuals per ha. For this function, we are usually interested in the response of the prey species to
#'varying predator densities.
#'@param diff is a matrix of differences by varying state variables; it is the output from the percent_diff function
#'
#'@author Elizabeth Hiroyasu
#'

plot_diff<-function(state, diff){
  plot(state, diff[1, 2:dim(diff)[2]], type='l', lwd=2, col=1, ylab="Percent Change in Prey", xlab="Predator Density per Ha", 
       main= "Percent change in Prey by Predator Density")
  for (i in 2:(dim(diff)[1])){
    lines(state, diff[i, 2:dim(diff)[2]], col=i, lwd=2)
  }
  
  legend("topright", legend=c(diff[,1]), col=c(1:(dim(diff)[1])), lty=1, cex=0.55)
  
}


