#'Plotting simulations
#'
#'This function can be used to plot the predator and prey populations over time for all simulation runs.
#'
#'@param times is the time vector to be used for the x axis, this should be the same as the t parameter in the pred_prey function
#'@param sim is the list of simulations
#'@param s is a vector of r values, usually this is a sample of r values
#'
#'@author Elizabeth Hiroyasu
#'

matplot_sim<-function(times, sim, s, r){
  matplot(times, sim[[s]][,,1][,-1], type='l', lty=c(1,2), col=1, lwd=2, xlab="Season", ylab="Population density individuals/ha", 
          main=paste("Population Density by Season, r=", r[s], "with other Control"))
  for (i in 2:(dim(sim[[s]])[3])){
    matlines(times, sim[[s]][,,i][,-1], type='l', lty=c(1,2), col=i, lwd=2)
  }
  legend(0, 55, legend=P, col=c(1:(dim(sim[[s]])[3])), lty=1, lwd=2, title="Predator Density")
}



