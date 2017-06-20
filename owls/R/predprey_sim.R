#'Predator Prey Simulation Function
#'
#'A function to simulate the pred_prey function in the owl package for multiple runs. This returns a list of 3D arrays with a
#'different element for each r-value.  The value of the r-value is the name of each element of the list.
#'Each element contains a simulation for varying state variables (usually Predator density).
#'
#'@param times is a numerical sequence of time steps, this should be the same as the t parameter in the pred_prey function
#'@param state is a matrix of starting values for N and P
#'@param r is a vector of r-values used in the simulation
#'
#'@author Elizabeth Hiroyasu
#'




predprey_sim<-function(times, state, r){
  sim<-rep(list(array(data=NA, dim=c(length(times), 3, dim(state)[1]))), length(r))

  for(i in 1:(dim(state))[1]){
    for (k in 1:(length(r))){
      sim[[k]][,,i]<- as.matrix(ode(y=state[i,], times = times, func = pred_prey, 
                                    parms = c(r = r[k], alpha, beta, delta, K_prey, k_max, D)))
      colnames(sim[[k]])<-c("time", "N", "P")
      }
  }
  
  sim<-setNames(sim, as.character(r))
  
  return(sim)
}
