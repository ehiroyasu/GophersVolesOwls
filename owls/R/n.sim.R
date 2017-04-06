#'N sim function
#'
#'Creates a list of the simulation output by different initial N values. Each element is a different initial N value.
#' 
#'@param sim is the output list from the function predprey_sim
#'@param r is a vector of population growth rates
#'
#'@author Elizabeth Hiroyasu
#'

n.sim<-function(sim, r){
  n175.sim<-lsplit(sim, n=1:5, r)
  n100.sim<-lsplit(sim, n=6:10, r)
  n50.sim<-lsplit(sim, n=11:15, r)
  n30.sim<-lsplit(sim, n=16:20, r)
  n10.sim<-lsplit(sim, n=21:25, r)
  n2.sim<-lsplit(sim, n=26:30, r)
  
  return(list(n175.sim, n100.sim, n50.sim, n30.sim, n10.sim, n2.sim))
}