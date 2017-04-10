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
  n1.sim<-lsplit(sim, n=1:5, r)
  n2.sim<-lsplit(sim, n=6:10, r)
  n3.sim<-lsplit(sim, n=11:15, r)
  n4.sim<-lsplit(sim, n=16:20, r)
  n5.sim<-lsplit(sim, n=21:25, r)
  n6.sim<-lsplit(sim, n=26:30, r)
  
  return(list(n1.sim, n2.sim, n3.sim, n4.sim, n5.sim, n6.sim))
}