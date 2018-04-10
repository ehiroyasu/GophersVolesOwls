#'N sim function
#'
#'Creates a list of the simulation output by different initial N values. Each element is a different initial N value.
#' 
#'@param sim is the output list from the function predprey_sim
#'@param r is a vector of population growth rates
#'@param N is a vector of initial prey population densities
#'@param P is a vector of prey densities
#'
#'
#'@author Elizabeth Hiroyasu
#'

n.sim<-function(sim, N, P, r){
  bar<-vector("list", length=length(unique(N)))
  n.split<-1:length(N)
  n.split<-split(n.split, rep(1:length(unique(N)), each=length(unique(P))))
  
  for(i in 1:length(unique(N))){
    bar[[i]]<-lsplit(sim, n.split[[i]], r)
  }
  
  names(bar)<-unique(N)
  
  return(bar)
}
