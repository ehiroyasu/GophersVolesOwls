#'List split
#'
#'Splits the output list from the predprey_sim function into a list of outputs based on the initial population size 
#'of the prey population
#'
#'@param sim is the list output of the simulation result from the predprey_sim function
#'@param n is a sequence identifying the matrices to be extracted from the sim data
#'@param r is the r values specified in the parameters 
#'
#'@author Elizabeth Hiroyasu
#'



lsplit<-function(sim, n, r){
  nlist<-vector("list", length(r))
  nlist<-setNames(nlist, r)
  for (i in 1:length(r)){
    nlist[[i]]<-test_sim[[i]][,,n] 
  }
  return(nlist)
}
