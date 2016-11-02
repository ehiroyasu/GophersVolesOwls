#'Calculating percent difference in population density
#'
#'This function calculates the total percent change in populations for the specified time period.
#'
#'@param sim is the list output from the predprey_sim function
#'@param r is a vector of r-values used in the simulation
#'
#'@author Elizabeth Hiroyasu
#'



percent_diff<-function(sim, r){
  diff<-vector(mode="list", length=(length(sim)))
  diff<-setNames(diff, r)
  
  for (i in 1:length(sim)){
    for (j in 1:(dim(sim[[1]])[3])){
      diff[[i]][j]<-((sim[[i]][,,j][dim(sim[[1]])[1],2]-r1976_sim[[i]][,,j][1,2])/r1976_sim[[i]][,,j][1,2])*100
    }
  }
  
  diff<-ldply(diff)
  colnames(diff)<-c("r", P)
  
  
  return(diff)
}






