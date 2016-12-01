#'df_sim
#'
#'Converts the output output list from the predprey_sim function into simulation data into a single dataframe.
#'
#'@param sim is the list output of the simulation result from the predprey_sim function
#'@param r is the r values specified in the parameters 
#'
#'@author Elizabeth Hiroyasu
#'
#'



df_sim<-function(sim, r){
 
  foo<-vector("list", length(r))
  for(i in 1:length(sim)){
    foo[[i]]<-apply(sim[[i]], 3, data.frame)
    foo[[i]]<-ldply(foo[[i]], data.frame)
    foo[[i]]$r<-r[i]
  }
  foo<-rbind.fill(foo)
  foo<-rename(foo, c("X1"="time", "X2"="N", "X3"="P"))
  
  return(foo)
}


