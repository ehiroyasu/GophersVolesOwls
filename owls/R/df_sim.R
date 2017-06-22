#'df_sim
#'
#'Converts the output output list from the predprey_sim function into simulation data into a single dataframe.
#'
#'@param sim is the list output of the simulation result from the predprey_sim function
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
    return(foo)
}


