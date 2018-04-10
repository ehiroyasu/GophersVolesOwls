#'n.table
#'
#'Creates a table of the ode output by initial abundance values.
#'
#'@param df is the output from the function df_sim



n.table<-function(df, times, K_prey, N){
  
  n.final<-NULL
  for(i in 1:length(df)){
    n.final[[i]]<-subset(df[[i]], time==max(times))
    n.final[[i]]$K<-K_prey
    print(kable(n.final[[i]], caption=paste("N init=", unique(N)[i]), row.names = FALSE))
  }
  
  for(i in 1:length(n.final)){
    n.final[[i]]$N.init<-unique(N)[i]
  }
  
  n.table<-bind_rows(n.final)
  
  return(n.table)
}

