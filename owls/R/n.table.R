#'n.table
#'
#'Creates a table of the ode output by initial abundance values.
#'
#'@param df is the output from the function df_sim
#'@param times is the length of time over which the scenario was run
#'@param K_prey is the carrying capacity of the prey species
#'@param N is the prey abundance
#'@param alpha is the attack rate of the predator



n.table<-function(df, times, K_prey, N, alpha){
  
  n.final<-NULL
  for(i in 1:length(df)){
    n.final[[i]]<-subset(df[[i]], time==max(times))
    n.final[[i]]$K<-K_prey
    n.final[[i]]$alpha<-alpha
    print(kable(n.final[[i]], caption=paste("N init=", unique(N)[i]), row.names = FALSE))
  }
  
  for(i in 1:length(n.final)){
    n.final[[i]]$N.init<-unique(N)[i]
  }
  
  n.table<-bind_rows(n.final)
  
  return(n.table)
}

