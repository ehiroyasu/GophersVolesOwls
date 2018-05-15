#'n.table
#'
#'Creates a table of the ode output by initial abundance values.
#'
#'@param df is the output from the function df_sim
#'@param times is the length of time over which the scenario was run
#'@param K_prey is the carrying capacity of the prey species
#'@param N is the prey abundance
#'@param alpha is the attack rate of the predator



n.table<-function(df, times, K_prey, N, alphaP){
  
  n.df<-bind_rows(df, .id="N0")
  n.df$N0<-as.numeric(n.df$N0)
  n.df$K<-K_prey
  n.df$alphaP<-alphaP
  
  n.final<-subset(n.df, time==max(times))
  print(kable(n.final, caption="Final Gopher Abundance", row.names = FALSE))
  

  return(n.df)
}

