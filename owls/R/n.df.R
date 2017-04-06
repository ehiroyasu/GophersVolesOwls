#'Converting initial simulation data to dataframes.
#'
#'This function converts the list data from the n.sim function to data frames for each initial value of N. These data 
#'frames can then be used to plot the output data.
#'
#'@param sim is the list output from the predprey_sim function
#'@param r is a vector of r-values used in the simulation
#'
#'@author Elizabeth Hiroyasu
#'


n.df<-function(sim, r){
  df<-df_sim(sim=sim, r=r)
  
  return(df)
}
