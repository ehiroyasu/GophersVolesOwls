#'Equilibrium N value
#'
#'Uses the Lotka-Volterra predator-prey differential equations with a type III functional response
#'to calculate populations of both predator and prey populations over a period of time.
#'
#'@param state is the state variables N for the prey population and P for the predator population; measured in individuals per ha.
#'@param parameters is a vector of parameters including r (population growth rate), alpha (attack rate), beta (assimilation efficiency of the predator),
#'delta (death rate of the predator), K_prey (carrying capacity of the prey population), k_max (max feeding rate of the predator), and D (half saturation constant)
#'
#'
#'@author Elizabeth Hiroyasu
#'



N_eq <- function(state, parameters){
  with(as.list(c(state,parameters)), {
   N1<-NULL
     for(i in 1:length(r)){
      for(j in 1:length(P)){
        N1<-((K_prey-D)+(((K_prey-D)^2)+4*(K_prey/r[i])*(1-k_max*P[j]))^0.5)/2
      }
    }
   
    for(i in 1:length(N1)){
      if(N1[i]=='NaN'){
        N1[i]<-0
      } else if (N1[i]<0){
        N1[i]<-0
      }
    }

    for(i in 1:length(r)){
      for(j in 1:length(P)){
        N2<-((K_prey-D)-(((K_prey-D)^2)+4*(K_prey/r[i])*(1-k_max*P[j]))^0.5)/2
      }
    }
    
    for(i in 1:length(N2)){
      if(N2[i]=='NaN'){
        N2[i]<-0
      } else if (N2[i]<0){
        N2[i]<-0
      }
    }
  })
  return(list(c(N1, N2)))  
}