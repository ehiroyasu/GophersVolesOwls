#'Type II Lotka Volterra Function
#'
#'Uses the Lotka-Volterra predator-prey differential equations with a type III functional response
#'to calculate populations of both predator and prey populations over a period of time.
#'
#'@param t is a sequence of numbers with a specified time step
#'@param state is the state variables N for the prey population and P for the predator population; measured in individuals per ha.
#'@param parameters is a vector of parameters including r (population growth rate), alpha (attack rate), beta (assimilation efficiency of the predator),
#'delta (death rate of the predator), K_prey (carrying capacity of the prey population), k_max (max feeding rate of the predator), and D (half saturation constant)
#'
#'
#'@author Elizabeth Hiroyasu
#'


pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #type II Functional response
    dprey=(r*N*(1-(N/K_prey)))-((k_max*N)/(N+D))*P #with density dependence
    #dprey=(r*N)-((k_max*N)/(N+D))*P #without density dependence
    dpredator=0 #constant number of predators
    #dpredator= beta*P*N - (delta*P) #incorporating predator
    
    #return rate of change
    list(c(dprey, dpredator))
    
  })
}