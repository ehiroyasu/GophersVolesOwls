#lotka Volterra predator prey model

library(deSolve)

#parameters:
#r = growth rate of prey pop
#N = starting population of prey
#P = starting population of predator
#K = carrying capacity of prey
#alpha = attack rate of predator
#epsilon= assimilation efficiency (efficiency of turning gophers into chicks)
#delta = death rate of predator

#vectors of parameters and state variables, this allows us to easily change either of them
parameters <- c(r = log(3.47), alpha = 0.5, epsilon = 0.5, delta = 0.5, K = 15)
state<-c(N = 10, P = 1)
times<- seq(0, 100, by = 0.01)

pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    #dprey=N*r - alpha * P * N #without density dependence, remember to remove K from vector of parameters
    dprey= N * (r * (1-(N/K)) - (alpha * P)) #with density dependence in prey
    dpredator = epsilon * P * N * alpha - delta * P
    
    #return rate of change
    list(c(dprey, dpredator))
  })
  
}


out<- ode(y=state, times = times, func = pred_prey, parms = parameters)

#plot(out, xlab = "time", ylab = "-") #plots on separate graphs
matplot(out, xlab="abundance", main = "Predator Prey Model")

#plotting barn gopher pops vs barn owl pops
#plot(out[,"N"], out[,"P"], pch = ".")

