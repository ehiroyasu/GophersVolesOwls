#lotka Volterra predator prey model

library(deSolve)

#parameters:
#r = growth rate of prey pop
#N = starting population of prey
#P = starting population of predator
#K = carrying capacity of prey
#alpha = attack rate of predator (or capture efficiency; the larger alpha is, the more the per
  #capita growth rate of the prey population is depressed by the addition of a single predator)
#beta= assimilation efficiency (efficiency of turning gophers into per capita growth)
#delta = death rate of predator

#vectors of parameters and state variables, this allows us to easily change either of them
parameters <- c(r = log(3.47), alpha = 0.3, beta = 0.5, delta = 0.5, K_prey = 175, K_pred = 6)
state<-c(N = 10, P = 1)
times<- seq(0, 100, by = 0.01)

pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    #dprey=N*r - alpha * P * N #without density dependence, remember to remove K from vector of parameters
    dprey= N * (r-alpha*P)*(1-(N/K_prey)) #with density dependence in prey
    dpredator = P*(beta * N * alpha - delta)*(1-(P/K_pred))#with density dependence in predator
    
    #return rate of change
    list(c(dprey, dpredator))
  })
  
}


out<- ode(y=state, times = times, func = pred_prey, parms = parameters)

#plot(out, xlab = "time", ylab = "-") #plots on separate graphs
matplot(out, xlab="time", main = "Predator Prey Model")

#plotting barn gopher pops vs barn owl pops
#plot(out[,"N"], out[,"P"], pch = ".")

