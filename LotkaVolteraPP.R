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
#k_max = maximum feeding rate
#D = half saturation constant (1/(alpha*k_max))

#vectors of parameters and state variables, this allows us to easily change either of them
parameters <- c(r = log(3.47), alpha = 0.01, beta = 0.3, delta = 0.6, K_prey = 175, k_max=10, D=1/(0.3*100))#, K_pred = 6)
state<-c(N = 30, P = 2)
times<- seq(0, 100, by = 0.01)

pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    #dprey=N*r - alpha * P * N #without density dependence, remember to remove K from vector of parameters
    #dprey= (r*N*(1-(N/K_prey)))-alpha*P*N #with density dependence in prey
    
    #type III Functional response
    dprey=(r*N)-((k_max*(N^2))/((N^2)+(D^2)))*P
    
    dpredator= beta*P*N*alpha - (delta*P)
    #dpredator = P*(beta * N * alpha - delta)*(1-(P/K_pred))#with density dependence in predator
    
    #return rate of change
    list(c(dprey, dpredator))
    
  })
  
}


out<- ode(y=state, times = times, func = pred_prey, parms = parameters)

plot(out[,1],out[,2], xlab = "Years", ylab = "Gopher Density", type='l', main="Gopher Density Over Time with
     Six Owls Present, K=50")
abline(h=state[2], col=3, lty=2)
matplot(out, xlab="time", main = "Predator Prey Model")

#plotting barn gopher pops vs barn owl pops
#plot(out[,"N"], out[,"P"], pch = ".")

