#lotka Volterra predator prey model

#For this model, we will need to use the deSolve package
library(deSolve)

#Writing the Lotka-Volterra predatory prey model as a function:
pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #type III Functional response
    dprey=(r*N*(1-(N/K_prey)))-((k_max*(N^2))/((N^2)+(D^2)))*P #with density dependence
    #dprey=(r*N)-((k_max*(N^2))/((N^2)+(D^2)))*P #without density dependence
    dpredator= beta*P*N - (delta*P)
    
    #return rate of change
    list(c(dprey, dpredator))
    
  })
}


#parameters:
#r = growth rate of prey pop (gophers/season)
#N = starting population of prey 
#P = starting population of predator
#K = carrying capacity of prey
#alpha = attack rate of predator (or capture efficiency; the larger alpha is, the more the per
  #capita growth rate of the prey population is depressed by the addition of a single predator)
#beta= assimilation efficiency (efficiency of turning gophers into per capita growth)
#delta = death rate of predator
#k_max = maximum feeding rate (1/handling time)
#D = half saturation constant (1/(alpha*handling time))

#vectors of parameters and state variables, this allows us to easily change either of them
parameters <- c(r = log(3.47), alpha = 1.68, beta = 0.002, delta = 0.01, K_prey=175, k_max=7.199e3, D=4.285e3)
state<-c(N = 150, P = 2)
times<- seq(0, 1000, by = 1)


out<- ode(y=state, times = times, func = pred_prey, parms = parameters)
matplot(out, xlab="time", main = "Predator Prey Model")


#calculating the functional response
func_response<-NULL
for (i in 1:length(out[,2])){
  for(j in 1:length(out[,3])){
    func_response[i]<-((k_max*(out[i,2]^2))/((out[i,2]^2)+(D^2)))*out[j,3]
  }
}


plot(out[,2], func_response)
