#lotka Volterra predator prey model

#first create function, containing the derivative equations
pred_prey <- function(t, x, pars){
  N <- x[1]
  P <- x[2]
  r = pars[1]
  C = pars[2]
  B = pars[3]
  d = pars[4]
  
  dprey = r * N - C * P * N
  dpredator = B * P * N - d * P
  list(c(dprey, dpredator))
}

#r = growth rate of prey pop
#N = starting population of prey
#C = attack rate of predator
#P = starting population of predator
#B = birth rate of predator
#d = death rate of predator

#Use the package deSolve to enable the use of ODE solvers to fit parameters
library(deSolve)

  t = seq(from=2, to=100, by=0.1) #time interval
  x<-c(3000, 2)
  #init = c(0.4, 0.2) #inital values
  r = log(3.47) #remember that r=ln(lambda)
  d = 0.7 #quick number pulled from a google search
  C = 1
  B = 4 #average number of chicks per nest box 
  pars = c(r, C, B, d)
  predprey_output <- ode(x, t, pred_prey, pars)
  par(mfrow=c(1,2))
  plot(predprey_output[,'time'], predprey_output[,2], col="blue", lwd=2,xlab="Time", ylab="Prey", main="Prey", type="l")
  grid()
  plot(predprey_output[,'time'], predprey_output[,3], col="red", lwd=2,xlab="Time", ylab="Predator", main="Predator",type="l")
  grid()
  
  