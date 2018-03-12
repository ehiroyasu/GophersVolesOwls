#parameters <- c(r = log(3.47), alpha = 0.3, beta = 0.5, delta = 0.5, K_prey = 175, K_pred = 6)
state<-c(N = 500, P = 1)
times<- seq(0, 10, by = 1)

pred_prey <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    #dprey=N*r - alpha * P * N #without density dependence, remember to remove K from vector of parameters
    dprey= N * (r-alpha*P)*(1-(N/K_prey)) #with density dependence in prey
    #dpredator = P*(beta * N * alpha - delta)#*(1-(P/K_pred))#with density dependence in predator
    dpredator=0
    #return rate of change
    list(c(dprey, dpredator))
  })
  
}

#out<- ode(y=state, times = times, func = pred_prey, parms = parameters)
#matplot(out[,2:3], xlab="time", main = "Predator Prey Model", type="l")


#looping through three different alpha values, the writing to a list
alpha<-c(0.001, 0.01, 0.1, 0.25, 1.0, 1.5, 2)

out_alpha_sim<-vector("list", length(alpha))
names(out_alpha_sim)=alpha

for(i in 1:length(alpha)){
  out_alpha_sim[[i]]<- ode(y=state, times = times, func = pred_prey, parms = c(r = log(3.47), alpha=alpha[i], K_prey = 1000))
}

alpha_sim_N<-ldply(out_alpha_sim, data.frame)
ggplot(data=alpha_sim_N[,-4], aes(x=time, y=N, group=as.factor(.id), color=as.factor(.id)))+geom_line()

#Plotting different outcomes
matplot(out_alpha_sim[[1]], lty=1:2, type='l', col=1, main="Predator prey dynamics over time")

#adding extra lines manually
#matplot(out_alpha_sim[[2]], add=TRUE,lty = 1:2, type = 'l', col=3)
#matplot(out_alpha_sim[[3]], add=TRUE, lty=1:2, type='l', col = 5)

#adding elements of list with a loop
for(j in 1:length(alpha)){
  matplot(out_alpha_sim[[j]], add=TRUE, lty=1:2, type='l', col=c(j))
}


#looping through three different beta values, the writing to a list
beta<-c(0.001, 0.01, 0.1)

out_beta_sim<-vector("list", length(beta))
names(out_beta_sim)=beta

for(i in 1:length(beta)){
  out_beta_sim[[i]]<- ode(y=state, times = times, func = pred_prey, parms = c(r = log(3.47), alpha=1.68, beta = beta[i], delta = 0.5, K_prey = 175, K_pred = 6))
}

#Plotting different outcomes
matplot(out_beta_sim[[1]], lty=1:2, type='l', col=1, main="Predator prey dynamics over time")

#adding extra lines manually
#matplot(out_alpha_sim[[2]], add=TRUE,lty = 1:2, type = 'l', col=3)
#matplot(out_alpha_sim[[3]], add=TRUE, lty=1:2, type='l', col = 5)

#adding elements of list with a loop
for(j in 1:length(beta)){
  matplot(out_beta_sim[[j]], add=TRUE, lty=1:2, type='l', col=c(j))
}
