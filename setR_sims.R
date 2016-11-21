library(roxygen2)
library(devtools)
install("owls")
library(owls)

library(deSolve)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)


alpha=1.68
beta = 0.002
delta=0.01
K_prey=175
h=1.39e-4
k_max=1/h
D=1/(alpha*h)
parameters <- c(r = log(3.47), alpha, beta, delta, K_prey, k_max, D)
N=c(150, 150, 150, 150, 150)
P=c(0.2, 0.4, 0.6, 0.95, 2.0)
state<-cbind(N, P)
times<- seq(0, 20, by=0.1)


r<-c(0.1, 0.9, 1.2, 1.9)


test_sim<-predprey_sim(times=times, state=state, r=r)

matplot_sim(times=times, sim=test_sim, r=r, s=1)
matplot_sim(times=times, sim=test_sim, r=r, s=2)
matplot_sim(times=times, sim=test_sim, r=r, s=3)
matplot_sim(times=times, sim=test_sim, r=r, s=4)
