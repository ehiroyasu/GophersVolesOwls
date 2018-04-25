#'Equilibrium N value
#'
#'Uses the Lotka-Volterra predator-prey differential equations with a type III functional response
#'to calculate populations of both predator and prey populations over a period of time.
#'
#'@param P is the scaled predator density (individuals/ha), here P represents (kP)/K_prey
#'@param D is the scaled half saturation constant (individuals/ha), here D represents (K_prey)/(alpha*h)
#'
#'@author Elizabeth Hiroyasu
#'

N_eq <- function(P,D){

  names<- c("D", "P", "eq")
    N1<-array(data=NA, dim=c(length(D), length(P)), dimnames=list(c(D), c(P)))
     
    for(i in 1:length(D)){
      for(j in 1:length(P)){
             N1[i,j]<-(-(D[i]-1)+(((D[i]-1)^2)-(4*(P[j]-D[i])))^0.5)/2
          }
        }
 
  N1<-melt(N1)
  colnames(N1)<-names

  N2<-array(data=NA, dim=c(length(D), length(P)), dimnames=list(c(D), c(P)))
  #N2<-array(data=NA, dim=c(length(P), length(r), length(D), length(K_prey)), dimnames=list(c(P), c(r), c(D), c(K_prey)))
  for(i in 1:length(D)){
    for(j in 1:length(P)){
      N2[i,j]<-(-(D[i]-1)-(((D[i]-1)^2)-(4*(P[j]-D[i])))^0.5)/2
    }
  }

  N2<-melt(N2)
  colnames(N2)<-names

  eq.df<- rbind(N1, N2)
  eq.df$N <- c(rep("N1", nrow(N1)), rep("N2", nrow(N2)))

  return(eq.df)
 }