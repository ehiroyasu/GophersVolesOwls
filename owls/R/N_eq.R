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



N_eq <- function(P, r, K_prey, D, k_max){

  names<- c("P", "r", "D", "K_prey", "Eq")
    N1<-array(data=NA, dim=c(length(P), length(r), length(D), length(K_prey)), dimnames=list(c(P), c(r), c(D), c(K_prey)))
     
    for(i in 1:length(P)){
      for(j in 1:length(r)){
        for(k in 1:length(D)){
          for(l in 1:length(K_prey)){
            N1[i,j,k,l]<-((K_prey[l]-D[k])+((((K_prey[l]-D[k])^2)+(4*(K_prey[l]/r[j])*((r[j]*D[k])-k_max*P[i])))^0.5))/2
          }
        }
      }
    }
   
    # for(i in 1:length(N1)){
    #   if(N1[i]=='NaN'){
    #     N1[i]<-0
    #   } else if (N1[i]<0){
    #     N1[i]<-0
    #   }
    # }
  N1<-melt(N1)
  colnames(N1)<-names

  N2<-array(data=NA, dim=c(length(P), length(r), length(D), length(K_prey)), dimnames=list(c(P), c(r), c(D), c(K_prey)))
     for(i in 1:length(P)){
      for(j in 1:length(r)){
        for(k in 1:length(D)){
          for(l in 1:length(K_prey)){
            N2[i,j, k, l]<-((K_prey[l]-D[k])-((((K_prey[l]-D[k])^2)+(4*(K_prey[l]/r[j])*((r[j]*D[k])-k_max*P[i])))^0.5))/2
          }
        }
      }
    }
  
  # 
  # for(i in 1:length(N2)){
  #   if(N2[i]=='NaN'){
  #     N2[i]<-0
  #   } else if (N2[i]<0){
  #     N2[i]<-0
  #   }
  # }
  
  N2<-melt(N2)
  colnames(N2)<-names

  eq.df<- rbind(N1, N2)
  eq.df$N <- c(rep("N1", nrow(N1)), rep("N2", nrow(N2)))
  eq.df$K_prey<-as.factor(eq.df$K_prey)
  eq.df$N<-as.factor(eq.df$N)

  return(eq.df)
 }