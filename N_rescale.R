N_rescale<-function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    
    dpreyprime<-N*(1-N)-((P*N)/(N+D))
    
    list(c(dpreyprime))
  })
}