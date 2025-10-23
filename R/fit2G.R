fit2G <- function(x,y,param1,param2,...){

  f = function(p){
    d = p[3]*dnorm(x,mean=p[1],sd=p[2]) + p[3]*dnorm(x,mean=p[1],sd=p[2])
    sum((d-y)^2)
  }
  optim(c(param1,param2),f,...)
}
