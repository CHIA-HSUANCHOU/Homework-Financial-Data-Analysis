#(b)
retx = function(x){
  x[-1]/x[-length(x)]-1
}

logrx<-function(x){              
  
  diff(log(x))
  
}

#(c)

my_skewness<-function(x){
  T<-length(x)
  y<-x-mean(x)
  T*sqrt(T-1)/(T-2)*sum(y^3)/(sum(y^2)^(3/2))
}

my_kurtosis<-function(x){
  T<-length(x)
  y<-x-mean(x)
  f1<-T*(T+1)*(T-1)/((T-2)*(T-3))
  f2<-3*((T-1)^2)/((T-2)*(T-3))
  f1*sum(y^4)/(sum(y^2)^2)-f2
}
