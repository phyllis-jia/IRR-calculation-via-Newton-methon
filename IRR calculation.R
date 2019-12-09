irr<-function(x,payment1,payment2,payment3,pv) {x-((payment1/(1+x)+payment2/(1+x)^2+payment3/(1+x)^3-pv)/(-payment1/(1+x)^2-2*payment2/(1+x)^3-3*payment3/(1+x)^4))}
x<-seq(1:50)
x[1]<-1
payment1<-runif(1,1,100)
payment2<-runif(1,1,100)
payment3<-runif(1,1,100)
pv<-runif(1,1,100)
for(i in 2:50) x[i]<-irr(x[i-1],payment1,payment2,payment3,pv)
verify<-function(x,payment1,payment2,payment3,pv) {payment1/(1+x)+payment2/(1+x)^2+payment3/(1+x)^3-pv}
verify(x,payment1,payment2,payment3,pv)==0   

