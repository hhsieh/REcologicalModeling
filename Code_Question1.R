discrete_logistic_population_growth<-function(n,param){
	r = param[1]
	K = param[2]
	discrete_logistic_population_growth<-r*n*(1-n/K)
	}
	
z1<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(2, 1),0.1,30)

discrete_times=seq(0,30)
plot(discrete_times, z1, xlab="time",ylab="population size", type="l", ylim=c(0,1))

z2<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(2.8, 1),0.1,30)
points(discrete_times, z2,type="l",col="red")

z3<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(3.04, 1),0.1,30)
points(discrete_times, z3,type="l",col="green")

z4<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(3.5, 1),0.1,30)
points(discrete_times, z4,type="l",col="blue")

z5<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(3.9, 1),0.1,30)
points(discrete_times, z5,type="l",col="purple")

text(1.5,1.0,"r=2;", col="black")
text(4,1.0,"r=2.8;",col="red")
text(7.5,1.0,"r=3.04;",col="green")
text(10.5,1.0,"r=3.5;",col="blue")
text(13,1.0,"r=3.9;",col="purple")

## Test more
discrete_logistic_population_growth<-function(n,param){
	r = param[1]
	K = param[2]
	discrete_logistic_population_growth<-r*n*(1-n/K)
	}
	
par(mfrow=c(2,2))
k1<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(1.5, 3),0.1,30)
discrete_times=seq(0,30)
plot(discrete_times, k1, xlab="time",ylab="population size", type="l", ylim=c(0,3))
text(10, 2.8, "n(0)=0.1")

k2<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(1.5,3), 1.1, 30)
plot(discrete_times, k2, ylab="population size",xlab="time",type="l",ylim=c(0,3))
text(10,2.8,"n(0)=1.1")

k3<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(1.5,3),2.4, 30)
plot(discrete_times,k3, ylab="population size",xlab="time", type="l",ylim=c(0,3))
text(10,2.8,"n(0)=2.4")

k4<-simulate_onedimensional_recursion(discrete_logistic_population_growth,c(1.5,3), 2.6, 30)
plot(discrete_times, k4, ylab="population size",xlab="time",type="l", ylim=c(0,3))
text(10,2.8,"n(0)=2.6")
