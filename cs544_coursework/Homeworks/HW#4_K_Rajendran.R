# File: HW#4_K_Rajendran.R
# Author: Ravi K.Rajendran
# Description: CS544_HW#4_codes
# Language: R


library(stats)
#Problem_1
#1_a
batters=6
probability=0.5
strike.outs.50<-dbinom(c(0:batters),size=batters,prob=probability)
strike.outs.50
sum(strike.outs.50)
plot(c(0:batters),strike.outs.50,col="red",type="h",main="PMF for 50% for next 6 batters",xlab="Batters",ylab="Probability")
points(c(0:batters),strike.outs.50,pch=8)
#1_b
cdf<-c(0,cumsum(strike.outs.50))
cdfplot<-stepfun(c(0:batters),cdf)
plot(cdfplot,verticals=FALSE,pch=16,main="CDF for 50% for next 6 batters",xlab="Batters",ylab="Probability")
#1_c
probability=0.7
strike.outs.70<-dbinom(c(0:batters),size=batters,prob=probability)
strike.outs.70
sum(strike.outs.70)
plot(c(0:batters),strike.outs.70,type="h",col="green",main="PMF for 70% for next 6 batters",xlab="Batters",ylab="Probability")
points(c(0:batters),strike.outs.70,pch=8)
cdf<-c(0,cumsum(strike.outs.70))
cdfplot<-stepfun(c(0:batters),cdf)
plot(cdfplot,verticals=FALSE,pch=16,main="CDF for 70% for next 6 batters",xlab="Batters",ylab="Probability")
#1_d
probability=0.3
strike.outs.30<-dbinom(c(0:batters),size=batters,prob=probability)
strike.outs.30
sum(strike.outs.30)
plot(c(0:batters),strike.outs.30,type="h",col="blue",main="PMF for 30% for next 6 batters",xlab="Batters",ylab="Probability")
points(c(0:batters),strike.outs.30,pch=8)
cdf<-c(0,cumsum(strike.outs.30))
cdfplot<-stepfun(c(0:batters),cdf)
plot(cdfplot,verticals=FALSE,pch=16,main="CDF for 30% for next 6 batters",xlab="Batters",ylab="Probability")

#Problem_2
#2_a
probability=0.80
total.flights=10
flights.on.time=4
x1=dbinom(flights.on.time,size=total.flights,prob=probability)
x1
#2_b
flights.on.time=c(0:4)
x2=dbinom(flights.on.time,size=total.flights,prob=probability)
x2
sum(x2)
x2=pbinom(4,size=total.flights,prob=probability)
x2
#2_c
flights.on.time=c(0:total.flights)
x3=dbinom(flights.on.time,size=total.flights,prob=probability)
x3
#2_d
plot(flights.on.time,x3,type="h",col="red",main="PMF for 80% for next 10 flights",xlab="Flights",ylab="Probability")
points(flights.on.time,x3,pch=16)
cdf<-c(0,cumsum(x3))
cdfplot<-stepfun(c(0:total.flights),cdf)
plot(cdfplot,verticals=FALSE,pch=16,main="CDF for 80% for next 10 flights",xlab="Flights",ylab="Probability")

#Problem_3
#3_a
mu=10
x=3
cars.serving1=dpois(x,lambda=mu)
cars.serving1
#3_b
cars.serving2=1-(dpois(0,lambda=mu)+dpois(1,lambda=mu)+dpois(2,lambda=mu))
cars.serving2
#3_c
x1=2
x2=5
cars.serving3=ppois(x2,lambda=mu)-ppois(x1,lambda=mu)
cars.serving3
#3_d
x=20
cars.serving=dpois(c(0:x),lambda=mu)
cars.serving
plot(c(0:x),cars.serving,type="h",col="blue",main="PMF for 20 cars",xlab="Cars",ylab="Probaility")
points(c(0:x),cars.serving,pch=16)

#Problem_4
minimum=60
maximum=100
plot(c(60:100),dunif(c(60:100),min=minimum,max=maximum),type="h",col="blue")
points(c(60:100),dunif(c(60:100),min=minimum,max=maximum),pch=16)
#4_a
x=c(60,80,100)
value<-dunif(x,min=minimum,max=maximum)
names(value)<-x
value
#4_b
uniform.mean=(minimum+maximum)/2
uniform.mean
uniform.sd=sqrt((maximum-minimum)^2/12)
uniform.sd
#4_c
x=70
punif(x,min=minimum,max=maximum)
#4_d
x=80
punif(x,min=minimum,max=maximum,lower.tail=FALSE)
#4_e
x1=90
x2=100
punif(x2,min=minimum,max=maximum)-punif(x1,min=minimum,max=maximum)

#Problem_5
#5_a
mu=100
sigma=10
x1<-c(90:110)
x2<-c(80:120)
x3<-c(70:130)
x<-c(60:140)
x5<-(50:150)
normal.pdf<-dnorm(x,mean=mu,sd=sigma)
plot(x,normal.pdf,col="blue",type="l",main="Normal distribution for theme park souvenirs",xlab="Cost",ylab="Probability")
#5_b
pdf=pnorm(120,mean=mu,sd=sigma,lower.tail=FALSE)
pdf
#5_c
x1=80
x2=90
pdf=pnorm(x1,mean=mu,sd=sigma,lower.tail=FALSE)-pnorm(x2,mean=mu,sd=sigma,lower.tail=FALSE)
pdf
#5_d
sd1=c(mu-sigma,mu+sigma)
sd2=c(mu-2*sigma,mu+2*sigma)
sd3=c(mu-3*sigma,mu+3*sigma)
pdf.sd1=pnorm(sd1[1],mean=mu,sd=sigma,lower.tail=FALSE)-pnorm(sd1[2],mean=mu,sd=sigma,lower.tail=FALSE)
pdf.sd1
pdf.sd2=pnorm(sd2[1],mean=mu,sd=sigma,lower.tail=FALSE)-pnorm(sd2[2],mean=mu,sd=sigma,lower.tail=FALSE)
pdf.sd2
pdf.sd3=pnorm(sd3[1],mean=mu,sd=sigma,lower.tail=FALSE)-pnorm(sd3[2],mean=mu,sd=sigma,lower.tail=FALSE)
pdf.sd3
#5_e
probability=.95
middle.money.spent.low=qnorm(probability,mean=mu,sd=sigma,lower.tail=FALSE)
middle.money.spent.high=qnorm(probability,mean=mu,sd=sigma,lower.tail=TRUE)
middle.money.spent=c(middle.money.spent.low,middle.money.spent.high)
middle.money.spent
middle.90<-pnorm(middle.money.spent[2],mean=mu,sd=sigma,lower.tail=TRUE)-pnorm(middle.money.spent[1],mean=mu,sd=sigma,lower.tail=TRUE)
middle.90
#5_f
visitors.count=10000
visitors<-rnorm(visitors.count,mean=mu,sd=sigma)
plot(table(ceiling(visitors)),type="h",col="green",main="Normal distribution for 10000 visitors",xlab="Visitors counts",ylab="Probability")

#Problem_6
#6_a
lambda=18
x=2
pdf1=pexp(x/60,rate=lambda)
pdf1
#6_b
x=5
pdf2=pexp(x/60,rate=lambda)
pdf2
#6_c
pdf3=pdf2-pdf1
pdf3
#6_d
x<-seq(0,1,by=1/60)
cdf<-pexp(x,rate=18)
plot(x,cdf,type="l",col="blue",main="CDF of exponential distribution",xlab="Hour(60 mins)",ylab="Probability")
