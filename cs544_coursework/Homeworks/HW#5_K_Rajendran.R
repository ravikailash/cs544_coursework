# File: HW#5_K_Rajendran.R
# Author: Ravi K.Rajendran
# Description: CS544_HW#5_codes
# Language: R

#Problem-1
#1_a
input<-c(1:25)
input
par(mfrow=c(1,3))
hist(input,main="Histogram of input",xlab="data range",ylab="Frequency",ylim=c(0,6),col="red")

#1_b
samples=100
sample.size=2
x1.mean<-numeric(samples)
for (i in 1:samples){
  x1.mean[i]<-mean(sample(input,size=sample.size))
}
x1.mean
hist(x1.mean,main="Histogram(samplesize=2)",xlab="data range",ylab="Frequency",col="light green")

#1_c
sample.size=5
x2.mean<-numeric(samples)
for (i in 1:samples){
  x2.mean[i]<-mean(sample(input,size=sample.size))
}
x2.mean
hist(x2.mean,main="Histogram(samplesize=5)",xlab="data range",ylab="Frequency",col="light blue")

#1_d
x.mean<-c(mean(input), mean(x1.mean), mean(x2.mean))
x.mean
x.sd<-c(sd(input), sd(x1.mean), sd(x2.mean))
x.sd


#Problem-2
#2_a
x=1000
par(mfrow=c(1,1))
input2<-rnbinom(x,size=5, prob=0.5)
input2.proportions<-table(input2)
input2.maxFrequency<-data.frame(input2.proportions)
max(input2.maxFrequency$Freq)
barplot(input2.proportions,col="blue",xlab="Values",ylab="Occurances",main="Negative binomial distribution for 1000 variables",ylim=c(0,max(input2.maxFrequency$Freq)+10))

#2_b
samples=5000
sample.size<-seq(10,40,by=10)
x1.mean<-numeric(samples)
x2.mean<-numeric(samples)
x3.mean<-numeric(samples)
x4.mean<-numeric(samples)
for (i in 1:samples) {
  x1.mean[i]<-mean(rnbinom(samples,size=sample.size[1],prob=0.5))
  x2.mean[i]<-mean(rnbinom(samples,size=sample.size[2],prob=0.5))
  x3.mean[i]<-mean(rnbinom(samples,size=sample.size[3],prob=0.5))
  x4.mean[i]<-mean(rnbinom(samples,size=sample.size[4],prob=0.5))
}
par(mfrow=c(2,2))
hist(x1.mean,col="red")
hist(x2.mean,col="light blue")
hist(x3.mean,col="light green")
hist(x4.mean,col="pink")

#2_c
xmean.mean<-c(mean(x1.mean),mean(x2.mean),mean(x3.mean),mean(x4.mean))
xmean.sd<-c(sd(x1.mean),sd(x2.mean),sd(x3.mean),sd(x4.mean))
xmean.mean
xmean.sd


#Problem-3
library(sampling)
data(MU284)
input3<-MU284
nrow(input3)
input3.reg<-data.frame(table(input3$REG))
input3.reg
x.count<-(input3.reg$Freq/sum(input3.reg$Freq))*100
names(x.count)<-input3.reg$Var1
x.count
sum(x.count)

#3_a
sample.size=20
s1<-srswor(sample.size,nrow(input3))
table(s1)
input3a.sampled<-input3[s1!=0,]
nrow(input3a.sampled)
options(digits=4)
x1.reg<-input3a.sampled$REG
x1.reg
x1.reg.count<-data.frame(table(x1.reg))
x1.reg.count
xa.count<-(x1.reg.count$Freq/sum(x1.reg.count$Freq))*100
names(xa.count)<-x1.reg.count$x1.reg
xa.count
sum(xa.count)

#3_b
x2<-sample(ceiling(nrow(input3)/sample.size),1)
x2
s2<-seq(x2, by=ceiling(nrow(input3)/sample.size), length=sample.size)
s2
input3b.sampled<-input3[s2,]
nrow(input3b.sampled)
x2.reg<-input3b.sampled$REG
x2.reg.count<-data.frame(table(input3b.sampled$REG))
x2.reg.count
xb.count<-(x2.reg.count$Freq/sum(x2.reg.count$Freq))*100
names(xb.count)<-x2.reg.count$input3b.sampled.reg
xb.count
sum(xb.count)

#3_c
x3<-inclusionprobabilities(input3$S82,sample.size)
s3<-UPsystematic(x3)
table(s3)
input3c.sampled<-input3[s3!=0,]
nrow(input3c.sampled)
x3.reg.count<-data.frame(table(input3c.sampled$REG))
x3.reg.count
xc.count<-(x3.reg.count$Freq/sum(x3.reg.count$Freq))*100
names(xc.count)<-x3.reg.count$Var1
xc.count
sum(xc.count)

#3_d
reg.freq<-table(input3$REG)
x4.sizes<-sample.size*reg.freq/sum(reg.freq)
x4<-strata(input3,stratanames = c("REG"),size = x4.sizes,method="srswor",description = TRUE)
nrow(x4)
input3d.sampled<-getdata(input3,x4)
nrow(input3d.sampled)
x4.reg.count<-data.frame(table(x4$REG))
x4.reg.count
xd.count<-(x4.reg.count$Freq/sum(x4.reg.count$Freq))*100
names(xd.count)<-x4.reg.count$Var1
xd.count
sum(xd.count)

#3_e
input3a.mean<-mean(input3a.sampled$RMT85,na.rm=TRUE)
input3b.mean<-mean(input3b.sampled$RMT85,na.rm=TRUE)
input3c.mean<-mean(input3c.sampled$RMT85,na.rm=TRUE)
input3d.mean<-mean(input3d.sampled$RMT85,na.rm=TRUE)
input3.mean<-c(input3a.mean,input3b.mean,input3c.mean,input3d.mean)
names(input3.mean)<-c(1,2,3,4)
input3.mean

