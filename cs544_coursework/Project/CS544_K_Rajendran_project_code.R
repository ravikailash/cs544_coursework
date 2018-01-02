# File: CS544_K_Rajendran_project_code
# Author: Ravi K.Rajendran
# Type: Source code
# Language: R
# Description: Categorical and Numerical Analysis for the dataset 'Adults'

#Reading data in R
getwd()
setwd("/Users/Ravi/Desktop")
par(mfrow=c(1,1))
adult <- read.csv("adult.csv")
dim(adult)
names(adult)<-c("Age","Workclass","Fnlwgt","Education","EduNum","Maritalstatus","Occupation","Relationship",
                "Race","Sex","CapGain","CapLoss","Hoursperweek","Native","Earning")


#Splitting two classes separately
morepay<-adult[adult$Earning == " >50K.", ]
lesspay<-adult[adult$Earning == " <=50K.", ]
nrow(morepay)
nrow(lesspay)

#Finding and imputing the unknown values with mean or median
length(table(adult$Workclass))
length(table(adult$Occupation))
length(sort(table(adult$Native)))
length(table(adult$Maritalstatus))

#Removing the data by setting ? as NA and using COMPLETE.CASES
adult.dup<-adult
table(adult.dup$Workclass)
adult.dup$Workclass<-replace(as.vector(adult.dup$Workclass),as.vector(adult.dup$Workclass)==" ?", NA)
table(adult.dup$Workclass)

table(adult.dup$Occupation)
adult.dup$Occupation<-replace(as.vector(adult.dup$Occupation),as.vector(adult.dup$Occupation)==" ?", NA)
table(adult.dup$Occupation)

table(adult.dup$Native)
adult.dup$Native<-replace(as.vector(adult.dup$Native),as.vector(adult.dup$Native)==" ?", NA)
table(adult.dup$Native)

#Processed data
adult.cleaned<-adult.dup[complete.cases(adult.dup),]
dim(adult.cleaned)
adult<-adult.cleaned
morepay<-adult[adult$Earning == " >50K.", ]
lesspay<-adult[adult$Earning == " <=50K.", ]

#Age vs earnings
dimnames(adult)
hist(adult$Age, xlim = c(0,100))
table(adult$Age)
levels(factor(adult$Earning))

age.earn.more = numeric(range(adult$Age)[2]- range(adult$Age)[1]+1 )
age.earn.less = numeric(range(adult$Age)[2]- range(adult$Age)[1]+1 )
age.count =1
for (i in c(min(adult$Age):max(adult$Age))){
  age.earn.more[age.count] = nrow(adult[adult$Age == i & adult$Earning ==" >50K." , ])
  age.earn.less[age.count] = nrow(adult[adult$Age == i & adult$Earning ==" <=50K." , ])
  age.count =age.count+ 1
}
age.range = range(adult$Age)[2]- range(adult$Age)[1]+1 
# plot(c(min(adult$Age):max(adult$Age)), age.earn.less,type = "l",main = "Age vs Earning")
# plot(c(min(adult$Age):max(adult$Age)), age.earn.more,type = "l",main = "Age vs Earning")

plot(c(min(adult$Age):max(adult$Age)), age.earn.less,type = "l",main = "Age vs Earning",col ="blue",xlab="Age",ylab="# of people")
lines(c(min(adult$Age):max(adult$Age)), age.earn.more,type = "l",col ="red")

#Gender Vs Earnings
female.more = nrow(adult[adult$Sex == " Female" & adult$Earning ==" >50K." , ])
female.less = nrow(adult[adult$Sex == " Female" & adult$Earning ==" <=50K." , ])
male.more = nrow(adult[adult$Sex == " Male" & adult$Earning ==" >50K." , ])
male.less = nrow(adult[adult$Sex == " Male" & adult$Earning ==" <=50K." , ])

males =nrow(adult[adult$Sex == " Male",])
males
females =nrow(adult[adult$Sex == " Female",])
females
male.earnings<-c(male.less,male.more)
names(male.earnings)<-c('<=50K' , '>50K')
female.earnings<-c(female.less,female.more)
names(female.earnings)<-c('<=50K' , '>50K')
male.earnings
female.earnings
sex.earnings<-data.frame(male.earnings,female.earnings)
barplot(t(as.matrix(sex.earnings)),main="Male vs Female Earnings",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)), ylim = c(0,1.25*max(sex.earnings)),xlab = "Earnings",ylab="#of people")
grid()

arrows(1.5, 20987, 1.7, 22500, code=1)
text(1.7, 23500, "0.6875123")
arrows(2.5, 13026, 2.7, 15500, code=1)
text(2.7, 16500, "0.886424")
arrows(4.5, 9539, 4.7, 11500, code=1)
text(4.7, 12500, "0.3124877")
arrows(5.5, 1669, 5.7, 3000, code=1)
text(5.7, 3500, "0.113576")

male.earnings/males
female.earnings/females

#Education vs Earnings
levels(adult$Education)
male.edu<- adult[adult$Sex ==" Male" & adult$Earning == " >50K.", ]
female.edu<- adult[adult$Sex ==" Female" & adult$Earning == " >50K.", ]

male.ed<-as.data.frame(table(male.edu$Education))
male.education<-c(as.vector(male.ed$Freq))
names(male.education)<- male.ed$Var1
male.education

female.ed<-as.data.frame(table(female.edu$Education))
female.education<-c(as.vector((female.ed$Freq)))
names(female.education)<-female.ed$Var1
sex.education.upto.hs<-data.frame(male.education[c(1,2,3,4,5,6,7,14)], female.education[c(1,2,3,4,5,6,7,14)])
sex.education.after.hs<-data.frame(male.education[c(8,9,10,11,12,13,15,16)], female.education[c(8,9,10,11,12,13,15,16)])
barplot(t(as.matrix(sex.education.upto.hs)),main="Male vs Female Education Earnings >50K",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)), xlab="Education",ylab="#of people")
barplot(t(as.matrix(sex.education.after.hs)),main="Male vs Female Education Earnings >50K",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)),xlab="Education",ylab="#of people")

male.edu.l<- adult[adult$Sex ==" Male" & adult$Earning == " <=50K.", ]
female.edu.l<- adult[adult$Sex ==" Female" & adult$Earning == " <=50K.", ]

male.ed.l<-as.data.frame(table(male.edu.l$Education))
male.education.l<-c(as.vector(male.ed.l$Freq))
names(male.education.l)<- male.ed.l$Var1
male.education.l

female.ed.l<-as.data.frame(table(female.edu.l$Education))
female.education.l<-c(as.vector((female.ed.l$Freq)))
names(female.education)<-female.ed.l$Var1
sex.education.upto.hs.l<-data.frame(male.education.l[c(1,2,3,4,5,6,7,14)], female.education.l[c(1,2,3,4,5,6,7,14)])
sex.education.after.hs.l<-data.frame(male.education.l[c(8,9,10,11,12,13,15,16)], female.education.l[c(8,9,10,11,12,13,15,16)])
barplot(t(as.matrix(sex.education.upto.hs.l)),main="Male vs Female Education Earnings <=50K",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)), xlab="Education",ylab="#of people")
barplot(t(as.matrix(sex.education.after.hs.l)),main="Male vs Female Education Earnings <=50K",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)), xlab="Education",ylab="#of people")

sum(male.education/males)+sum(male.education.l/males)
sum(female.education/females)+sum(female.education.l/females)

a<-list()
edu<-levels(factor(adult$Education))
ear<-levels(factor(adult$Earning))
for(i in c(1:length(levels(factor(adult$Education))))){
  a[[i]]<-c(nrow(adult[adult$Education == edu[i] & adult$Earning == ear[1] , ]), nrow(adult[adult$Education == edu[i] & adult$Earning == ear[2] , ]))
}

a.sum=numeric(16)
for(i in c(1:16)){
  a.sum[i]=sum(a[[i]])
}
sum(a.sum)

a1<-numeric(16)
a2<-numeric(16)
for(i in c(1:2)){
  if(i== 1){
    for (j in c(1:16)){
    a1[j]<-a[[j]][i]
    }
  }else if (i==2){
    for (j in c(1:16)){
      a2[j]<-a[[j]][i]
    }
  }
}

edu.names<-c("10th","11th", "12th","1-4th","5-6th","7-8th","9th","acdm","voc","Bach",
             "Doc","HSgrad","MS","Pre","Prof","Some")

names(a1)<-edu.names
names(a2)<-edu.names
education.earnings<-data.frame(a1,a2)
barplot(t(as.matrix(education.earnings)),main="Education vs Earnings",beside = TRUE,col=c(rgb(0,1,0,0.5), rgb(0,0,1)), xlab="Education level",ylab="Earnings", legend.text = TRUE)

#HS-grad details
male.hs.grad = adult[adult$Education == " HS-grad" & adult$Sex ==" Male",]
female.hs.grad = adult[adult$Education == " HS-grad" & adult$Sex ==" Female",]

male.hsgrad<-data.frame(table(male.hs.grad$Earning))
female.hsgrad<-data.frame(table(female.hs.grad$Earning))

library(plotrix)
slices <- c(male.hsgrad$Freq[1],female.hsgrad$Freq[1],male.hsgrad$Freq[2],female.hsgrad$Freq[2])
lbls <- c("Male:<=50K ", "Female:<=50K ","Male:>50K ","Female:>50K ")
pie3D(slices, labels = lbls, main="HS-Grad variation analysis",explode = 0.1)

#Years of Education vs Earnings
ed.number<-data.frame(table(adult$EduNum))
hist(adult$EduNum, breaks =(seq(0,16, by=1)), xlim=c(0,20),col=rgb(0,0,1,0.5), xlab ="#of years")

edu.more<-data.frame(table(morepay$EduNum))
edunum.more<-edu.more$Freq
edu.less<-data.frame(table(lesspay$EduNum))
edunum.less<-edu.less$Freq
names(edunum.less)<-c(1:16)
names(edunum.more)<-c(1:16)
ednum<-data.frame(edunum.less, edunum.more)

barplot(c(1:16), edunum.more, col=rgb(0,0,1,0.5),main="#of years of school vs Earning >50K", xlab = "# of people")
barplot(edunum.more,col=rgb(0,0,1,0.5),main="#of years of school vs Earning >50K", xlab = "# of people")
barplot(c(1:16), edunum.less,col=rgb(0,1,0,0.5),main="#of years of school vs Earning <=50K", xlab = "# of people")
barplot(edunum.less,col=rgb(0,1,0,0.5),main="#of years of school vs Earning <=50K", xlab = "# of people")

library(ggplot2)
ggplot(data=ednum, aes(x=c(1:16), y=edunum.less)) + xlab("# of years") +ylab("# of people") + ggtitle("#of years of school vs Earning")+
  geom_bar(stat="identity", fill=rgb(0,1,0,0.5), colour="green")+
  geom_bar(data=ednum, aes(x=c(1:16), y=edunum.more),
           stat="identity", fill=rgb(0,0,1,0.5), colour="blue")


#Natives vs Earnings
length(table(morepay$Native))
length(table(lesspay$Native))

nat.more<-data.frame(table(morepay$Native))
native.more<-nat.more$Freq[c(1:14)]
native.more<-append(native.more,0)
native.more<-append(native.more,nat.more$Freq[c(15:40)])
a<-as.character(nat.more$Var1[c(1:14)])
a<-append(a,as.character(nat.less$Var1[15]))
a<-append(a,as.character(nat.more$Var1[c(15:40)]))
names(native.more)<-a
nat.less<-data.frame(table(lesspay$Native))
native.less<-nat.less$Freq
names(native.less)<-as.character(nat.less$Var1)

native<-data.frame(native.less,native.more)
native

par(mfrow=c(1,3))
plot(c(1:41),native$native.less,type="l",col="red", main="Natives vs Earning",xlab="Native people",ylab="# of people")
points(c(1:41),native$native.more,type="l",col="blue")

native.less.rec<-native$native.less[c(c(1:38),c(40,41))]
length(native.less.rec)
native.more.rec<-native$native.more[c(c(1:38),c(40,41))]

plot(c(1:40),native.less.rec,col= "red",type="l", main="Natives vs Earning",xlab="Native people",ylab="# of people")
points(c(1:40),native.more.rec,type = "l",col ="blue")

native.paidmore<-native.more.rec[c(c(1:25),c(27:40))]
native.paidless<-native.less.rec[c(c(1:25),c(27:40))]

plot(c(1:length(native.paidmore)),native.paidmore,col= "blue",type="l", ylim=c(0,250),main="Natives vs Earning",xlab="Native people",ylab="# of people")
points(c(1:length((native.paidless))),native.paidless,type = "l",col ="red")

par(mfrow=c(1,1))


#Hours per week Vs Earnings
hr.more<- data.frame(table(morepay$Hoursperweek))
hr.less<- data.frame(table(lesspay$Hoursperweek))

levels(hr.more$Var1 )
levels(hr.less$Var1 )

par(mfrow=c(1,2))
plot(as.integer(hr.less$Var1), hr.less$Freq,col="red", type="l",main="Hours/week vs earnings <=50K", xlab="Hours/week",ylab="#of people")
plot(as.integer(hr.more$Var1), hr.more$Freq,col="blue", type="l",main="Hours/week vs earnings >50K", xlab="Hours/week",ylab="#of people")
par(mfrow=c(1,1))

plot(as.integer(hr.less$Var1), hr.less$Freq,col="red", type="l",main="Hours/week vs earnings <=50K", xlab="Hours/week",ylab="#of people")
lines(as.integer(hr.more$Var1), hr.more$Freq,col="blue", type="l",main="Hours/week vs earnings >50K", xlab="Hours/week",ylab="#of people")

#Relationships vs Earnings
relation.earn<-c(table(morepay$Relationship),table(lesspay$Relationship))
relation.more=numeric(6)
relation.less=numeric(6)

for(i in c(1:6)){
  relation.more[i]<-relation.earn[i]
  relation.less[i]<-relation.earn[i+6]
}
relation.earn
names(relation.less)<-levels(adult$Relationship)
names(relation.more)<-levels(adult$Relationship)
relationship.earning<-data.frame(relation.less,relation.more)
barplot(t(as.matrix(relationship.earning)),main="Relationship vs Earning",beside =TRUE, col=(c(rgb(0,1,0,0.5),rgb(0,0,1,0.5))),xlab="Relationship", ylab="# of people")

#Final weights
library(vioplot)
summary(adult$Fnlwgt)
vioplot(adult$Fnlwgt,col=rgb(0,0,1,0.5))

boxplot(adult$Fnlwgt,col=rgb(0,0,1,0.5),xaxt="n", xlab="Weight values",main="Boxplot for FinalWeight",horizontal = TRUE)
axis(side=1,at=fivenum(adult$Fnlwgt),labels=TRUE,las=2)
text(fivenum(adult$Fnlwgt),rep(1.2,5),srt=90,adj=0,labels=c("Min","Q1","Median","Q3","Max"))

hist(adult$Fnlwgt,col=rgb(1,0,0,0.5),main="Histogram for FinalWeight",xlab="Weights")
final.weight.lower<-adult[adult$Fnlwgt <= 0.4*(max(adult$Fnlwgt)), ]$Fnlwgt
hist(final.weight.lower,col=rgb(0,0,1,0.5),main="Histogram for FinalWeight-Lower",xlab="Weights")
final.weight.upper<-adult[adult$Fnlwgt > 0.4*(max(adult$Fnlwgt)), ]$Fnlwgt
hist(final.weight.upper,col=rgb(0,0,1,0.5),main="Histogram for FinalWeight-Upper",xlab="Weights")

#Central limit theorem
final.weight<-adult$Fnlwgt
samples<-5000
final.weight.bar.5<-as.numeric(samples)
final.weight.bar.10<-as.numeric(samples)
final.weight.bar.50<-as.numeric(samples)
final.weight.bar.100<-as.numeric(samples)
final.weight.bar.200<-as.numeric(samples)
final.weight.bar.500<-as.numeric(samples)
samples.size<-c(5,10,50,100,200,500)
for(i in 1:samples){
  final.weight.bar.5[i]<-mean(sample(final.weight, samples.size[1]))
  final.weight.bar.10[i]<-mean(sample(final.weight, samples.size[2]))
  final.weight.bar.50[i]<-mean(sample(final.weight, samples.size[3]))
  final.weight.bar.100[i]<-mean(sample(final.weight, samples.size[4]))
  final.weight.bar.200[i]<-mean(sample(final.weight, samples.size[5]))
  final.weight.bar.500[i]<-mean(sample(final.weight, samples.size[6]))
}

par(mfrow=c(2,3))
hist(final.weight.bar.5,main="Histogram for sample size 5", xlab= "weights",col="red")
hist(final.weight.bar.50,main="Histogram for sample size 50", xlab= "weights",col="green")
hist(final.weight.bar.10,main="Histogram for sample size 10", xlab= "weights",col="blue")
hist(final.weight.bar.100,main="Histogram for sample size 100", xlab= "weights",col="red")
hist(final.weight.bar.500,main="Histogram for sample size 500", xlab= "weights",col="green")
hist(final.weight.bar.200,main="Histogram for sample size 200", xlab= "weights",col="blue")
par(mfrow=c(1,1))

mean(final.weight)
final.weight.mean<-c(mean(final.weight.bar.5),mean(final.weight.bar.10),mean(final.weight.bar.50),
                     mean(final.weight.bar.100),mean(final.weight.bar.200),mean(final.weight.bar.500))
final.weight.mean
sd(final.weight)
final.weight.sd<-c(sd(final.weight.bar.5),sd(final.weight.bar.10),sd(final.weight.bar.50),
                   sd(final.weight.bar.100),sd(final.weight.bar.200),sd(final.weight.bar.500))
final.weight.sd

par(mfrow=c(1,3))
plot(samples.size, final.weight.mean, type="h", col="red",ylim=c(0,195000), main="Mean and SD of samples", xlab="Sample sizes",ylab="Means")
points(samples.size,final.weight.mean,pch=8)

plot(samples.size, final.weight.mean, type="h", col="red",main="Mean of samples", xlab="Sample sizes",ylab="Means")
points(samples.size,final.weight.mean,pch=8)

plot(samples.size, final.weight.sd, type="h", col="blue",main="Mean of samples", xlab="Sample sizes",ylab="SD")
points(samples.size,final.weight.sd,pch=8)
par(mfrow=c(1,1))


#Sampling
library(sampling)
#### Simple random sampling with replacement
s<-srswr(10000,nrow(adult.cleaned))
length(s)
table(s)
rows<-(1:nrow(adult.cleaned))[s!=0]
length(rows)
rows<-rep(rows,s[s!=0])
length(rows)
adult.srs.w.rep<-adult.cleaned[rows,]
(table(adult.srs.w.rep$Native))

###### Simple random sampling without replacement
swo<-srswor(10000,nrow(adult.cleaned))
length(swo)
table(swo)
adult.srs.wo.rep<-adult.cleaned[swo!=0,]
table(adult.srs.wo.rep$Native)

#### Systematic sampling
N<-nrow(adult.cleaned)
n<-10000
k<-ceiling(N/n)
k
r<-sample(k,1)
r
s.sys<-seq(r,by=k,length=n)
adult.sys<- adult.cleaned[s.sys,]
table(adult.sys$Native)

#####
#Changing categorical to numeric for inclusionprobability
adult.native.fac<-factor(adult.cleaned$Native)
length(adult.native.fac)
levels(adult.native.fac)<-c(1:41)
adult.native.cleaned<-adult.cleaned
adult.native.cleaned$Native<-as.numeric(adult.native.fac)

levels(adult.native.cleaned$Native)
pik<-inclusionprobabilities(adult.native.cleaned$Native, n)
length(pik)
sum(pik)

s.sys.unequal<-UPsystematic(pik)
table(s.sys.unequal)
adult.sys.unequal<- adult.cleaned[s.sys.unequal!=0,]
table(adult.sys.unequal$Native)

####
#Strata sampling : unequal probility
freq<-table(adult.cleaned$Native)
st.sizes<- round(n*freq/sum(freq))
if(sum(st.sizes) > n){
  zero.pointer<-match(0,st.sizes)
  max.pointer<-match(max(st.sizes),st.sizes)
  if(zero.pointer!= 0 | is.na(zero.pointer)!=FALSE){
    st.sizes[zero.pointer] <-table(adult.cleaned$Native)[[zero.pointer]]
    st.sizes[max.pointer] <- st.sizes[max.pointer] - st.sizes[zero.pointer]
  }
  second.high<-match(sort(st.sizes,decreasing = TRUE)[2], st.sizes)
  st.sizes[second.high] = st.sizes[second.high] - (sum(st.sizes)-n)
}
sum(st.sizes)
length(st.sizes)
sort(table(adult.cleaned$Native))
s.strata<-strata(adult.cleaned[order(adult.cleaned$Native),],stratanames = c("Native"), size= st.sizes, method = "srswor", description = TRUE)

adult.strata<-getdata(adult.cleaned, s.strata)
table(adult.strata$Native)

#Confidence Intervals
population.mean<-mean(adult$Fnlwgt)
population.sd<-sd(adult$Fnlwgt)
sd.sample.means<-population.sd/sqrt(5)
sd(final.weight.bar.5)
sd.sample.means
sd.sample.means<-population.sd/sqrt(100)
sd(final.weight.bar.100)
sd.sample.means
sd.sample.means<-population.sd/sqrt(500)
sd(final.weight.bar.500)
sd.sample.means

population.mean
final.weight.mean
final.weight.sd
samples.size

conf80.lower<-numeric(length(samples.size))
conf80.upper<-numeric(length(samples.size))
conf90.lower<-numeric(length(samples.size))
conf90.upper<-numeric(length(samples.size))
for (i in 1:length(samples.size)){
      conf80.lower[i]<-final.weight.mean[i] - 1.28*(population.sd/sqrt(samples.size[i]))
      conf80.upper[i]<-final.weight.mean[i] + 1.28*(population.sd/sqrt(samples.size[i]))
      conf90.lower[i]<-final.weight.mean[i] - 1.645*(population.sd/sqrt(samples.size[i]))
      conf90.upper[i]<-final.weight.mean[i] + 1.645*(population.sd/sqrt(samples.size[i]))
}

par(mfrow=c(1,2))
plot(samples.size,conf90.lower,type="l",col="red",ylim = c(0,max(conf90.upper)), xlab="Sample size", ylab="values",main="Confidence Interval 90%")
lines(samples.size, conf90.upper,type="l",col="red")
lines(samples.size, rep(population.mean,length(samples.size)),type="l",col="blue")

plot(samples.size,conf80.lower,type="l",col="red",ylim = c(0,max(conf80.upper)), xlab="Sample size", ylab="values",main="Confidence Interval 80%")
lines(samples.size, conf80.upper,type="l",col="red")
lines(samples.size, rep(population.mean,length(samples.size)),type="l",col="blue")
par(mfrow=c(1,1))

xbar<-numeric(20)
for (i in 1:length(samples.size)){
  cat("Sample.size : ",samples.size[i],"\n")
  for (j in 1:20){
    xbar[j]<-mean(sample(final.weight,samples.size[i]))
    cat("80% confidence intervals =",
        xbar[j] - 1.28*(population.sd/sqrt(samples.size[i]))," - ",
        xbar[j] + 1.28*(population.sd/sqrt(samples.size[i])),  "\n")
  }
}

for (i in 1:length(samples.size)){
  cat("Sample.size : ",samples.size[i],"\n")
  for (j in 1:20){
    xbar[j]<-mean(sample(final.weight,samples.size[i]))
    cat("90% confidence intervals =",
        xbar[j] - 1.645*(population.sd/sqrt(samples.size[i]))," - ",
        xbar[j] + 1.645*(population.sd/sqrt(samples.size[i])),  "\n")
  }
}

