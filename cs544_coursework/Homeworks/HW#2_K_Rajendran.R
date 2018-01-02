# File: HW#2_K_Rajendran.R
# Author: Ravi K.Rajendran
# Description: CS544_HW#2_codes
# Language: R

#Part#2
#Bayes rule
bayes<-function(prior,likelihood){
  num<-prior*likelihood
  den<-sum(num)
  return(num/den)
}
#Problem--1(a)
prior<-c(0.07,0.93)
likelihood<-c(0.10,0.75)
bayes(prior,likelihood)

#Problem--1(b)
prior<-c(0.40,0.50,0.10)
likelihood<-c(0.70,0.40,0.20)
bayes(prior, likelihood )


#Functions
#Problem--3(a)
is.prime<-function(x){
  if(is.element(x,c(1:3))){                       #checks whether x is 1 or 2 or 3
    if(x==1){
      cat("1 is neither prime nor composite!")
    }else{
    return (TRUE)
    }
  }else if(x%%2==0){
    return (FALSE)
  }else{
    sq.x<-as.integer(sqrt(x))                     #Vector operation instead of for loop
    p.check<-c(3:max(sq.x,3))
    p.check<-x%%p.check
    if(is.element(0,p.check)){
      return (FALSE)
    } else{
      return (TRUE)
    }
  }
}

is.prime(123456789)

#Problem--3(b)
i=2
x<-2
for(i in 2:100){
  res<-is.prime(i)
  if(res==TRUE && i!=2){
    x<-c(x,i)                                     # Creating a vector x which stores all primes
  }
}
x

#Problem--2(b)
library(prob)
S<-rolldie(2,makespace=TRUE)
s<-addrv(S,X=abs(X1-X2))                          # X is the random variable
s

Prob(s,X== 3)
Prob(s,X<= 3)
Prob(s,X>= 4)

#Problem--2(c)
marginal(s,vars="X")

#Problem--2(d)
rolltwice<-function(x){
  if(x[[1]]==2*x[[2]] | x[[2]]==2*x[[1]]){        # Accessing x as a dataframe
    return (TRUE)
  }else{
    return (FALSE)
  }
}
s1<-addrv(s,FUN=rolltwice,name="RT")              # Creating another random variable RT
s1
Prob(s1,RT==TRUE)
marginal(s1,vars="RT")

