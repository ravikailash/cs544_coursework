# File: HW#3_K_Rajendran.R
# Author: Ravi K.Rajendran
# Description: CS544_HW#3_codes
# Language: R

#Problem_1
library(UsingR)
primes
prime.difference<-diff(primes,differences = 1)
prime.bar<-table(prime.difference)
barplot(prime.bar,col="pink",xlab = "Difference",ylab = "Frequency",main="Primes difference",ylim=c(0,85))
prime.bar

#Problem_2
coins
names(coins)
#2_a
table(coins$value)
#2_b
coin.values<-data.frame(table(coins$value))
coin.denom<-as.numeric(as.vector(coin.values[,1]))
coin.count<-as.vector(coin.values[,2])
Total.value<-coin.denom*coin.count
Totals<-data.frame(coin.values[,1],Total.value)
names(Totals)<-c("Denomination","Total_counts")
Totals
#2_cTotal value of the coins
Total.denom.value<-sum(Total.value)
Total.denom.value
#2_d
coin.years<-table(coins$year)
barplot(coin.years,col="skyblue",xlab = "Years", ylab = "# of coins", main="Number of coins per year",ylim=c(0,35))

#Problem_3
south
sort(south)
stem(south)
#Summarizing the data 
fivenum(south)
summary(south) 
sd(south)
#Sorting data for the outlier separation 
data.sorted<-sort(south)
#Quantile calculation 
lowerq=quantile(south)[2]
middleq=quantile(south)[3]
upperq=quantile(south)[4] 
iqr=upperq-lowerq; 
#Outlier calculation 
threshold.upper=(iqr * 1.5 )+ upperq 
threshold.lower=lowerq - (iqr * 1.5 )
#Outlier separation 
data.upper<-c(data.sorted[data.sorted>=((iqr * 1.5 )+ upperq)]) 
data.lower<-c(data.sorted[data.sorted<=(lowerq-(iqr * 1.5 ))])
#Plotting the boxplot 
boxplot(south,col=rgb(0,0,1,0.5),xaxt="n",xlab="South data values",main="Boxplot for south",horizontal = TRUE)
axis(side=1,at=fivenum(south),labels=TRUE,las=2)
text(fivenum(south),rep(1.2,5),srt=90,adj=0,labels=c("Min","Q1","Median","Q3","Max"))

#Problem_4
pi2000
#4_a
pi.data<-data.frame(table(pi2000))
names(pi.data)<-c("digits","occurrence")
pi.data
#4_b
pi.data.percentage<-(pi.data$occurrence/sum(pi.data$occurrence))*100
sum(pi.data.percentage)
pi.digit.percentage<-data.frame(pi.data$digits,pi.data.percentage)
pi.digit.percentage
#4_c
hist(pi2000,seq(-1,10,by=1),col="blue",xlab="Digits",ylab="Occurrences",main="# of occurrences of each digits",ylim=c(0,250))

#Problem_5
a<-c(25,20)
b<-c(10,40)
c<-c(15,30)
#5_a
games.data<-cbind(a,b,c)
#5_b
games.row<-c("Men","Women")
rownames(games.data)<-games.row
#5_c
games.col<-c("NFL","NBA","NHL")
colnames(games.data)<-games.col
#5_d
dimnames(games.data)<-list(Gender=games.row,Sport=games.col)
games.data
#5_e
margin.table(games.data,1)
margin.table(games.data,2)
#5_f
addmargins(games.data)
#5_g
prop.table(games.data,1)
prop.table(games.data,2)
#5_h
mosaicplot(games.data,color=c("green","black"))
game.sports.counts <- data.frame(games.data[1,], games.data[2,])
barplot(t(as.matrix(game.sports.counts)),main="Sports",legend=games.row,
        col=c(rgb(0,1,0,0.5), rgb(0,0,1)), beside=TRUE,args.legend = list(x = "topleft",bty="n"))
game.gender.counts<-data.frame(games.data[,1],games.data[,2],games.data[,3])
barplot(t(as.matrix(game.gender.counts)),main="Gender",legend=games.col,
        col=c(rgb(0,1,0,0.5),rgb(0,0,1),"pink"),beside=TRUE,args.legend = list(x = "topleft", bty = "n"))

#Problem_6
require(ggplot2)
pairs(midsize)


#Problem_7
require(UsingR)
names(MLBattend)
desired.teams<-c("BAL","BOS","DET","LA","PHI")
#7_a->Function for counting the total wins for all the teams
counting<-function(teams,desired.teams){
  x.counts<-numeric(length(desired.teams))
  for (i in 1:nrow(teams)){
    if(is.element(teams$franchise[i],desired.teams)){
      r<-match(teams$franchise[i],desired.teams)
      x.counts[r]<-x.counts[r]+teams$wins[i]
    }
  }
  return (x.counts)
}
teams.wins<-counting(MLBattend,desired.teams)
teams.wins

#7_a->Funtion for creating the vector of all wins for each teams individually
counts<-function(teams,desired.team){
  x.counts<-numeric(1)
  r<-1
  for (i in 1:nrow(teams)){
    if(teams$franchise[i]==desired.team){
      x.counts[r]<-teams$wins[i]
      r<-r+1
    }
  }
  return (x.counts)
}
team.BAL.wins<-counts(MLBattend,c("BAL"))
(team.BAL.wins)
team.BOS.wins<-counts(MLBattend,c("BOS"))
(team.BOS.wins)
team.DET.wins<-counts(MLBattend,c("DET"))
(team.DET.wins)
team.LA.wins<-counts(MLBattend,c("LA"))
(team.LA.wins)
team.PHI.wins<-counts(MLBattend,c("PHI"))
(team.PHI.wins)
#7_b
team.data<-data.frame(c(team.BAL.wins),c(team.BOS.wins),c(team.DET.wins),c(team.LA.wins),c(team.PHI.wins))
names(team.data)<-desired.teams
head(team.data)
#7_c
boxplot(team.data,xlab="teams",ylab="# of wins",main="# of wins per teams")

