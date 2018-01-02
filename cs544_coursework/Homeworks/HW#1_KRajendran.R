# File: HW#1_K_Rajendran.R
# Author: Ravi K.Rajendran
# Description: CS544_HW#1_codes
# Language: R


#Part#2
#Loading the scores
scores<-c(45, 80, 83, 78, 75, 77, 79, 83, 83, 100)
scores

#a)Displaying first and last item of scores
scores[c(1,length(scores))]

#b)Booelan for the scores less than mean
scores<mean(scores)
#c)Scores less than the mean
scores[scores<mean(scores)]

#d)Repetitive sequence of true and false and printing the scores with respect to TRUE in rep.sequence
(rep.sequence<-rep(c(TRUE,FALSE),5))
scores[rep.sequence]

#e)Pasting letters with scores
paste(LETTERS[1:length(scores)],scores,sep="=")

#f)Creating matrix of 2x5 with the scores
score.matrix<-matrix(scores,nrow=2,ncol=5,byrow = TRUE)
score.matrix

#g)Displaying the first and the last column in the matrix
score.matrix[,c(1,ncol(score.matrix))]

#h)Assigning the names for the rows and columns of the matrix
dimnames(score.matrix)<-list(c("Student_1","Student_2"),c("Quiz_1","Quiz_2","Quiz_3","Quiz_4","Quiz_5"))
score.matrix

#Part#3
#a)Creating and displaying the data frame
college.name<-c("Pomona College","Williams College","Stanford University","Princeton University","Yale University")
college.state<-c("California","Massachusetts","California","New Jersey","Connecticut")
college.cost<-c(62632,64020,62801,58965,63970)
college.population<-c(1610,2150,18346,8014,12109)

college.info<-data.frame(college.name,college.state,college.cost,college.population)
names(college.info)<-c("Name","State","Cost","Population")
college.info

#b)Summary of the state,cost and population
summary(college.info$State)
summary(college.info$Cost)
summary(college.info$Population)

#c)Displaying the name and cost
college.info[c("Name","Cost")]

#d)Displaying the first and last column in the matrix
college.info[c(1,ncol(college.info))]

#e)Displaying the datarows whose population is greater than 5000
subset(college.info,Population>5000)

#f)Modifying the cost in the data with 5% increase and rounded to nearest dollar
cost.rise<-college.info
cost.rise$Cost<-round(cost.rise$Cost+0.05*cost.rise$Cost)
cost.rise
