#Question 1
#AAAAAAAAAAAAAAAA
race<-read.csv("/Users/dijin/Desktop/统计/minipro/2/roadrace.csv",head=T,sep=",",na.strings="*")  #get the data 

place<-race[11]  # get column maine
place_freq<-table(place)  #get the frequency 
print(place_freq)# show proportion of each place 
barplot(place_freq,ylim=c(0,5000))  #draw the barplot according to frenquncy


#BBBBBBBBBBBBBBBBBBB
maine<-subset(race,Maine=="Maine") #get the subset where they are from maine
time_maine<-maine[8]/60 #get time of each runner in minute
#head(time_maine)
#mode(time_maine)  
time_maine<-unlist(time_maine)  # change the type of time_marin for hist
hist(time_maine,main="Time of Maine runner",xlab="time(minute)",ylim=c(0,0.04), probability = T) #draw hist


away<-subset(race,Maine=="Away") #get the subset where they are from away
time_away<-away[8]/60 #get time of each runner in minute
time_away<-unlist(time_away)  # change the type of time_away for hist
hist(time_away,main="Time of Away runner",xlab="time(minute)",ylim=c(0,0.04), probability = T) #draw hist

mean(time_maine) #mean
mean(time_away)

sd(time_maine) #standard 
sd(time_away)

range(time_maine) # range
range(time_away)

median(time_maine) #median
median(time_away)

IQR(time_maine) # IQR
IQR(time_away)

#CCCCCCCCCCCCCC
place<-c("Maine","Away")  #set category
boxplot(time_maine,time_away,names=place, main = "Time of runner", xlab = "Place") #draw boxplot according to place

#DDDDDDDDDDDDDDD
male<-subset(race,Sex=="M") #get the subset where they are from maine
female<-subset(race,Sex=="F") #get the subset where they are from maine
m_age<-as.numeric(unlist(male[5])) #get the column sex=male unlist and change it to numeric for boxplot
f_age<-as.numeric(unlist(female[5]))# same with above]
sex<-c("Male","Female")  #set category
boxplot(m_age,f_age,names=sex, main = "Age of runner", xlab = "Sex") #draw boxplot according to sex

mean(m_age) #mean
mean(f_age)

sd(m_age) #standard 
sd(f_age)

range(m_age) # range
range(f_age)

median(m_age) #median
median(f_age)

IQR(m_age) # IQR
IQR(f_age)


#Question 2
motor<-read.csv("/Users/dijin/Desktop/统计/minipro/2/motorcycle.csv",head=T,sep=",")  #get the data 
number<-unlist(motor[2]) #get the column and unlist for boxplot
boxplot(number, main = "Times of accident") #draw boxplot

summary(number)
IQR(number)
range(number)
temp1<-motor[motor[2]>(quantile(number,0.75)+1.5*IQR(number))] #get the outlier above 75%+IQR
temp2<-motor[motor[2]<(quantile(number,0.25)-1.5*IQR(number))] #get the outlier above 25%-IQR

