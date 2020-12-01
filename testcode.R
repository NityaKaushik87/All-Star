#Install Packages 
install.packages("ggthemes")
install.packages("openxlsx")
install.packages("lmtest")
install.packages("nortest")
install.packages("psych")
#Load Packages 
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(nortest)
library(psych)
library(ggpmisc)
library(car)


setwd("~/Files/fall2020/ISM6423/data_p1") 

case_info<- read.csv("case_info.csv")
mydata1<- read.csv("Pop Density.csv")
employ_rate<- read.csv("Employment rate.csv")
crime_info<- read.csv("Crime rate.csv")
old_info<- read.csv("Old people.csv")
educ_info<- read.csv("Education.csv")
health_coverage<- read.csv("Health coverage.csv")
pop_growth<- read.csv("Population_Growth.csv")
percapita_income<- read.csv("Household income.csv")
family_income<- read.csv("Family Income.csv")
factor_YJB<-read.csv("Fatality-Variables.csv") #yjb's dataset



mydata1$total_case<-case_info$Total.Cases
mydata1$total_death<-case_info$Total.Deaths
mydata1$fatality<-mydata1$total_death/mydata1$total_case
mydata1<-mydata1[,c(1,7,2,3,4,5,6)]

#1 fatality~population 
graph_1 <- ggplot(mydata1, aes(mydata1$Pop, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Population")+ xlab("\nPopulation")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_1 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$Pop))
summary(lm(log(mydata1$fatality)~ mydata1$Pop))
graph_1.5 <- ggplot(mydata1, aes(mydata1$Density, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Population")+ xlab("\nPop Density")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_1.5 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$Density))
## the pop density seems to be extremely high-correlated (I not sure whether I've made a mistake here)


#2 falality~employment rate
mydata1$empl_rate<-employ_rate$employmentRate
graph_2 <- ggplot(mydata1, aes(mydata1$empl_rate, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Employment Rate")+ xlab("\nEmployment Rate")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_2 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$empl_rate))
summary(lm(log(mydata1$fatality)~ mydata1$empl_rate))


#3 fatality~old people
mydata1$`over85%`<-old_info$Over85Perc
mydata1$`over75%`<-old_info$X75AndOverPerc
mydata1$`over65%`<-old_info$X65AndOverPerc
mydata1$median_age<-old_info$MedianAge
mydata1$median_age_male<-old_info$MedianAgeMale
mydata1$median_age_female<-old_info$MedianAgeFemale
graph_3 <- ggplot(mydata1, aes(mydata1$`over85%`, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Age")+ xlab("\nOver 85 Percent")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_3 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$`over85%`))
summary(lm(log(mydata1$fatality)~ mydata1$`over85%`))
summary(lm(mydata1$fatality~ mydata1$`over75%`))
summary(lm(mydata1$fatality~ mydata1$`over65%`))
summary(lm(mydata1$fatality~ mydata1$median_age))
summary(lm(mydata1$fatality~ mydata1$median_age_male))
summary(lm(mydata1$fatality~ mydata1$median_age_female))
graph_4 <- ggplot(mydata1, aes(mydata1$median_age_female, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Age")+ xlab("\nMedian Age of Female")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_4 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
#Seems a little bit reasonable, but R^2 still low

#4 fatality~education
mydata1$`HS%`<-educ_info$PercentHighSchoolOrHigher
mydata1$`BA%`<-educ_info$PercentBachelorsOrHigher
graph_5 <- ggplot(mydata1, aes(mydata1$`HS%`, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Education")+ xlab("\nHigh School Percentage")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_5 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$`HS%`))
graph_6 <- ggplot(mydata1, aes(mydata1$`BA%`, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Education")+ xlab("\nBachelor Percentage")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_6 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$`BA%`))
summary(lm(log(mydata1$fatality)~ mydata1$`BA%`))
summary(lm(mydata1$fatality~ log(mydata1$`BA%`)))
#Bachelor Percentage performs better but still low

#5 fatality~income
mydata1$percapita_income<-percapita_income$HouseholdIncome
mydata1$family_income<-family_income$FamiliesMeanIncome
graph_7 <- ggplot(mydata1, aes(mydata1$percapita_income, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Income")+ xlab("\nPer Capita Income")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_7 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$percapita_income))
summary(lm(mydata1$fatality~ log(mydata1$percapita_income)))
graph_8 <- ggplot(mydata1, aes(mydata1$family_income, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Income")+ xlab("\nFamily Income")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_8 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$family_income))
summary(lm(mydata1$fatality~ log(mydata1$family_income)))
#Income seems a little better

#6 fatality~race/poverty
mydata1$poverty_rate<-factor_YJB$PovertyRate
mydata1$nh_black<-factor_YJB$Non.Hispanic.Black
mydata1$nh_white<-factor_YJB$Non.Hispanic.White
graph_9 <- ggplot(mydata1, aes(mydata1$nh_black, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Race")+ xlab("\nNon-Hispanic Black rate")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_9 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$nh_black))
summary(lm(mydata1$fatality~ mydata1$nh_white))
summary(lm(mydata1$fatality~ mydata1$poverty_rate))


#7 fatality~climate
mydata1$temperature<-factor_YJB$AverageTemperature
mydata1$humidity<-factor_YJB$Humidity
graph_10 <- ggplot(mydata1, aes(mydata1$temperature, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Climtae")+ xlab("\nTemperature")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_10 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$temperature))
graph_11 <- ggplot(mydata1, aes(mydata1$humidity, mydata1$fatality))+ geom_point(size=1)+
  ggtitle("Fatality by Climtae")+ xlab("\nHumidity")+ ylab("Fatality\n")+
  geom_smooth(method='lm', col="red")
graph_11 + theme_economist()+ theme(plot.title = element_text(hjust = 0.5))
summary(lm(mydata1$fatality~ mydata1$humidity))


# Still needs a lot of more reasonable correalted variables 
# I am not sure about  using the log or squared or interaction

# For multiple linear regression, for example like fanily income &  Bachelor percent
summary(lm(mydata1$fatality~ mydata1$family_income+mydata1$`BA%`))
# For interactions, seems much better, but not sure how whether we need to use mean-center predictors
summary(lm(mydata1$fatality~ mydata1$family_income*mydata1$`BA%`))
summary(lm(mydata1$fatality~ mydata1$percapita_income*mydata1$`BA%`))





