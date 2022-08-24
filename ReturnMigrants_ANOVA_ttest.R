ReturnMigrantsData<- read.csv("All_SPOTACT_Return_Migrants.csv")

#check normality
###We need to check assumptions in order to verify ANOVA is correct choice for analysis
###ANOVA assumptions: normality and homogeneity of variance

#check normality of each treatment (normally distributed data)
Year2016 <- subset(ReturnMigrantsData, ReturnMigrantsData$Year == "2016") 
Year2017 <- subset(ReturnMigrantsData, ReturnMigrantsData$Year == "2017") 
Year2019 <- subset(ReturnMigrantsData, ReturnMigrantsData$Year == "2019") 


#Year2016
qqnorm(Year2016$Size) #normality plot
qqline(Year2016$Size)
shapiro.test(Year2016$Size) #Shapiro-Wilk test to check normality (null hypothesis = normal)
#normal (p=0.13)

#Year2017
qqnorm(Year2017$Size) #normality plot
qqline(Year2017$Size)
shapiro.test(Year2017$Size) #Shapiro-Wilk test to check normality (null hypothesis = normal)
#normal (p=0.08)

#Year2019
qqnorm(Year2019$Size) #normality plot
qqline(Year2019$Size)
shapiro.test(Year2019$Size) #Shapiro-Wilk test to check normality (null hypothesis = normal)
#cannot check becuase n=2, need to at least be 3

##check homogeneity of variance (equal variance among groups)
#first, load car package, then factor categories, run Levene's test
library(car)
fYear<- factor(ReturnMigrantsData$Year)

leveneTest(ReturnMigrantsData$Size ~fYear) #run levene's test (more robust than Bartlett's test)
#homogeneous (p=0.515)

##if both normality and homogeneity look good, we are ready to run t-test
#(homogeneity is more important than normality)
#If data fails assumptions, you would need to either transform the data (log transform, etc) or choose another statistical analysis to run (we won't worry about that now)

#now, let's run our t-test
ReturnMigrantsData$fYear<- as.factor(ReturnMigrantsData$Year) #make year a factor and add into dataframe
summary(ReturnMigrantsData$Size)
summary(Year2016$Size)
summary(Year2017$Size)
summary(Year2019$Size)


#Size_ttest<-t.test(ReturnMigrantsData$Size ~ ReturnMigrantsData$fYear, var.equal=TRUE) #p-value=0.6099 (not significant, null hypothesis = true)
#Size_ttest
#can't run t-test when including 2019 data. Need to run ANOVA

#instead run ANOVA
Size_aov<- aov(Size ~ fYear, data=ReturnMigrantsData)
summary(Size_aov)
TukeyHSD(Size_aov) 

mean(ReturnMigrantsData$Size, na.rm = TRUE) #156.0315
mean(Year2016$Size, na.rm = TRUE) #155.1
mean(Year2017$Size, na.rm = TRUE) #157.233
mean(Year2019$Size, na.rm = TRUE) #153.75

sd(Year2016$Size, na.rm = TRUE) #9.94
sd(Year2017$Size, na.rm = TRUE) #7.47
sd(Year2019$Size, na.rm = TRUE) #2.47
#boxplot
boxplot(ReturnMigrantsData$Size~fYear, xlab="Year", ylab="Total Length (cm)", main="Size of Tagged Individual", col=c("darkorchid1", "darkturquoise", "deeppink"))

        