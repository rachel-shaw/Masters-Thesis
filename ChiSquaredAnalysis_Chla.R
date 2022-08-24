library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(cowplot)
library(ggthemes)
library(grid)
library(readxl)

###Chla

#Available Chla all years
#ChlaAllData=read.csv("All_SPOTACT_Rerddap_ChlaPoly_Results201620172018.csv") #set working directory first to recognize file

#Chosen Chla all years
#All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("All_SPOTACT_BathySSTChlaSSS_Results.xlsx")

#Break data into lists for looping/iterations to fill matrix
ChosenChla<-c(211,184,92,35,20,13,5,7,5,3)
AvailChla<-c(190799,32559,9580,4560,2797,1725,1182,772,600,456)

WS1Chla<-c(12,12,5,2,0,1,0,0,0,0)
WS2Chla<-c(8,16,5,3,0,1,0,0,1,0)
WS3Chla<-c(5,6,0,3,0,0,0,0,0,0)
WS4Chla<-c(3,2,0,0,0,1,0,0,0,0)
WS5Chla<-c(4,2,2,0,0,0,0,0,0,0)
WS6Chla<-c(2,5,6,2,3,1,0,0,1,0)
WS7Chla<-c(9,8,1,0,1,0,0,0,0,0)
WS8Chla<-c(5,5,10,2,3,0,1,1,0,0)
WS9Chla<-c(14,25,11,5,4,0,2,0,2,0)
WS10Chla<-c(6,11,7,1,0,0,0,1,0,0)
WS11Chla<-c(33,15,7,4,3,2,0,2,0,1)
WS12Chla<-c(3,15,8,2,2,0,1,1,0,0)
WS13Chla<-c(1,13,12,4,1,1,1,0,0,0)
WS14Chla<-c(28,21,8,6,0,4,0,0,0,0)
WS15Chla<-c(20,13,6,1,1,2,0,1,1,0)
WS16Chla<-c(39,11,3,0,0,0,0,0,0,1)
WS17Chla<-c(0,0,0,0,0,0,0,0,0,0)
WS18Chla<-c(2,0,0,0,2,0,0,0,0,0)
WS20Chla<-c(3,4,1,0,0,0,0,0,0,0)
WS21Chla<-c(14,0,0,0,0,0,0,1,0,1)

#Chla bins + 20 sharks + total habitat + total avail habitat = 23 columns
#bins from 0-19 in 1.0 increments = 40 rows
ChlyaMatrix <- matrix(NA,10,23)

#Fill matrix with WS# Chla
ChlyaMatrix[,22] <- ChosenChla
ChlyaMatrix[,23] <- AvailChla

ChlyaMatrix[,2] <- WS1Chla
ChlyaMatrix[,3] <- WS2Chla
ChlyaMatrix[,4] <- WS3Chla
ChlyaMatrix[,5] <- WS4Chla
ChlyaMatrix[,6] <- WS5Chla
ChlyaMatrix[,7] <- WS6Chla
ChlyaMatrix[,8] <- WS7Chla
ChlyaMatrix[,9] <- WS8Chla
ChlyaMatrix[,10] <- WS9Chla
ChlyaMatrix[,11] <- WS10Chla
ChlyaMatrix[,12] <- WS11Chla
ChlyaMatrix[,13] <- WS12Chla
ChlyaMatrix[,14] <- WS13Chla
ChlyaMatrix[,15] <- WS14Chla
ChlyaMatrix[,16] <- WS15Chla
ChlyaMatrix[,17] <- WS16Chla
ChlyaMatrix[,18] <- WS17Chla
ChlyaMatrix[,19] <- WS18Chla
ChlyaMatrix[,20] <- WS20Chla
ChlyaMatrix[,21] <- WS21Chla

#fill with SSS bin intervals
ChlyaMatrix[,1] <- seq(0,18,2)

colnames(ChlyaMatrix) <- c("Chla_Bin", "WS1", "WS2","WS3", "WS4", "WS5", "WS6", "WS7", "WS8", "WS9", "WS10", "WS11",
                           "WS12", "WS13", "WS14", "WS15", "WS16", "WS17","WS18","WS20", "WS21","ChosenChla", "AvailChla")

#write.csv(ChlyaMatrix, file = "ChiSquared_Chla_2increment.csv")

#THEN GO INTO EXCEL AND ADD TOTALCOUNT ROW!!!! SAVE AGAIN AS EXCEL WORKBOOK!

###################################################################################################################
##USE LOG-LIKELIHOOD PACKAGE TO MAKE SURE EQUATION IS WORKING CORRECTLY IN NEXT STEP!

#get data
WS1Chla<-c(12,12,5,2,0,1,0,0,0,0)
WS2Chla<-c(8,16,5,3,0,1,0,0,1,0)
WS3Chla<-c(5,6,0,3,0,0,0,0,0,0)
WS4Chla<-c(3,2,0,0,0,1,0,0,0,0)
WS5Chla<-c(4,2,2,0,0,0,0,0,0,0)
WS6Chla<-c(2,5,6,2,3,1,0,0,1,0)
WS7Chla<-c(9,8,1,0,1,0,0,0,0,0)
WS8Chla<-c(5,5,10,2,3,0,1,1,0,0)
WS9Chla<-c(14,25,11,5,4,0,2,0,2,0)
WS10Chla<-c(6,11,7,1,0,0,0,1,0,0)
WS11Chla<-c(33,15,7,4,3,2,0,2,0,1)
WS12Chla<-c(3,15,8,2,2,0,1,1,0,0)
WS13Chla<-c(1,13,12,4,1,1,1,0,0,0)
WS14Chla<-c(28,21,8,6,0,4,0,0,0,0)
WS15Chla<-c(20,13,6,1,1,2,0,1,1,0)
WS16Chla<-c(39,11,3,0,0,0,0,0,0,1)
WS17Chla<-c(0,0,0,0,0,0,0,0,0,0)
WS18Chla<-c(2,0,0,0,2,0,0,0,0,0)
WS20Chla<-c(3,4,1,0,0,0,0,0,0,0)
WS21Chla<-c(14,0,0,0,0,0,0,1,0,1)

#number of fish = 18 columns
#bins from 0-19 in 1.0 increments = 40 rows
ChlaMatrix <- matrix(NA,10,20)

ChlaMatrix[,1] <- WS1Chla
ChlaMatrix[,2] <- WS2Chla
ChlaMatrix[,3] <- WS3Chla
ChlaMatrix[,4] <- WS4Chla
ChlaMatrix[,5] <- WS5Chla
ChlaMatrix[,6] <- WS6Chla
ChlaMatrix[,7] <- WS7Chla
ChlaMatrix[,8] <- WS8Chla
ChlaMatrix[,9] <- WS9Chla
ChlaMatrix[,10] <- WS10Chla
ChlaMatrix[,11] <- WS11Chla
ChlaMatrix[,12] <- WS12Chla
ChlaMatrix[,13] <- WS13Chla
ChlaMatrix[,14] <- WS14Chla
ChlaMatrix[,15] <- WS15Chla
ChlaMatrix[,16] <- WS16Chla
ChlaMatrix[,17] <- WS17Chla
ChlaMatrix[,18] <- WS18Chla
ChlaMatrix[,19] <- WS20Chla
ChlaMatrix[,20] <- WS21Chla

library(rJava)
library(Deducer)
likelihood.test(ChlaMatrix) # p-value = 6.332e-06   log-likelihood stat = 264.06

#######################################################################################
#PART 1: EQUATION 14.1

#base equation: 
#(x[i,j]*(log(x[i,j]/((y*k)/a)))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- ChlaMatrix #u.ij
totalhab <- c(211,184,92,35,20,13,5,7,5,3) #u.i+
totalcount <- c(32,34,14,6,8,20,19,27,63,26,67,32,33,67,45,54,0,4,8,16) #u.+j
totalhaball <- 575 #u.++

part1<-0
part1<-capture.output(for(i in 1:nrow(count)){
  for(j in 1:ncol(count)){
    xvar <- count[i,j]
    yvar <- totalhab[i]
    part1 <- ((((xvar*(log(xvar/(((yvar)*totalcount[j])/totalhaball)))))))
    cat(part1, "\n")
  }
})

part1<-as.numeric(part1)
part1
part1<-sum(part1, na.rm=TRUE)
part1*2 #=264.0624 IT WORKED CORRECTLY!!!!!!!!!!!!!!

######################################
#PART 2: USE EQUATION 14.2

#base equation: 
#(x[i,j]*(log(x[i,j]/((y/a)*k))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- ChlaMatrix #u.ij
availhab <- c(190799,32559,9580,4560,2797,1725,1182,772,600,456) #pie.i when divided by availhaball
totalcount <- c(32,34,14,6,8,20,19,27,63,26,67,32,33,67,45,54,0,4,8,16) #u.+j
availhaball <- 245030

part2<-0
part2<-capture.output(for(i in 1:nrow(count)){
  for(j in 1:ncol(count)){
    xvar <- count[i,j]
    yvar <- availhab[i]
    part2 <- ((((xvar*(log(xvar/(((yvar/availhaball)*totalcount[j]))))))))
    cat(part2, "\n")
  }
})

part2<-as.numeric(part2)
part2
part2<-sum(part2, na.rm=TRUE)
part2*2 #=730.8104

####################################################
##NOW TAKE THE DIFFERENCE OF EQUATION 14.2-14.1 TO SEE IF SELECTION IS OCCURRING
730.8104-264.0624 #diff of stat = 466.748

#degrees of freedom
#part 1 df = (habitat categories-1)*(number of sharks-1)
(10-1)*(20-1) #171

#part 2 df = sum(habitat categories - 1) from each shark
(10-1)*20 #180

180-171 #=9 df

#look up value on chi-squared table to determine if selection is occurring

#part 1: The P-Value is < .00001. The result is significant at p < .05.
#part 2: The P-Value is < .00001. The result is significant at p < .05
#part 2-part 1: The P-Value is < .00001. The result is significant at p < .05.

#yes, selection is occurring!

############################################################
############################################
#PART 4: FIND SELECTION RATIO FOR WHOLE POPULATION

#try using loop

#base equation:
#w.i= totalhab/((availhab/availhaball)*totalhaball)

#data
availhab <- c(190799,32559,9580,4560,2797,1725,1182,772,600,456) #pie.i when divided by availhaball
availhaball <- 245030
totalhaball <- 575 #u.++
ChosenChla<-c(211,184,92,35,20,13,5,7,5,3)

#loop
SR<-0
SR<-capture.output(for(i in 1:length(ChosenChla)){
  xvar <- ChosenChla[i]
  yvar <- availhab[i]
  SR <- (xvar/((yvar/availhaball)*totalhaball))
  cat(SR, "\n")
})

SR<-as.numeric(SR)
SR ##IT WORKS!!!!! WOOT WOOT!!!

####################################################################
#PART 5: FIND STANDARD ERROR (SE)

#base equation:
#SE(w.i) = sqrt((n/((n-1)*(u.++)^2)*sum(((u.ij/pie.i)-w.i(u.+j))^2)))

###NEED TO CHANGE Hab1_count for EVERY LEVEL OF HABITAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! and switch availhab[1] and SR[1] to whatever level you're on

#Chla0.0
totalcount <- c(32,34,14,6,8,20,19,27,63,26,67,32,33,67,45,54,0,4,8,16) #u.+j
availhaball <- 245030
availhab <- c(190799,32559,9580,4560,2797,1725,1182,772,600,456) #pie.i when divided by availhaball
SR<-c(0.471257,2.408231,4.092359,3.270805,3.047116,3.211483,1.802619,3.863956,3.551159,2.803547)

hab1_count <- c(12,8,5,3,4,2,9,5,14,6,33,3,1,28,20,39,0,2,3,14) #u.ij

sum_hab1<-(((hab1_count[1]/(availhab[1]/availhaball))-(SR[1]*totalcount[1]))^2)+
  (((hab1_count[2]/(availhab[1]/availhaball))-(SR[1]*totalcount[2]))^2)+
  (((hab1_count[3]/(availhab[1]/availhaball))-(SR[1]*totalcount[3]))^2)+
  (((hab1_count[4]/(availhab[1]/availhaball))-(SR[1]*totalcount[4]))^2)+
  (((hab1_count[5]/(availhab[1]/availhaball))-(SR[1]*totalcount[5]))^2)+
  (((hab1_count[6]/(availhab[1]/availhaball))-(SR[1]*totalcount[6]))^2)+
  (((hab1_count[7]/(availhab[1]/availhaball))-(SR[1]*totalcount[7]))^2)+
  (((hab1_count[8]/(availhab[1]/availhaball))-(SR[1]*totalcount[8]))^2)+
  (((hab1_count[9]/(availhab[1]/availhaball))-(SR[1]*totalcount[9]))^2)+
  (((hab1_count[10]/(availhab[1]/availhaball))-(SR[1]*totalcount[10]))^2)+
  (((hab1_count[11]/(availhab[1]/availhaball))-(SR[1]*totalcount[11]))^2)+
  (((hab1_count[12]/(availhab[1]/availhaball))-(SR[1]*totalcount[12]))^2)+
  (((hab1_count[13]/(availhab[1]/availhaball))-(SR[1]*totalcount[13]))^2)+
  (((hab1_count[14]/(availhab[1]/availhaball))-(SR[1]*totalcount[14]))^2)+
  (((hab1_count[15]/(availhab[1]/availhaball))-(SR[1]*totalcount[15]))^2)+
  (((hab1_count[16]/(availhab[1]/availhaball))-(SR[1]*totalcount[16]))^2)+
  (((hab1_count[17]/(availhab[1]/availhaball))-(SR[1]*totalcount[17]))^2)+
  (((hab1_count[18]/(availhab[1]/availhaball))-(SR[1]*totalcount[18]))^2)+
  (((hab1_count[19]/(availhab[1]/availhaball))-(SR[1]*totalcount[19]))^2)+  
  (((hab1_count[20]/(availhab[1]/availhaball))-(SR[1]*totalcount[20]))^2)

#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab1)) #=

#Chla2
hab2_count <- c(12,16,6,2,2,5,8,5,25,11,15,15,13,21,13,11,0,0,4,0) #u.ij

sum_hab2<-(((hab2_count[1]/(availhab[2]/availhaball))-(SR[2]*totalcount[1]))^2)+
  (((hab2_count[2]/(availhab[2]/availhaball))-(SR[2]*totalcount[2]))^2)+
  (((hab2_count[3]/(availhab[2]/availhaball))-(SR[2]*totalcount[3]))^2)+
  (((hab2_count[4]/(availhab[2]/availhaball))-(SR[2]*totalcount[4]))^2)+
  (((hab2_count[5]/(availhab[2]/availhaball))-(SR[2]*totalcount[5]))^2)+
  (((hab2_count[6]/(availhab[2]/availhaball))-(SR[2]*totalcount[6]))^2)+
  (((hab2_count[7]/(availhab[2]/availhaball))-(SR[2]*totalcount[7]))^2)+
  (((hab2_count[8]/(availhab[2]/availhaball))-(SR[2]*totalcount[8]))^2)+
  (((hab2_count[9]/(availhab[2]/availhaball))-(SR[2]*totalcount[9]))^2)+
  (((hab2_count[10]/(availhab[2]/availhaball))-(SR[2]*totalcount[10]))^2)+
  (((hab2_count[11]/(availhab[2]/availhaball))-(SR[2]*totalcount[11]))^2)+
  (((hab2_count[12]/(availhab[2]/availhaball))-(SR[2]*totalcount[12]))^2)+
  (((hab2_count[13]/(availhab[2]/availhaball))-(SR[2]*totalcount[13]))^2)+
  (((hab2_count[14]/(availhab[2]/availhaball))-(SR[2]*totalcount[14]))^2)+
  (((hab2_count[15]/(availhab[2]/availhaball))-(SR[2]*totalcount[15]))^2)+
  (((hab2_count[16]/(availhab[2]/availhaball))-(SR[2]*totalcount[16]))^2)+
  (((hab2_count[17]/(availhab[2]/availhaball))-(SR[2]*totalcount[17]))^2)+
  (((hab2_count[18]/(availhab[2]/availhaball))-(SR[2]*totalcount[18]))^2)+
  (((hab2_count[19]/(availhab[2]/availhaball))-(SR[2]*totalcount[19]))^2)+
  (((hab2_count[20]/(availhab[2]/availhaball))-(SR[2]*totalcount[20]))^2)

#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab2)) #=

#Chla4
hab3_count <- c(5,5,0,0,2,6,1,10,11,7,7,8,12,8,6,3,0,0,1,0) #u.ij

sum_hab3<-(((hab3_count[1]/(availhab[3]/availhaball))-(SR[3]*totalcount[1]))^2)+
  (((hab3_count[2]/(availhab[3]/availhaball))-(SR[3]*totalcount[2]))^2)+
  (((hab3_count[3]/(availhab[3]/availhaball))-(SR[3]*totalcount[3]))^2)+
  (((hab3_count[4]/(availhab[3]/availhaball))-(SR[3]*totalcount[4]))^2)+
  (((hab3_count[5]/(availhab[3]/availhaball))-(SR[3]*totalcount[5]))^2)+
  (((hab3_count[6]/(availhab[3]/availhaball))-(SR[3]*totalcount[6]))^2)+
  (((hab3_count[7]/(availhab[3]/availhaball))-(SR[3]*totalcount[7]))^2)+
  (((hab3_count[8]/(availhab[3]/availhaball))-(SR[3]*totalcount[8]))^2)+
  (((hab3_count[9]/(availhab[3]/availhaball))-(SR[3]*totalcount[9]))^2)+
  (((hab3_count[10]/(availhab[3]/availhaball))-(SR[3]*totalcount[10]))^2)+
  (((hab3_count[11]/(availhab[3]/availhaball))-(SR[3]*totalcount[11]))^2)+
  (((hab3_count[12]/(availhab[3]/availhaball))-(SR[3]*totalcount[12]))^2)+
  (((hab3_count[13]/(availhab[3]/availhaball))-(SR[3]*totalcount[13]))^2)+
  (((hab3_count[14]/(availhab[3]/availhaball))-(SR[3]*totalcount[14]))^2)+
  (((hab3_count[15]/(availhab[3]/availhaball))-(SR[3]*totalcount[15]))^2)+
  (((hab3_count[16]/(availhab[3]/availhaball))-(SR[3]*totalcount[16]))^2)+
  (((hab3_count[17]/(availhab[3]/availhaball))-(SR[3]*totalcount[17]))^2)+
  (((hab3_count[18]/(availhab[3]/availhaball))-(SR[3]*totalcount[18]))^2)+
  (((hab3_count[19]/(availhab[3]/availhaball))-(SR[3]*totalcount[19]))^2)+  
  (((hab3_count[20]/(availhab[3]/availhaball))-(SR[3]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab3)) #=


#Chla6
hab4_count <- c(2,3,3,0,0,2,0,2,5,1,4,2,4,6,1,0,0,0,0,0) #u.ij

sum_hab4<-(((hab4_count[1]/(availhab[4]/availhaball))-(SR[4]*totalcount[1]))^2)+
  (((hab4_count[2]/(availhab[4]/availhaball))-(SR[4]*totalcount[2]))^2)+
  (((hab4_count[3]/(availhab[4]/availhaball))-(SR[4]*totalcount[3]))^2)+
  (((hab4_count[4]/(availhab[4]/availhaball))-(SR[4]*totalcount[4]))^2)+
  (((hab4_count[5]/(availhab[4]/availhaball))-(SR[4]*totalcount[5]))^2)+
  (((hab4_count[6]/(availhab[4]/availhaball))-(SR[4]*totalcount[6]))^2)+
  (((hab4_count[7]/(availhab[4]/availhaball))-(SR[4]*totalcount[7]))^2)+
  (((hab4_count[8]/(availhab[4]/availhaball))-(SR[4]*totalcount[8]))^2)+
  (((hab4_count[9]/(availhab[4]/availhaball))-(SR[4]*totalcount[9]))^2)+
  (((hab4_count[10]/(availhab[4]/availhaball))-(SR[4]*totalcount[10]))^2)+
  (((hab4_count[11]/(availhab[4]/availhaball))-(SR[4]*totalcount[11]))^2)+
  (((hab4_count[12]/(availhab[4]/availhaball))-(SR[4]*totalcount[12]))^2)+
  (((hab4_count[13]/(availhab[4]/availhaball))-(SR[4]*totalcount[13]))^2)+
  (((hab4_count[14]/(availhab[4]/availhaball))-(SR[4]*totalcount[14]))^2)+
  (((hab4_count[15]/(availhab[4]/availhaball))-(SR[4]*totalcount[15]))^2)+
  (((hab4_count[16]/(availhab[4]/availhaball))-(SR[4]*totalcount[16]))^2)+
  (((hab4_count[17]/(availhab[4]/availhaball))-(SR[4]*totalcount[17]))^2)+
  (((hab4_count[18]/(availhab[4]/availhaball))-(SR[4]*totalcount[18]))^2)+
  (((hab4_count[19]/(availhab[4]/availhaball))-(SR[4]*totalcount[19]))^2)+
  (((hab4_count[20]/(availhab[4]/availhaball))-(SR[4]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab4)) #=

#Chla8
hab5_count <- c(0,0,0,0,0,3,1,3,4,0,3,2,1,0,1,0,0,2,0,0) #u.ij

sum_hab5<-(((hab5_count[1]/(availhab[5]/availhaball))-(SR[5]*totalcount[1]))^2)+
  (((hab5_count[2]/(availhab[5]/availhaball))-(SR[5]*totalcount[2]))^2)+
  (((hab5_count[3]/(availhab[5]/availhaball))-(SR[5]*totalcount[3]))^2)+
  (((hab5_count[4]/(availhab[5]/availhaball))-(SR[5]*totalcount[4]))^2)+
  (((hab5_count[5]/(availhab[5]/availhaball))-(SR[5]*totalcount[5]))^2)+
  (((hab5_count[6]/(availhab[5]/availhaball))-(SR[5]*totalcount[6]))^2)+
  (((hab5_count[7]/(availhab[5]/availhaball))-(SR[5]*totalcount[7]))^2)+
  (((hab5_count[8]/(availhab[5]/availhaball))-(SR[5]*totalcount[8]))^2)+
  (((hab5_count[9]/(availhab[5]/availhaball))-(SR[5]*totalcount[9]))^2)+
  (((hab5_count[10]/(availhab[5]/availhaball))-(SR[5]*totalcount[10]))^2)+
  (((hab5_count[11]/(availhab[5]/availhaball))-(SR[5]*totalcount[11]))^2)+
  (((hab5_count[12]/(availhab[5]/availhaball))-(SR[5]*totalcount[12]))^2)+
  (((hab5_count[13]/(availhab[5]/availhaball))-(SR[5]*totalcount[13]))^2)+
  (((hab5_count[14]/(availhab[5]/availhaball))-(SR[5]*totalcount[14]))^2)+
  (((hab5_count[15]/(availhab[5]/availhaball))-(SR[5]*totalcount[15]))^2)+
  (((hab5_count[16]/(availhab[5]/availhaball))-(SR[5]*totalcount[16]))^2)+
  (((hab5_count[17]/(availhab[5]/availhaball))-(SR[5]*totalcount[17]))^2)+
  (((hab5_count[18]/(availhab[5]/availhaball))-(SR[5]*totalcount[18]))^2)+
  (((hab5_count[19]/(availhab[5]/availhaball))-(SR[5]*totalcount[19]))^2)+ 
  (((hab5_count[20]/(availhab[5]/availhaball))-(SR[5]*totalcount[20]))^2)

#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab5)) #=


#Chla10
hab6_count <- c(1,1,0,1,0,1,0,0,0,0,2,0,1,4,2,0,0,0,0,0) #u.ij

sum_hab6<-(((hab6_count[1]/(availhab[6]/availhaball))-(SR[6]*totalcount[1]))^2)+
  (((hab6_count[2]/(availhab[6]/availhaball))-(SR[6]*totalcount[2]))^2)+
  (((hab6_count[3]/(availhab[6]/availhaball))-(SR[6]*totalcount[3]))^2)+
  (((hab6_count[4]/(availhab[6]/availhaball))-(SR[6]*totalcount[4]))^2)+
  (((hab6_count[5]/(availhab[6]/availhaball))-(SR[6]*totalcount[5]))^2)+
  (((hab6_count[6]/(availhab[6]/availhaball))-(SR[6]*totalcount[6]))^2)+
  (((hab6_count[7]/(availhab[6]/availhaball))-(SR[6]*totalcount[7]))^2)+
  (((hab6_count[8]/(availhab[6]/availhaball))-(SR[6]*totalcount[8]))^2)+
  (((hab6_count[9]/(availhab[6]/availhaball))-(SR[6]*totalcount[9]))^2)+
  (((hab6_count[10]/(availhab[6]/availhaball))-(SR[6]*totalcount[10]))^2)+
  (((hab6_count[11]/(availhab[6]/availhaball))-(SR[6]*totalcount[11]))^2)+
  (((hab6_count[12]/(availhab[6]/availhaball))-(SR[6]*totalcount[12]))^2)+
  (((hab6_count[13]/(availhab[6]/availhaball))-(SR[6]*totalcount[13]))^2)+
  (((hab6_count[14]/(availhab[6]/availhaball))-(SR[6]*totalcount[14]))^2)+
  (((hab6_count[15]/(availhab[6]/availhaball))-(SR[6]*totalcount[15]))^2)+
  (((hab6_count[16]/(availhab[6]/availhaball))-(SR[6]*totalcount[16]))^2)+
  (((hab6_count[17]/(availhab[6]/availhaball))-(SR[6]*totalcount[17]))^2)+
  (((hab6_count[18]/(availhab[6]/availhaball))-(SR[6]*totalcount[18]))^2)+
  (((hab6_count[19]/(availhab[6]/availhaball))-(SR[6]*totalcount[19]))^2)+  
  (((hab6_count[20]/(availhab[6]/availhaball))-(SR[6]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab6)) #=0.9438925


#Chla12
hab7_count <- c(0,0,0,0,0,0,0,1,2,0,0,1,1,0,0,0,0,0,0,0) #u.ij

sum_hab7<-(((hab7_count[1]/(availhab[7]/availhaball))-(SR[7]*totalcount[1]))^2)+
  (((hab7_count[2]/(availhab[7]/availhaball))-(SR[7]*totalcount[2]))^2)+
  (((hab7_count[3]/(availhab[7]/availhaball))-(SR[7]*totalcount[3]))^2)+
  (((hab7_count[4]/(availhab[7]/availhaball))-(SR[7]*totalcount[4]))^2)+
  (((hab7_count[5]/(availhab[7]/availhaball))-(SR[7]*totalcount[5]))^2)+
  (((hab7_count[6]/(availhab[7]/availhaball))-(SR[7]*totalcount[6]))^2)+
  (((hab7_count[7]/(availhab[7]/availhaball))-(SR[7]*totalcount[7]))^2)+
  (((hab7_count[8]/(availhab[7]/availhaball))-(SR[7]*totalcount[8]))^2)+
  (((hab7_count[9]/(availhab[7]/availhaball))-(SR[7]*totalcount[9]))^2)+
  (((hab7_count[10]/(availhab[7]/availhaball))-(SR[7]*totalcount[10]))^2)+
  (((hab7_count[11]/(availhab[7]/availhaball))-(SR[7]*totalcount[11]))^2)+
  (((hab7_count[12]/(availhab[7]/availhaball))-(SR[7]*totalcount[12]))^2)+
  (((hab7_count[13]/(availhab[7]/availhaball))-(SR[7]*totalcount[13]))^2)+
  (((hab7_count[14]/(availhab[7]/availhaball))-(SR[7]*totalcount[14]))^2)+
  (((hab7_count[15]/(availhab[7]/availhaball))-(SR[7]*totalcount[15]))^2)+
  (((hab7_count[16]/(availhab[7]/availhaball))-(SR[7]*totalcount[16]))^2)+
  (((hab7_count[17]/(availhab[7]/availhaball))-(SR[7]*totalcount[17]))^2)+
  (((hab7_count[18]/(availhab[7]/availhaball))-(SR[7]*totalcount[18]))^2)+
  (((hab7_count[19]/(availhab[7]/availhaball))-(SR[7]*totalcount[19]))^2)+
  (((hab7_count[20]/(availhab[7]/availhaball))-(SR[7]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab7)) #=

#Chla14
hab8_count <- c(0,0,0,0,0,0,0,1,0,1,2,1,0,0,1,0,0,0,0,1) #u.ij

sum_hab8<-(((hab8_count[1]/(availhab[8]/availhaball))-(SR[8]*totalcount[1]))^2)+
  (((hab8_count[2]/(availhab[8]/availhaball))-(SR[8]*totalcount[2]))^2)+
  (((hab8_count[3]/(availhab[8]/availhaball))-(SR[8]*totalcount[3]))^2)+
  (((hab8_count[4]/(availhab[8]/availhaball))-(SR[8]*totalcount[4]))^2)+
  (((hab8_count[5]/(availhab[8]/availhaball))-(SR[8]*totalcount[5]))^2)+
  (((hab8_count[6]/(availhab[8]/availhaball))-(SR[8]*totalcount[6]))^2)+
  (((hab8_count[7]/(availhab[8]/availhaball))-(SR[8]*totalcount[7]))^2)+
  (((hab8_count[8]/(availhab[8]/availhaball))-(SR[8]*totalcount[8]))^2)+
  (((hab8_count[9]/(availhab[8]/availhaball))-(SR[8]*totalcount[9]))^2)+
  (((hab8_count[10]/(availhab[8]/availhaball))-(SR[8]*totalcount[10]))^2)+
  (((hab8_count[11]/(availhab[8]/availhaball))-(SR[8]*totalcount[11]))^2)+
  (((hab8_count[12]/(availhab[8]/availhaball))-(SR[8]*totalcount[12]))^2)+
  (((hab8_count[13]/(availhab[8]/availhaball))-(SR[8]*totalcount[13]))^2)+
  (((hab8_count[14]/(availhab[8]/availhaball))-(SR[8]*totalcount[14]))^2)+
  (((hab8_count[15]/(availhab[8]/availhaball))-(SR[8]*totalcount[15]))^2)+
  (((hab8_count[16]/(availhab[8]/availhaball))-(SR[8]*totalcount[16]))^2)+
  (((hab8_count[17]/(availhab[8]/availhaball))-(SR[8]*totalcount[17]))^2)+
  (((hab8_count[18]/(availhab[8]/availhaball))-(SR[8]*totalcount[18]))^2)+
  (((hab8_count[19]/(availhab[8]/availhaball))-(SR[8]*totalcount[19]))^2)+ 
  (((hab8_count[20]/(availhab[8]/availhaball))-(SR[8]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab8)) #=1.373823


#Chla16
hab9_count <- c(0,1,0,0,0,1,0,0,2,0,0,0,0,0,1,0,0,0,0,0) #u.ij

sum_hab9<-(((hab9_count[1]/(availhab[9]/availhaball))-(SR[9]*totalcount[1]))^2)+
  (((hab9_count[2]/(availhab[9]/availhaball))-(SR[9]*totalcount[2]))^2)+
  (((hab9_count[3]/(availhab[9]/availhaball))-(SR[9]*totalcount[3]))^2)+
  (((hab9_count[4]/(availhab[9]/availhaball))-(SR[9]*totalcount[4]))^2)+
  (((hab9_count[5]/(availhab[9]/availhaball))-(SR[9]*totalcount[5]))^2)+
  (((hab9_count[6]/(availhab[9]/availhaball))-(SR[9]*totalcount[6]))^2)+
  (((hab9_count[7]/(availhab[9]/availhaball))-(SR[9]*totalcount[7]))^2)+
  (((hab9_count[8]/(availhab[9]/availhaball))-(SR[9]*totalcount[8]))^2)+
  (((hab9_count[9]/(availhab[9]/availhaball))-(SR[9]*totalcount[9]))^2)+
  (((hab9_count[10]/(availhab[9]/availhaball))-(SR[9]*totalcount[10]))^2)+
  (((hab9_count[11]/(availhab[9]/availhaball))-(SR[9]*totalcount[11]))^2)+
  (((hab9_count[12]/(availhab[9]/availhaball))-(SR[9]*totalcount[12]))^2)+
  (((hab9_count[13]/(availhab[9]/availhaball))-(SR[9]*totalcount[13]))^2)+
  (((hab9_count[14]/(availhab[9]/availhaball))-(SR[9]*totalcount[14]))^2)+
  (((hab9_count[15]/(availhab[9]/availhaball))-(SR[9]*totalcount[15]))^2)+
  (((hab9_count[16]/(availhab[9]/availhaball))-(SR[9]*totalcount[16]))^2)+
  (((hab9_count[17]/(availhab[9]/availhaball))-(SR[9]*totalcount[17]))^2)+
  (((hab9_count[18]/(availhab[9]/availhaball))-(SR[9]*totalcount[18]))^2)+
  (((hab9_count[19]/(availhab[9]/availhaball))-(SR[9]*totalcount[19]))^2)+
  (((hab9_count[20]/(availhab[9]/availhaball))-(SR[9]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab9)) #=

#Chla18
hab10_count <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1) #u.ij

sum_hab10<-(((hab10_count[1]/(availhab[10]/availhaball))-(SR[10]*totalcount[1]))^2)+
  (((hab10_count[2]/(availhab[10]/availhaball))-(SR[10]*totalcount[2]))^2)+
  (((hab10_count[3]/(availhab[10]/availhaball))-(SR[10]*totalcount[3]))^2)+
  (((hab10_count[4]/(availhab[10]/availhaball))-(SR[10]*totalcount[4]))^2)+
  (((hab10_count[5]/(availhab[10]/availhaball))-(SR[10]*totalcount[5]))^2)+
  (((hab10_count[6]/(availhab[10]/availhaball))-(SR[10]*totalcount[6]))^2)+
  (((hab10_count[7]/(availhab[10]/availhaball))-(SR[10]*totalcount[7]))^2)+
  (((hab10_count[8]/(availhab[10]/availhaball))-(SR[10]*totalcount[8]))^2)+
  (((hab10_count[9]/(availhab[10]/availhaball))-(SR[10]*totalcount[9]))^2)+
  (((hab10_count[10]/(availhab[10]/availhaball))-(SR[10]*totalcount[10]))^2)+
  (((hab10_count[11]/(availhab[10]/availhaball))-(SR[10]*totalcount[11]))^2)+
  (((hab10_count[12]/(availhab[10]/availhaball))-(SR[10]*totalcount[12]))^2)+
  (((hab10_count[13]/(availhab[10]/availhaball))-(SR[10]*totalcount[13]))^2)+
  (((hab10_count[14]/(availhab[10]/availhaball))-(SR[10]*totalcount[14]))^2)+
  (((hab10_count[15]/(availhab[10]/availhaball))-(SR[10]*totalcount[15]))^2)+
  (((hab10_count[16]/(availhab[10]/availhaball))-(SR[10]*totalcount[16]))^2)+
  (((hab10_count[17]/(availhab[10]/availhaball))-(SR[10]*totalcount[17]))^2)+
  (((hab10_count[18]/(availhab[10]/availhaball))-(SR[10]*totalcount[18]))^2)+
  (((hab10_count[19]/(availhab[10]/availhaball))-(SR[10]*totalcount[19]))^2)+
  (((hab10_count[20]/(availhab[10]/availhaball))-(SR[10]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*575^2))*(sum_hab10)) #=


######################################################################
#PART 6: CONFIDENCE INTERVALS!

#use upper-tailed test z score to get CIs
#base equation:
#SelectionRatio[i] +/- z(0.10/(2*number of habitats))*SE
#0.10 is used for 90% CIs
#in this example, we are looking at 5 habitats

##for Roger&White example, Z-score = 2.326 based on the table for upper-tailed z score test
#this will  change based on how many habitats you are looking at in other studies
#0.10/(2*number of habitats)
0.10/(2*10) #find z score based on this number
#z score for chla = 2.5758


#for Chla, z score = 2.5758
SelRatio<-c(0.471257,2.408231,4.092359,3.270805,3.047116,3.211483,1.802619,3.863956,3.551159,2.803547)
StandErr<-c(0.06886402,0.2005484,0.6034241,0.545478,0.8723102,0.9438925,0.8351209,1.373823,1.625424,1.438315)

#for Chla, z score = 2.5758
#CI for Chla0.0
#use loop
SelRatio[1]+(2.5758*StandErr[1]) #=  for upper CI
SelRatio[1]-(2.5758*StandErr[1]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla2
SelRatio[2]+(2.5758*StandErr[2]) #=  for upper CI
SelRatio[2]-(2.5758*StandErr[2]) #= for lower CI (can't observe neg value, so change to 0)

#CI for Chla4
SelRatio[3]+(2.5758*StandErr[3]) #=  for upper CI
SelRatio[3]-(2.5758*StandErr[3]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla6
SelRatio[4]+(2.5758*StandErr[4]) #=  for upper CI
SelRatio[4]-(2.5758*StandErr[4]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla8
SelRatio[5]+(2.5758*StandErr[5]) #=  for upper CI
SelRatio[5]-(2.5758*StandErr[5]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla10
SelRatio[6]+(2.5758*StandErr[6]) #=  for upper CI
SelRatio[6]-(2.5758*StandErr[6]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla12
SelRatio[7]+(2.5758*StandErr[7]) #=  for upper CI
SelRatio[7]-(2.5758*StandErr[7]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla14
SelRatio[8]+(2.5758*StandErr[8]) #=  for upper CI
SelRatio[8]-(2.5758*StandErr[8]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla16
SelRatio[9]+(2.5758*StandErr[9]) #=  for upper CI
SelRatio[9]-(2.5758*StandErr[9]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for Chla18
SelRatio[10]+(2.5758*StandErr[10]) #=  for upper CI
SelRatio[10]-(2.5758*StandErr[10]) #=  for lower CI (can't observe neg value, so change to 0)

#########################################################################
####CHL A PLOT BOXPLOT WITH 90% CIs

SR<-c(0.471257,2.408231,4.092359,3.270805,3.047116,3.211483,1.802619,3.863956,3.551159,2.803547)
CI_up<- c(0.6486369,2.924804,5.646659,4.675847,5.294013,5.642761,3.953723,7.402649,7.737926,6.508359)
CI_low<-c(0.2938771,1.891658,2.538059,1.865763,0.8002194,0.7802047,0,0.3252627,0,0)
Chla_bin<- seq(0,18,2)

Chla_Plot_Matrix <- matrix(NA,10,4)

Chla_Plot_Matrix[,1] <- Chla_bin
Chla_Plot_Matrix[,2] <- SR
Chla_Plot_Matrix[,3] <- CI_up
Chla_Plot_Matrix[,4] <- CI_low

colnames(Chla_Plot_Matrix)<-c("bin", "SelRatio", "CI_upper", "CI_lower")

Chla_df<- as.data.frame(Chla_Plot_Matrix)

#par(mar=c(5,4,4,2)+0.1) #normal
#par(mar=c(5.5,4,4,2)+0.1) #give more margin space for x-axis

plot(Chla_df$bin, Chla_df$SelRatio, ylim=c(0, 10), xlim=c(0, 18), xlab="", ylab="Selection Ratio", main="", pch=20, xaxt="n", cex=1.8, cex.lab=1.45)
axis(1, at=seq(0, 18, 2), labels=rep("", each=10))
text(Chla_df$bin, par("usr")[3]-0.5, srt = 60, adj= 1, xpd = TRUE, labels = c("0-2.0", "2.0-4.0", "4.0-6.0",
                                                                              "6.0-8.0", "8.0-10.0", "10.0-12.0", 
                                                                              "12.0-14.0", "14.0-16.0", "16.0-18.0", 
                                                                              "18.0-20.0") , cex=.95)
mtext("Chlorophyll a (mg^m-3)", side=1, line=4, cex=1.45)
abline(h=1, lty=2, col="red")
arrows(Chla_df$bin, Chla_df$CI_upper, Chla_df$bin, Chla_df$CI_lower, length=0.05, angle=90, code=3, col = 'black')

