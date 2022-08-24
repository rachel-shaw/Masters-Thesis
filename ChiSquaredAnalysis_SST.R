library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(cowplot)
library(ggthemes)
library(grid)
library(readxl)


###SST

#Available SST all years
#SSTAllData=read.csv("All_SPOTACT_Rerddap_SSTPoly_Results201620172018.csv") #set working directory first to recognize file

#Chosen SST all years
#All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("All_SPOTACT_BathySSTChlaSSS_Results.xlsx")

#Break,data,into,lists,for,looping/iterations,to,fill,matrix
ChosenSST<-c(0,1,51,285,454,296,128,8,0)
AvailSST<-c(5126,181857,1121882,1997840,3067960,3159870,2414120,912603,16390)

WS1SST<-c(0,0,0,24,19,23,0,0,0)
WS2SST<-c(0,0,10,16,23,10,4,0,0)
WS3SST<-c(0,0,0,0,12,19,0,0,0)
WS4SST<-c(0,0,0,0,0,7,0,0,0)
WS5SST<-c(0,0,0,0,7,5,3,0,0)
WS6SST<-c(0,0,0,9,9,22,4,0,0)
WS7SST<-c(0,0,1,11,19,13,0,0,0)
WS8SST<-c(0,1,10,19,6,26,5,0,0)
WS9SST<-c(0,0,1,32,54,32,27,4,0)
WS10SST<-c(0,0,0,0,31,16,0,0,0)
WS11SST<-c(0,0,10,25,59,39,36,0,0)
WS12SST<-c(0,0,0,30,16,5,6,0,0)
WS13SST<-c(0,0,0,49,16,0,0,0,0)
WS14SST<-c(0,0,13,36,76,5,0,0,0)
WS15SST<-c(0,0,6,16,42,22,11,0,0)
WS16SST<-c(0,0,0,17,29,39,30,4,0)
WS17SST<-c(0,0,0,1,0,0,0,0,0)
WS18SST<-c(0,0,0,0,9,0,0,0,0)
WS20SST<-c(0,0,0,0,8,7,0,0,0)
WS21SST<-c(0,0,0,0,19,6,2,0,0)

#SST bins + 20 sharks + total habitat + total avail habitat = 23 columns
#bins from 12-29.5 in 0.5 increments = 35 rows
TempMatrix <- matrix(NA, 9,23)

#Fill matrix with WS# SST
TempMatrix[,22] <- ChosenSST
TempMatrix[,23] <- AvailSST

TempMatrix[,2] <- WS1SST
TempMatrix[,3] <- WS2SST
TempMatrix[,4] <- WS3SST
TempMatrix[,5] <- WS4SST
TempMatrix[,6] <- WS5SST
TempMatrix[,7] <- WS6SST
TempMatrix[,8] <- WS7SST
TempMatrix[,9] <- WS8SST
TempMatrix[,10] <- WS9SST
TempMatrix[,11] <- WS10SST
TempMatrix[,12] <- WS11SST
TempMatrix[,13] <- WS12SST
TempMatrix[,14] <- WS13SST
TempMatrix[,15] <- WS14SST
TempMatrix[,16] <- WS15SST
TempMatrix[,17] <- WS16SST
TempMatrix[,18] <- WS17SST
TempMatrix[,19] <- WS18SST
TempMatrix[,20] <- WS20SST
TempMatrix[,21] <- WS21SST

#fill with SSS bin intervals
TempMatrix[,1] <- seq(12.0,28.0,2.0)

colnames(TempMatrix) <- c("SST_Bin", "WS1", "WS2","WS3", "WS4", "WS5", "WS6", "WS7", "WS8", "WS9", "WS10", "WS11",
                          "WS12", "WS13", "WS14", "WS15", "WS16", "WS17","WS18","WS20", "WS21","ChosenSSS", "AvailSSS")

write.csv(TempMatrix, file = "ChiSquared_SST_2increment.csv")

#THEN GO INTO EXCEL AND ADD TOTALCOUNT ROW!!!! SAVE AGAIN AS EXCEL WORKBOOK!

###################################################################################################################
##USE LOG-LIKELIHOOD PACKAGE TO MAKE SURE EQUATION IS WORKING CORRECTLY IN NEXT STEP!

#get data
WS1SST<-c(0,0,0,24,19,23,0,0,0)
WS2SST<-c(0,0,10,16,23,10,4,0,0)
WS3SST<-c(0,0,0,0,12,19,0,0,0)
WS4SST<-c(0,0,0,0,0,7,0,0,0)
WS5SST<-c(0,0,0,0,7,5,3,0,0)
WS6SST<-c(0,0,0,9,9,22,4,0,0)
WS7SST<-c(0,0,1,11,19,13,0,0,0)
WS8SST<-c(0,1,10,19,6,26,5,0,0)
WS9SST<-c(0,0,1,32,54,32,27,4,0)
WS10SST<-c(0,0,0,0,31,16,0,0,0)
WS11SST<-c(0,0,10,25,59,39,36,0,0)
WS12SST<-c(0,0,0,30,16,5,6,0,0)
WS13SST<-c(0,0,0,49,16,0,0,0,0)
WS14SST<-c(0,0,13,36,76,5,0,0,0)
WS15SST<-c(0,0,6,16,42,22,11,0,0)
WS16SST<-c(0,0,0,17,29,39,30,4,0)
WS17SST<-c(0,0,0,1,0,0,0,0,0)
WS18SST<-c(0,0,0,0,9,0,0,0,0)
WS20SST<-c(0,0,0,0,8,7,0,0,0)
WS21SST<-c(0,0,0,0,19,6,2,0,0)

#SST bins + 18 sharks + total habitat + total avail habitat = 21 columns
#bins from 12-29.5 in 0.5 increments = 35 rows
SSTMatrix <- matrix(NA, 9,20)

#Fill matrix
SSTMatrix[,1] <- WS1SST
SSTMatrix[,2] <- WS2SST
SSTMatrix[,3] <- WS3SST
SSTMatrix[,4] <- WS4SST
SSTMatrix[,5] <- WS5SST
SSTMatrix[,6] <- WS6SST
SSTMatrix[,7] <- WS7SST
SSTMatrix[,8] <- WS8SST
SSTMatrix[,9] <- WS9SST
SSTMatrix[,10] <- WS10SST
SSTMatrix[,11] <- WS11SST
SSTMatrix[,12] <- WS12SST
SSTMatrix[,13] <- WS13SST
SSTMatrix[,14] <- WS14SST
SSTMatrix[,15] <- WS15SST
SSTMatrix[,16] <- WS16SST
SSTMatrix[,17] <- WS17SST
SSTMatrix[,18] <- WS18SST
SSTMatrix[,19] <- WS20SST
SSTMatrix[,20] <- WS21SST

library(rJava)
library(Deducer)
likelihood.test(SSTMatrix) # p-value = < 2.2e-16   log-likelihood stat = 603.08

#######################################################################################
#PART 1: EQUATION 14.1

#base equation: 
#(x[i,j]*(log(x[i,j]/((y*k)/a)))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- SSTMatrix #u.ij
totalhab<-c(0,1,51,285,454,296,128,8,0) #u.i+
totalcount<-c(66,63,31,7,15,44,44,67,150,47,169,57,65,130,97,119,1,9,15,27) #u.+j
totalhaball<-1223#u.++

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
part1*2 #=603.08 IT WORKED CORRECTLY!!!!!!!!!!!!!!

######################################
#PART 2: USE EQUATION 14.2

#base equation: 
#(x[i,j]*(log(x[i,j]/((y/a)*k))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- SSTMatrix #u.ij
availhab <- c(5126,181857,1121882,1997840,3067960,3159870,2414120,912603,16390) #pie.i when divided by availhaball
totalcount<-c(66,63,31,7,15,44,44,67,150,47,169,57,65,130,97,119,1,9,15,27) #u.+j
availhaball <- 12877648

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
part2*2 #=961.379

####################################################
####################################################
##NOW TAKE THE DIFFERENCE OF EQUATION 14.2-14.1 TO SEE IF SELECTION IS OCCURRING
961.379-603.08 #diff of stat = 1598.213

#degrees of freedom
#part 1 df = (habitat categories-1)*(number of sharks-1)
(9-1)*(20-1) #152

#part 2 df = sum(habitat categories - 1) from each shark
(9-1)*20 #160

160-152 #= 8 df

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
availhab <- c(5126,181857,1121882,1997840,3067960,3159870,2414120,912603,16390) #pie.i when divided by availhaball
availhaball <- 12877648
totalhaball<-1223#u.++
ChosenSST<-c(0,1,51,285,454,296,128,8,0)

#loop
SR<-0
SR<-capture.output(for(i in 1:length(ChosenSST)){
  xvar <- ChosenSST[i]
  yvar <- availhab[i]
  SR <- (xvar/((yvar/availhaball)*totalhaball))
  cat(SR, "\n")
})

SR<-as.numeric(SR)
SR

####################################################################
#PART 5: FIND STANDARD ERROR (SE)

#base equation:
#SE(w.i) = sqrt((n/((n-1)*(u.++)^2)*sum(((u.ij/pie.i)-w.i(u.+j))^2)))

#try loop 

###NEED TO CHANGE Hab1_count for EVERY LEVEL OF HABITAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! and switch availhab[1] and SR[1] to whatever level you're on

#SST12.0
totalcount<-c(66,63,31,7,15,44,44,67,150,47,169,57,65,130,97,119,1,9,15,27) #u.+j
availhaball <- 12877648
availhab <- c(5126,181857,1121882,1997840,3067960,3159870,2414120,912603,16390) #pie.i when divided by availhaball
SR<-c(0,0.0579002,0.4786666,1.502084,1.558175,0.9863535,0.5582917,0.0923035,0)

hab1_count <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab1)) #=

#SST14
hab2_count <- c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab2)) #=

#SST16
hab3_count <- c(0,10,0,0,0,0,1,10,1,0,10,0,0,13,6,0,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab3)) #=

#SST18
hab4_count <- c(24,16,0,0,0,9,11,19,32,0,25,30,49,36,16,17,1,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab4)) #=

#SST20
hab5_count <- c(19,23,12,0,7,9,19,6,54,31,59,16,16,76,42,29,0,9,8,19) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab5)) #=

#SST22
hab6_count <- c(23,10,19,7,5,22,13,26,32,16,39,5,0,5,22,39,0,0,7,6) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab6)) #=

#SST24
hab7_count <- c(0,4,0,0,3,4,0,5,27,0,36,6,0,0,11,30,0,0,0,2) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab7)) #=

#SST26
hab8_count <- c(0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,4,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab8)) #=

#SST28
hab9_count <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1223^2))*(sum_hab9)) #=


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
0.10/(2*9) #find z score based on this number
#z score for SST = 2.5392

SelRatio<-c(0,0.0579002,0.4786666,1.502084,1.558175,0.9863535,0.5582917,0.0923035,0)
StandErr<-c(0,0.0585299,0.1555485,0.2516528,0.1536535,0.1393177,0.1500566,0.05685175,0)


#for SSS, z score = 2.5392

#SST 12 = 0

#CI for SST14
SelRatio[2]+(2.5392*StandErr[2]) #=  for upper CI
SelRatio[2]-(2.5392*StandErr[2]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST16
SelRatio[3]+(2.5392*StandErr[3]) #=  for upper CI
SelRatio[3]-(2.5392*StandErr[3]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST18
SelRatio[4]+(2.5392*StandErr[4]) #=  for upper CI
SelRatio[4]-(2.5392*StandErr[4]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST20
SelRatio[5]+(2.5392*StandErr[5]) #=  for upper CI
SelRatio[5]-(2.5392*StandErr[5]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST22
SelRatio[6]+(2.5392*StandErr[6]) #=  for upper CI
SelRatio[6]-(2.5392*StandErr[6]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST24
SelRatio[7]+(2.5392*StandErr[7]) #=  for upper CI
SelRatio[7]-(2.5392*StandErr[7]) #=  for lower CI (can't observe neg value, so change to 0)

#CI for SST26
SelRatio[8]+(2.5392*StandErr[8]) #=  for upper CI
SelRatio[8]-(2.5392*StandErr[8]) #= for lower CI (can't observe neg value, so change to 0)

#CI for SST28
SelRatio[9]+(2.5392*StandErr[9]) #=  for upper CI
SelRatio[9]-(2.5392*StandErr[9]) #= for lower CI (can't observe neg value, so change to 0)


#########################################################################
##SST PLOT BOXPLOT WITH 90% CIs

SR<-c(0,0.0579002,0.4786666,1.502084,1.558175,0.9863535,0.5582917,0.0923035,0)
CI_up<-c(0,0.2065193,0.8736354,2.141081,1.948332,1.340109,0.9393154,0.2366615,0)
CI_low<-c(0,0,0.08369785,0.8630872,1.168018,0.632598,0.177268,0,0)
SST_bin<-seq(12,28,2)

SST_Plot_Matrix <- matrix(NA, 9,4)

SST_Plot_Matrix[,1] <- SST_bin
SST_Plot_Matrix[,2] <- SR
SST_Plot_Matrix[,3] <- CI_up
SST_Plot_Matrix[,4] <- CI_low

colnames(SST_Plot_Matrix)<-c("bin", "SelRatio", "CI_upper", "CI_lower")

SST_df<- as.data.frame(SST_Plot_Matrix)

#par(mar=c(5,4,4,2)+0.1) #normal
#par(mar=c(5.5,4,4,2)+0.1) #give more margin space for x-axis

plot(SST_df$bin, SST_df$SelRatio, ylim=c(0, 3), xlim=c(12, 30), xlab="", ylab="Selection Ratio", main="", pch=20, xaxt="n", cex=1.8, cex.lab=1.45)
axis(1, at=seq(12, 28, 2), labels=rep("", each=9))
text(SST_df$bin, par("usr")[3]-0.1, srt = 60, adj= 1, xpd = TRUE, labels = c("12.0-14.0", "14.0-16.0", "16.0-18.0", "18.0-20.0", "20.0-22.0", "22.0-24.0", "24.0-26.0",
                                                                             "26.0-28.0", "28.0-30.0") , cex=.95)
mtext("Temperature (?C)", side=1, line=4, cex=1.45)
abline(h=1, lty=2, col="red")
arrows(SST_df$bin, SST_df$CI_upper, SST_df$bin, SST_df$CI_lower, length=0.05, angle=90, code=3, col = 'black')


