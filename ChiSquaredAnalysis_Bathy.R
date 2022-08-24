library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(cowplot)
library(ggthemes)
library(grid)
library(readxl)


###Bathy

#Available Bathy all years
#GEBCOAllData<-read_excel("GEBCO_bathy_NYBightShelf_points.xlsx")

#Chosen Bathy all years
#All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("All_SPOTACT_BathySSTChlaSSS_Results.xlsx")

#Break data into lists for looping/iterations to fill matrix
ChosenBathy<-c(10,208,581,335,45,28,6,5,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
AvailBathy<-c(6549,12226,16452,20213,17760,16134,13891,9437,6416,3054,1976,2556,2344,1405,919,667,429,361,311,232,71,47,19,15,6,6,6,1,5,2)

WS1Bathy<-c(1,17,33,13,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS2Bathy<-c(0,21,13,18,10,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS3Bathy<-c(0,2,19,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS4Bathy<-c(0,2,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS5Bathy<-c(0,8,1,2,1,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS6Bathy<-c(0,16,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS7Bathy<-c(0,0,17,24,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS8Bathy<-c(7,39,16,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS9Bathy<-c(1,9,126,13,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS10Bathy<-c(0,0,17,13,10,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS11Bathy<-c(0,14,90,41,2,7,6,4,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS12Bathy<-c(0,23,16,16,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS13Bathy<-c(1,38,25,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS14Bathy<-c(0,3,78,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS15Bathy<-c(0,9,67,17,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS16Bathy<-c(0,1,14,97,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS17Bathy<-c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS18Bathy<-c(0,0,0,0,4,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS20Bathy<-c(0,1,8,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS21Bathy<-c(0,5,10,9,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


#Bathy bins + 20 sharks + total habitat + total avail habitat = 23 columns
#bins from -300-0 in 10 increments = 30 rows
GEBCOBathyMatrix <- matrix(NA, 30,23)

#Fill matrix with WS# Bathy
GEBCOBathyMatrix[,20] <- ChosenBathy
GEBCOBathyMatrix[,21] <- AvailBathy

GEBCOBathyMatrix[,2] <- WS1Bathy
GEBCOBathyMatrix[,3] <- WS2Bathy
GEBCOBathyMatrix[,4] <- WS3Bathy
GEBCOBathyMatrix[,5] <- WS4Bathy
GEBCOBathyMatrix[,6] <- WS5Bathy
GEBCOBathyMatrix[,7] <- WS6Bathy
GEBCOBathyMatrix[,8] <- WS7Bathy
GEBCOBathyMatrix[,9] <- WS8Bathy
GEBCOBathyMatrix[,10] <- WS9Bathy
GEBCOBathyMatrix[,11] <- WS10Bathy
GEBCOBathyMatrix[,12] <- WS11Bathy
GEBCOBathyMatrix[,13] <- WS12Bathy
GEBCOBathyMatrix[,14] <- WS13Bathy
GEBCOBathyMatrix[,15] <- WS14Bathy
GEBCOBathyMatrix[,16] <- WS15Bathy
GEBCOBathyMatrix[,17] <- WS16Bathy
GEBCOBathyMatrix[,18] <- WS17Bathy
GEBCOBathyMatrix[,19] <- WS18Bathy
GEBCOBathyMatrix[,20] <- WS20Bathy
GEBCOBathyMatrix[,21] <- WS21Bathy


#fill with Bathy bin intervals
GEBCOBathyMatrix[,1] <- rev(seq(-290,0,10))

colnames(GEBCOBathyMatrix) <- c("Bathy_Bin", "WS1", "WS2","WS3", "WS4", "WS5", "WS6", "WS7", "WS8", "WS9", "WS10", "WS11",
                         "WS12", "WS13", "WS14", "WS15", "WS16", "WS17","WS18","WS20", "WS21","ChosenBathy", "AvailBathy")

#write.csv(GEBCOBathyMatrix, file = "ChiSquared_Bathy_REDO.csv")

#THEN GO INTO EXCEL AND ADD TOTALCOUNT ROW!!!! SAVE AGAIN AS EXCEL WORKBOOK!

###################################################################################################################
##USE LOG-LIKELIHOOD PACKAGE TO MAKE SURE EQUATION IS WORKING CORRECTLY IN NEXT STEP!

#get data
ChosenBathy<-c(10,208,581,335,45,28,6,5,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
AvailBathy<-c(6549,12226,16452,20213,17760,16134,13891,9437,6416,3054,1976,2556,2344,1405,919,667,429,361,311,232,71,47,19,15,6,6,6,1,5,2)

WS1Bathy<-c(1,17,33,13,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS2Bathy<-c(0,21,13,18,10,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS3Bathy<-c(0,2,19,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS4Bathy<-c(0,2,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS5Bathy<-c(0,8,1,2,1,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS6Bathy<-c(0,16,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS7Bathy<-c(0,0,17,24,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS8Bathy<-c(7,39,16,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS9Bathy<-c(1,9,126,13,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS10Bathy<-c(0,0,17,13,10,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS11Bathy<-c(0,14,90,41,2,7,6,4,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS12Bathy<-c(0,23,16,16,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS13Bathy<-c(1,38,25,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS14Bathy<-c(0,3,78,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS15Bathy<-c(0,9,67,17,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS16Bathy<-c(0,1,14,97,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS17Bathy<-c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS18Bathy<-c(0,0,0,0,4,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS20Bathy<-c(0,1,8,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WS21Bathy<-c(0,5,10,9,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#number of fish = 20 columns
#bins are habitat type = 16 rows
BathyMatrix <- matrix(NA, 30,20)

#Fill matrix
BathyMatrix[,1] <- WS1Bathy
BathyMatrix[,2] <- WS2Bathy
BathyMatrix[,3] <- WS3Bathy
BathyMatrix[,4] <- WS4Bathy
BathyMatrix[,5] <- WS5Bathy
BathyMatrix[,6] <- WS6Bathy
BathyMatrix[,7] <- WS7Bathy
BathyMatrix[,8] <- WS8Bathy
BathyMatrix[,9] <- WS9Bathy
BathyMatrix[,10] <- WS10Bathy
BathyMatrix[,11] <- WS11Bathy
BathyMatrix[,12] <- WS12Bathy
BathyMatrix[,13] <- WS13Bathy
BathyMatrix[,14] <- WS14Bathy
BathyMatrix[,15] <- WS15Bathy
BathyMatrix[,16] <- WS16Bathy
BathyMatrix[,17] <- WS17Bathy
BathyMatrix[,18] <- WS18Bathy
BathyMatrix[,19] <- WS20Bathy
BathyMatrix[,20] <- WS21Bathy

library(rJava)
library(Deducer)
likelihood.test(BathyMatrix) # p-value = 2.554e-15  log-likelihood stat = 852.14

#######################################################################################
#PART 1: EQUATION 14.1 (First Chi-Square Statistic)

#base equation: 
#(x[i,j]*(log(x[i,j]/((y*k)/a)))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- BathyMatrix #u.ij
totalhab <- c(10,208,581,335,45,28,6,5,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #u.i+
totalcount <- c(66,63,31,7,15,44,44,67,150,47,168,57,65,130,97,119,1,9,15,27) #u.+j
totalhaball <- 1222 #u.++

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
part1*2 #=852.1356 IT WORKED CORRECTLY!!!!!!!!!!!!!!

######################################
#PART 2: USE EQUATION 14.2  (Second chi-square statistic)

#base equation: 
#(x[i,j]*(log(x[i,j]/((y/a)*k))) and then multiple answer by 2 to get log-likelihood chi squared value

count <- BathyMatrix #u.ij
availhab <- c(6549,12226,16452,20213,17760,16134,13891,9437,6416,3054,1976,2556,2344,1405,919,667,429,361,311,232,71,47,19,15,6,6,6,1,5,2) #pie.i when divided by availhaball
totalcount <- c(66,63,31,7,15,44,44,67,150,47,168,57,65,130,97,119,1,9,15,27) #u.+j
availhaball <- 133510

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
part2*2 #=2743.337

####################################################
##NOW TAKE THE DIFFERENCE OF EQUATION 14.2-14.1 TO SEE IF SELECTION IS OCCURRING
2743.337-852.1356 #diff of stat = 1891.201

#degrees of freedom
#part 1 df = (habitat categories-1)*(number of sharks-1)
(30-1)*(20-1) #551

#part 2 df = sum(habitat categories - 1) from each shark
(30-1)*20 #580

580-551 #=29  df

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
availhab <- c(6549,12226,16452,20213,17760,16134,13891,9437,6416,3054,1976,2556,2344,1405,919,667,429,361,311,232,71,47,19,15,6,6,6,1,5,2) #pie.i when divided by availhaball
availhaball <- 133510
totalhaball <- 1222 #u.++
ChosenBathy<-c(10,208,581,335,45,28,6,5,2,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#loop
SR<-0
SR<-capture.output(for(i in 1:length(ChosenBathy)){
  xvar <- ChosenBathy[i]
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

#try loop to find sum_hab1 instead   


###NEED TO CHANGE Hab1_count for EVERY LEVEL OF HABITAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! and switch availhab[1] and SR[1] to whatever level you're on

#0-10 m
totalcount <- c(66,63,31,7,15,44,44,67,150,47,168,57,65,130,97,119,1,9,15,27) #u.+j
availhaball <- 133510
availhab <- c(6549,12226,16452,20213,17760,16134,13891,9437,6416,3054,1976,2556,2344,1405,919,667,429,361,311,232,71,47,19,15,6,6,6,1,5,2) #pie.i when divided by availhaball
SR<-c(0.1668275,1.858752,3.858336,1.810742,0.2768294,0.1896088,0.04719112,0.05788668,0.03405714,0.0357745,0,0,0,0,0,0.1638011,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

hab1_count <- c(1,0,0,0,0,0,0,7,1,0,0,0,1,0,0,0,0,0,0,0) #u.ij

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
sqrt((20/((20-1)*1222^2))*(sum_hab1)) #= #works correctly!!

#10-20 m
hab2_count <- c(17,21,2,2,8,16,0,39,9,0,14,23,38,3,9,1,0,0,1,5) #u.ij

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
sqrt((20/((20-1)*1222^2))*(sum_hab2))

#20-30m
hab3_count <- c(33,13,19,2,1,28,17,16,126,17,90,16,25,78,67,14,1,0,8,10) #u.ij

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
sqrt((20/((20-1)*1222^2))*(sum_hab3)) 

#30-40m
hab4_count <- c(13,18,10,3,2,0,24,3,13,13,41,16,1,49,17,97,0,0,6,9) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab4))

#40-50m
hab5_count <- c(1,10,0,0,1,0,3,2,1,10,2,2,0,0,4,3,0,4,0,2) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab5))

#50-60m
hab6_count <- c(1,1,0,0,2,0,0,0,0,7,7,0,0,0,0,4,0,5,0,1) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab6))

#60-70m
hab7_count <- c(0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab7))

#70-80m
hab8_count <- c(0,0,0,0,1,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab8))

#80-90m
hab9_count <- c(0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab9)) 

#90-100m
hab10_count <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0) #u.ij 

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
sqrt((20/((20-1)*1222^2))*(sum_hab10))

#100-110m
hab11_count <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #u.ij

sum_hab11<-(((hab11_count[1]/(availhab[11]/availhaball))-(SR[11]*totalcount[1]))^2)+
  (((hab11_count[2]/(availhab[11]/availhaball))-(SR[11]*totalcount[2]))^2)+
  (((hab11_count[3]/(availhab[11]/availhaball))-(SR[11]*totalcount[3]))^2)+
  (((hab11_count[4]/(availhab[11]/availhaball))-(SR[11]*totalcount[4]))^2)+
  (((hab11_count[5]/(availhab[11]/availhaball))-(SR[11]*totalcount[5]))^2)+
  (((hab11_count[6]/(availhab[11]/availhaball))-(SR[11]*totalcount[6]))^2)+
  (((hab11_count[7]/(availhab[11]/availhaball))-(SR[11]*totalcount[7]))^2)+
  (((hab11_count[8]/(availhab[11]/availhaball))-(SR[11]*totalcount[8]))^2)+
  (((hab11_count[9]/(availhab[11]/availhaball))-(SR[11]*totalcount[9]))^2)+
  (((hab11_count[10]/(availhab[11]/availhaball))-(SR[11]*totalcount[10]))^2)+
  (((hab11_count[11]/(availhab[11]/availhaball))-(SR[11]*totalcount[11]))^2)+
  (((hab11_count[12]/(availhab[11]/availhaball))-(SR[11]*totalcount[12]))^2)+
  (((hab11_count[13]/(availhab[11]/availhaball))-(SR[11]*totalcount[13]))^2)+
  (((hab11_count[14]/(availhab[11]/availhaball))-(SR[11]*totalcount[14]))^2)+
  (((hab11_count[15]/(availhab[11]/availhaball))-(SR[11]*totalcount[15]))^2)+
  (((hab11_count[16]/(availhab[11]/availhaball))-(SR[11]*totalcount[16]))^2)+
  (((hab11_count[17]/(availhab[11]/availhaball))-(SR[11]*totalcount[17]))^2)+
  (((hab11_count[18]/(availhab[11]/availhaball))-(SR[11]*totalcount[18]))^2)+
  (((hab11_count[19]/(availhab[11]/availhaball))-(SR[11]*totalcount[19]))^2)+
  (((hab11_count[20]/(availhab[11]/availhaball))-(SR[11]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*1222^2))*(sum_hab11))

#150-160m
hab16_count <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0) #u.ij

sum_hab16<-(((hab16_count[1]/(availhab[16]/availhaball))-(SR[16]*totalcount[1]))^2)+
  (((hab16_count[2]/(availhab[16]/availhaball))-(SR[16]*totalcount[2]))^2)+
  (((hab16_count[3]/(availhab[16]/availhaball))-(SR[16]*totalcount[3]))^2)+
  (((hab16_count[4]/(availhab[16]/availhaball))-(SR[16]*totalcount[4]))^2)+
  (((hab16_count[5]/(availhab[16]/availhaball))-(SR[16]*totalcount[5]))^2)+
  (((hab16_count[6]/(availhab[16]/availhaball))-(SR[16]*totalcount[6]))^2)+
  (((hab16_count[7]/(availhab[16]/availhaball))-(SR[16]*totalcount[7]))^2)+
  (((hab16_count[8]/(availhab[16]/availhaball))-(SR[16]*totalcount[8]))^2)+
  (((hab16_count[9]/(availhab[16]/availhaball))-(SR[16]*totalcount[9]))^2)+
  (((hab16_count[10]/(availhab[16]/availhaball))-(SR[16]*totalcount[10]))^2)+
  (((hab16_count[11]/(availhab[16]/availhaball))-(SR[16]*totalcount[11]))^2)+
  (((hab16_count[12]/(availhab[16]/availhaball))-(SR[16]*totalcount[12]))^2)+
  (((hab16_count[13]/(availhab[16]/availhaball))-(SR[16]*totalcount[13]))^2)+
  (((hab16_count[14]/(availhab[16]/availhaball))-(SR[16]*totalcount[14]))^2)+
  (((hab16_count[15]/(availhab[16]/availhaball))-(SR[16]*totalcount[15]))^2)+
  (((hab16_count[16]/(availhab[16]/availhaball))-(SR[16]*totalcount[16]))^2)+
  (((hab16_count[17]/(availhab[16]/availhaball))-(SR[16]*totalcount[17]))^2)+
  (((hab16_count[18]/(availhab[16]/availhaball))-(SR[16]*totalcount[18]))^2)+
  (((hab16_count[19]/(availhab[16]/availhaball))-(SR[16]*totalcount[19]))^2)+
  (((hab16_count[20]/(availhab[16]/availhaball))-(SR[16]*totalcount[20]))^2)
#then take sqrt
sqrt((20/((20-1)*1222^2))*(sum_hab16))

#the rest are 0


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
0.10/(2*30) #find z score based on this number
#z score for Bathy = 2.9352

SelRatio<-c(0.1668275,1.858752,3.858336,1.810742,0.2768294,0.1896088,0.04719112,0.05788668,0.03405714,0.0357745,0,0,0,0,0,0.1638011,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
StandErr<-c(0.1182748,0.528336,0.5485632,0.4325996,0.09345501,0.07205054,0.04344848,0.04345547,0.03135613,0.03293728,0,0,0,0,0,0.1508103,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


#for Bathy, z score = 2.9352

#0-10
SelRatio[1]+(2.9352*StandErr[1]) #=  for upper CI
SelRatio[1]-(2.9352*StandErr[1]) #=  for lower CI (can't observe neg value, so change to 0)

#10-20
SelRatio[2]+(2.9352*StandErr[2]) #=  for upper CI
SelRatio[2]-(2.9352*StandErr[2]) #=  for lower CI (can't observe neg value, so change to 0)

#20-30
SelRatio[3]+(2.9352*StandErr[3]) #=  for upper CI
SelRatio[3]-(2.9352*StandErr[3]) #=  for lower CI (can't observe neg value, so change to 0)

#30-40
SelRatio[4]+(2.9352*StandErr[4]) #=  for upper CI
SelRatio[4]-(2.9352*StandErr[4]) #=  for lower CI (can't observe neg value, so change to 0)

#40-50
SelRatio[5]+(2.9352*StandErr[5]) #=  for upper CI
SelRatio[5]-(2.9352*StandErr[5]) #=  for lower CI (can't observe neg value, so change to 0)

#50-60
SelRatio[6]+(2.9352*StandErr[6]) #=  for upper CI
SelRatio[6]-(2.9352*StandErr[6]) #=  for lower CI (can't observe neg value, so change to 0)

#60-70
SelRatio[7]+(2.9352*StandErr[7]) #=  for upper CI
SelRatio[7]-(2.9352*StandErr[7]) #=  for lower CI (can't observe neg value, so change to 0)

#70-80
SelRatio[8]+(2.9352*StandErr[8]) #=  for upper CI
SelRatio[8]-(2.9352*StandErr[8]) #=  for lower CI (can't observe neg value, so change to 0)

#80-90
SelRatio[9]+(2.9352*StandErr[9]) #=  for upper CI
SelRatio[9]-(2.9352*StandErr[9]) #=  for lower CI (can't observe neg value, so change to 0)

#90-100
SelRatio[10]+(2.9352*StandErr[10]) #=  for upper CI
SelRatio[10]-(2.9352*StandErr[10]) #=  for lower CI (can't observe neg value, so change to 0)

#100-110
SelRatio[11]+(2.9352*StandErr[11]) #=  for upper CI
SelRatio[11]-(2.9352*StandErr[11]) #=  for lower CI (can't observe neg value, so change to 0)

#110-150 = 0

#150-160
SelRatio[16]+(2.9352*StandErr[16]) #=  for upper CI
SelRatio[16]-(2.9352*StandErr[16]) #=  for lower CI (can't observe neg value, so change to 0)

#160-170
SelRatio[17]+(2.9352*StandErr[17]) #=  for upper CI
SelRatio[17]-(2.9352*StandErr[17]) #=  for lower CI (can't observe neg value, so change to 0)

#the rest are 0

#########################################################################
##DEPTH/BATHY PLOT BOXPLOT WITH 90% CIs

SR<-c(0.1668275,1.858752,3.858336,1.810742,0.2768294,0.1896088,0.04719112,0.05788668,0.03405714,0.0357745,0,0,0,0,0,0.1638011)
CI_up<-c(0.5139877,3.409524,5.468479,3.080508,0.5511385,0.4010915,0.1747211,0.1854372,0.1260937,0.132452,0,0,0,0,0,0.6064595)
CI_low<-c(0,0.3079802,2.248193,0.5409757,0.002520255,0,0,0,0,0,0,0,0,0,0,0)
Bathy_bin<- rev(seq(-150,0,10))

Bathy_Plot_Matrix <- matrix(NA, 16,4)

Bathy_Plot_Matrix[,1] <- Bathy_bin
Bathy_Plot_Matrix[,2] <- SR
Bathy_Plot_Matrix[,3] <- CI_up
Bathy_Plot_Matrix[,4] <- CI_low

colnames(Bathy_Plot_Matrix)<-c("bin", "SelRatio", "CI_upper", "CI_lower")

Bathy_df<- as.data.frame(Bathy_Plot_Matrix)

#par(mar=c(5,4,4,2)+0.1) #normal
#par(mar=c(5.5,4,4,2)+0.1) #give more margin space for x-axis

plot(Bathy_df$bin, Bathy_df$SelRatio, ylim=c(0, 6), xlab="", ylab="Selection Ratio", main="", pch=20, xaxt="n", xlim=c(0,-150), cex=1.8, cex.lab=1.45)
axis(1, at=seq(-150, 0, 10), labels=rep("", each=16))
text(Bathy_df$bin, par("usr")[3]-.15, srt = 60, adj= 1, xpd = TRUE, labels = rev(c("-150 to -160", 
                                                                                   "-140 to -150", "-130 to -140", "-120 to -130", "-110 to -120", "-100 to -110", 
                                                                                   "-90 to -100", "-80 to -90", "-70 to -80", "-60 to -70", "-50 to -60", "-40 to -50", 
                                                                                   "-30 to -40", "-20 to -30", "-10 to -20", "0 to -10")), cex=.95)
mtext("Depth (m)", side=1, line=4, cex=1.45)
abline(h=1, lty=2, col="red")
arrows(Bathy_df$bin, Bathy_df$CI_upper, Bathy_df$bin, Bathy_df$CI_lower, length=0.05, angle=90, code=3, col = 'black')
