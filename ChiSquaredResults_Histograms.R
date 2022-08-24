library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(cowplot)
library(ggthemes)
library(grid)
library(readxl)

#make transparent colors for histograms
t_col <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
}

dodgerblue3_tr <- t_col("dodgerblue3", perc = 50, name=NULL)
orange_tr <-  t_col("orange", perc = 50, name=NULL)
blue_tr <-  t_col("blue", perc = 50, name=NULL)
purple_tr <-  t_col("purple", perc = 50, name=NULL)
green4_tr <-  t_col("green4", perc = 50, name=NULL)
limegreen_tr <-  t_col("limegreen", perc = 50, name=NULL)
darkorchid1_tr<- t_col("darkorchid1", perc = 50, name=NULL)
mediumpurple1_tr<- t_col("mediumpurple1", perc = 50, name=NULL)
violetred1_tr<- t_col("violetred1", perc = 50, name=NULL)
turquoise1_tr <- t_col("turquoise1", perc = 50, name=NULL)

################################################################################################################################################
##SSS ***********1 ppt interval*********

#Available SSS all years 
SSSAllData=read.csv("All_SPOTACT_Rerddap_SSSPoly_Results_16_19.csv") #set working directory first to recognize file

AvailSSSAllhisto<-hist(SSSAllData$Salinity, breaks=seq(30, 38, 1), main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,1450))
text(x=AvailSSSAllhisto$mids, y=AvailSSSAllhisto$counts,labels = AvailSSSAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38,1), labels=seq(30, 38, 1), las=2)
summary(SSSAllData$Salinity) # 145641 NAs
AvailSSSAllhisto #max count: 1399


#Available SSS all years PERCENTAGES
AvailSSSAllhisto_percent<-hist(SSSAllData$Salinity, breaks=seq(30, 38, 1), main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,1300))
AvailSSSAllhisto_percent$density<-AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100
AvailSSSAllhisto_percent$density
summary(SSSAllData$Salinity) # 117780 NAs
AvailSSSAllhisto_percent #max count: 
plot(AvailSSSAllhisto_percent, freq=FALSE, ylab="Percentage", main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,35))
text(x=AvailSSSAllhisto_percent$mids, y=AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100,labels = AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38,1), labels=seq(30, 38, 1), las=2)

#chosen SSS all years
library(readxl)
All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("DS_Bathy_SST_SSS_Chla_16_19.xlsx")

#subset by WS number
ChosenSSS_WS1sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==1)
ChosenSSS_WS2sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==2)
ChosenSSS_WS3sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==3)
ChosenSSS_WS4sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==4)
ChosenSSS_WS5sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==5)
ChosenSSS_WS6sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==6)
ChosenSSS_WS7sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==7)
ChosenSSS_WS8sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==8)
ChosenSSS_WS9sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==9)
ChosenSSS_WS10sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==10)
ChosenSSS_WS11sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==11)
ChosenSSS_WS12sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==12)
ChosenSSS_WS13sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==13)
ChosenSSS_WS14sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==14)
ChosenSSS_WS15sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==15)
ChosenSSS_WS16sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==16)
ChosenSSS_WS17sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==17)
ChosenSSS_WS18sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==18)
ChosenSSS_WS20sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==20)
ChosenSSS_WS21sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==21)
###########
#All Chosen SSS
ChosenSSSAllhisto<-hist(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS, breaks=seq(30, 38, 1), main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,70))
text(x=ChosenSSSAllhisto$mids, y=ChosenSSSAllhisto$counts,labels = ChosenSSSAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
summary(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS) #1070 NAs
ChosenSSSAllhisto  #max count: 66

#Chosen SSS all years PERCENTAGES
ChosenSSSAllhisto_percent<-hist(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS, breaks=seq(30, 38, 1), main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,170))
ChosenSSSAllhisto_percent$density<-ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100
ChosenSSSAllhisto_percent$density
summary(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS) #3627 NAs
ChosenSSSAllhisto_percent #max count: 
plot(ChosenSSSAllhisto_percent, freq=FALSE, ylab="Percentage",  main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,70))
text(x=ChosenSSSAllhisto_percent$mids, y=ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100,labels = ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)

#two axes - 1:1 ratio TRANSPARENT
plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity")
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,1450,50), labels=seq(0,1450,50), col="red4", col.axis="red4")
par(new=T)
plot(ChosenSSSAllhisto, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
axis(side = 4, at=seq(0,65,5),labels=seq(0,65,5), col = "orange", col.axis = "orange", ylab = "Frequency")
legend(36,160, legend=c("Available", "Actual"), col=c("red4", "orange"), pch=15:15, cex=0.9)

#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity", freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(ChosenSSSAllhisto_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,45))
axis(side = 4, at=seq(0,44,2),labels=seq(0,44,2), col = "orange", col.axis = "orange")
legend(36,42, legend=c("Available", "Actual"), col=c("red4", "orange"), pch=15:15, cex=0.9)

#two axes - 100 vs 100 ratio TRANSPARENT PERCENTAGE
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity", freq=FALSE, ylab="Percentage", ylim=c(0,45))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,45,5), labels=seq(0,45,5), col="red4", col.axis="red4")
par(new=T)
plot(ChosenSSSAllhisto_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,45))
axis(side = 4, at=seq(0,45,5),labels=seq(0,45,5), col = "orange", col.axis = "orange")
legend(35,45, legend=c("Available", "Actual"), col=c("red4", "orange"), pch=15:15, cex=0.9)

{
  ###########
  #WS1 Chosen SSS
  WS1_ChosenSSS_histo<-hist(ChosenSSS_WS1sub$MeanSSS, breaks=seq(30, 38, 1), main="WS1 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS1_ChosenSSS_histo$mids, y=WS1_ChosenSSS_histo$counts,labels = WS1_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS1sub$MeanSSS) #85 NAs
  WS1_ChosenSSS_histo  #max count: 10
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS1 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS1_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS1_ChosenSSS_histo$mids, y=WS1_ChosenSSS_histo$counts,labels = WS1_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,10,2),labels=seq(0,10,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS2 Chosen SSS
  WS2_ChosenSSS_histo<-hist(ChosenSSS_WS2sub$MeanSSS, breaks=seq(30, 38, 1), main="WS2 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS2_ChosenSSS_histo$mids, y=WS2_ChosenSSS_histo$counts,labels = WS2_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS2sub$MeanSSS) #59 NAs
  WS2_ChosenSSS_histo  #max count: 29
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS2 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS2_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS2_ChosenSSS_histo$mids, y=WS2_ChosenSSS_histo$counts,labels = WS2_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,30,5),labels=seq(0,30,5), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS3 Chosen SSS
  WS3_ChosenSSS_histo<-hist(ChosenSSS_WS3sub$MeanSSS, breaks=seq(30, 38, 1), main="WS3 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS3_ChosenSSS_histo$mids, y=WS3_ChosenSSS_histo$counts,labels = WS3_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS3sub$MeanSSS) #21 NAs
  WS3_ChosenSSS_histo  #max count: 8
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS3 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS3_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS3_ChosenSSS_histo$mids, y=WS3_ChosenSSS_histo$counts,labels = WS3_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,8,2),labels=seq(0,8,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS4 Chosen SSS
  WS4_ChosenSSS_histo<-hist(ChosenSSS_WS4sub$MeanSSS, breaks=seq(30, 38, 1), main="WS4 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS4_ChosenSSS_histo$mids, y=WS4_ChosenSSS_histo$counts,labels = WS4_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS4sub$MeanSSS) #39 NAs
  WS4_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS4 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS4_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS4_ChosenSSS_histo$mids, y=WS4_ChosenSSS_histo$counts,labels = WS4_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS5 Chosen SSS
  WS5_ChosenSSS_histo<-hist(ChosenSSS_WS5sub$MeanSSS, breaks=seq(30, 38, 1), main="WS5 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS5_ChosenSSS_histo$mids, y=WS5_ChosenSSS_histo$counts,labels = WS5_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS5sub$MeanSSS) #361 NAs
  WS5_ChosenSSS_histo  #max count: 50
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS5 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS5_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS5_ChosenSSS_histo$mids, y=WS5_ChosenSSS_histo$counts,labels = WS5_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,50,10),labels=seq(0,50,10), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS6 Chosen SSS
  WS6_ChosenSSS_histo<-hist(ChosenSSS_WS6sub$MeanSSS, breaks=seq(30, 38, 1), main="WS6 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS6_ChosenSSS_histo$mids, y=WS6_ChosenSSS_histo$counts,labels = WS6_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS6sub$MeanSSS) #502 NAs
  WS6_ChosenSSS_histo  #max count: 14
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS6 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS6_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS6_ChosenSSS_histo$mids, y=WS6_ChosenSSS_histo$counts,labels = WS6_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS7 Chosen SSS
  WS7_ChosenSSS_histo<-hist(ChosenSSS_WS7sub$MeanSSS, breaks=seq(30, 38, 1), main="WS7 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS7_ChosenSSS_histo$mids, y=WS7_ChosenSSS_histo$counts,labels = WS7_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS7sub$MeanSSS) #251 NAs
  WS7_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS7 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS7_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS7_ChosenSSS_histo$mids, y=WS7_ChosenSSS_histo$counts,labels = WS7_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,01),labels=seq(0,1,01), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS8 Chosen SSS
  WS8_ChosenSSS_histo<-hist(ChosenSSS_WS8sub$MeanSSS, breaks=seq(30, 38, 1), main="WS8 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS8_ChosenSSS_histo$mids, y=WS8_ChosenSSS_histo$counts,labels = WS8_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS8sub$MeanSSS) #364 NAs
  WS8_ChosenSSS_histo  #max count: 39
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS8 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS8_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS8_ChosenSSS_histo$mids, y=WS8_ChosenSSS_histo$counts,labels = WS8_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,40,5),labels=seq(0,40,5), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS9 Chosen SSS
  WS9_ChosenSSS_histo<-hist(ChosenSSS_WS9sub$MeanSSS, breaks=seq(30, 38, 1), main="WS9 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS9_ChosenSSS_histo$mids, y=WS9_ChosenSSS_histo$counts,labels = WS9_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS9sub$MeanSSS) #20 NAs
  WS9_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS9 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS9_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS9_ChosenSSS_histo$mids, y=WS9_ChosenSSS_histo$counts,labels = WS9_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS10 Chosen SSS
  WS10_ChosenSSS_histo<-hist(ChosenSSS_WS10sub$MeanSSS, breaks=seq(30, 38, 1), main="WS10 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS10_ChosenSSS_histo$mids, y=WS10_ChosenSSS_histo$counts,labels = WS10_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS10sub$MeanSSS) #142 NAs
  WS10_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS10 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS10_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS10_ChosenSSS_histo$mids, y=WS10_ChosenSSS_histo$counts,labels = WS10_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,01),labels=seq(0,1,01), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS11 Chosen SSS
  WS11_ChosenSSS_histo<-hist(ChosenSSS_WS11sub$MeanSSS, breaks=seq(30, 38, 1), main="WS11 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS11_ChosenSSS_histo$mids, y=WS11_ChosenSSS_histo$counts,labels = WS11_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS11sub$MeanSSS) #61 NAs
  WS11_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS11 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS11_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS11_ChosenSSS_histo$mids, y=WS11_ChosenSSS_histo$counts,labels = WS11_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS12 Chosen SSS
  WS12_ChosenSSS_histo<-hist(ChosenSSS_WS12sub$MeanSSS, breaks=seq(30, 38, 1), main="WS12 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS12_ChosenSSS_histo$mids, y=WS12_ChosenSSS_histo$counts,labels = WS12_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS12sub$MeanSSS) #16 NAs
  WS12_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS12 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS12_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS12_ChosenSSS_histo$mids, y=WS12_ChosenSSS_histo$counts,labels = WS12_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS13 Chosen SSS
  WS13_ChosenSSS_histo<-hist(ChosenSSS_WS13sub$MeanSSS, breaks=seq(30, 38, 1), main="WS13 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS13_ChosenSSS_histo$mids, y=WS13_ChosenSSS_histo$counts,labels = WS13_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS13sub$MeanSSS) #1276 NAs
  WS13_ChosenSSS_histo  #max count: 15
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS13 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS13_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS13_ChosenSSS_histo$mids, y=WS13_ChosenSSS_histo$counts,labels = WS13_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,15,3),labels=seq(0,15,3), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS14 Chosen SSS
  WS14_ChosenSSS_histo<-hist(ChosenSSS_WS14sub$MeanSSS, breaks=seq(30, 38, 1), main="WS14 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS14_ChosenSSS_histo$mids, y=WS14_ChosenSSS_histo$counts,labels = WS14_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS14sub$MeanSSS) #314 NAs
  WS14_ChosenSSS_histo  #max count: 14
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS14 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS14_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS14_ChosenSSS_histo$mids, y=WS14_ChosenSSS_histo$counts,labels = WS14_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS15 Chosen SSS
  WS15_ChosenSSS_histo<-hist(ChosenSSS_WS15sub$MeanSSS, breaks=seq(30, 38, 1), main="WS15 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS15_ChosenSSS_histo$mids, y=WS15_ChosenSSS_histo$counts,labels = WS15_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS15sub$MeanSSS) #62 NAs
  WS15_ChosenSSS_histo  #max count: 2
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS15 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS15_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS15_ChosenSSS_histo$mids, y=WS15_ChosenSSS_histo$counts,labels = WS15_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS16 Chosen SSS
  WS16_ChosenSSS_histo<-hist(ChosenSSS_WS16sub$MeanSSS, breaks=seq(30, 38, 1), main="WS16 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS16_ChosenSSS_histo$mids, y=WS16_ChosenSSS_histo$counts,labels = WS16_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS16sub$MeanSSS) #37 NAs
  WS16_ChosenSSS_histo  #max count: 10
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS16 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS16_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS16_ChosenSSS_histo$mids, y=WS16_ChosenSSS_histo$counts,labels = WS16_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,10,2),labels=seq(0,10,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS17 Chosen SSS
  WS17_ChosenSSS_histo<-hist(ChosenSSS_WS17sub$MeanSSS, breaks=seq(30, 38, 1), main="WS17 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS17_ChosenSSS_histo$mids, y=WS17_ChosenSSS_histo$counts,labels = WS17_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS17sub$MeanSSS) #15 NAs
  WS17_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS17 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS17_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS17_ChosenSSS_histo$mids, y=WS17_ChosenSSS_histo$counts,labels = WS17_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  
  ###########
  #WS18 Chosen SSS
  WS18_ChosenSSS_histo<-hist(ChosenSSS_WS18sub$MeanSSS, breaks=seq(30, 38, 1), main="WS18 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS18_ChosenSSS_histo$mids, y=WS18_ChosenSSS_histo$counts,labels = WS18_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS18sub$MeanSSS) #2 NAs
  WS18_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS18 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS18_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS18_ChosenSSS_histo$mids, y=WS18_ChosenSSS_histo$counts,labels = WS18_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS20 Chosen SSS
  WS20_ChosenSSS_histo<-hist(ChosenSSS_WS20sub$MeanSSS, breaks=seq(30, 38, 1), main="WS20 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS20_ChosenSSS_histo$mids, y=WS20_ChosenSSS_histo$counts,labels = WS20_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS20sub$MeanSSS) #2 NAs
  WS20_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS20 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS20_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS20_ChosenSSS_histo$mids, y=WS20_ChosenSSS_histo$counts,labels = WS20_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS21 Chosen SSS
  WS21_ChosenSSS_histo<-hist(ChosenSSS_WS21sub$MeanSSS, breaks=seq(30, 38, 1), main="WS21 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS21_ChosenSSS_histo$mids, y=WS21_ChosenSSS_histo$counts,labels = WS21_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  summary(ChosenSSS_WS21sub$MeanSSS) #2 NAs
  WS21_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS21 Salinity Preference")
  axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS21_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS21_ChosenSSS_histo$mids, y=WS21_ChosenSSS_histo$counts,labels = WS21_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
}

##turn each individual into percentages
###########
{
par(mfrow = c(2, 2))
#WS1 Chosen SSS
WS1_ChosenSSS_histo_percent<-hist(ChosenSSS_WS1sub$MeanSSS, breaks=seq(30, 38, 1), main="WS1 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS1_ChosenSSS_histo_percent$density<-WS1_ChosenSSS_histo_percent$counts/sum(WS1_ChosenSSS_histo_percent$counts)*100
WS1_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS1_ChosenSSS_histo_percent, freq=FALSE, main="WS1 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS2 Chosen SSS
WS2_ChosenSSS_histo_percent<-hist(ChosenSSS_WS2sub$MeanSSS, breaks=seq(30, 38, 1), main="WS2 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS2_ChosenSSS_histo_percent$density<-WS2_ChosenSSS_histo_percent$counts/sum(WS2_ChosenSSS_histo_percent$counts)*100
WS2_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS2_ChosenSSS_histo_percent, freq=FALSE, main="WS2 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS3 Chosen SSS
WS3_ChosenSSS_histo_percent<-hist(ChosenSSS_WS3sub$MeanSSS, breaks=seq(30, 38, 1), main="WS3 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS3_ChosenSSS_histo_percent$density<-WS3_ChosenSSS_histo_percent$counts/sum(WS3_ChosenSSS_histo_percent$counts)*100
WS3_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS3_ChosenSSS_histo_percent, freq=FALSE,  main="WS3 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS4 Chosen SSS
WS4_ChosenSSS_histo_percent<-hist(ChosenSSS_WS4sub$MeanSSS, breaks=seq(30, 38, 1), main="WS4 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS4_ChosenSSS_histo_percent$density<-WS4_ChosenSSS_histo_percent$counts/sum(WS4_ChosenSSS_histo_percent$counts)*100
WS4_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS4_ChosenSSS_histo_percent, freq=FALSE,  main="WS4 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS5 Chosen SSS
WS5_ChosenSSS_histo_percent<-hist(ChosenSSS_WS5sub$MeanSSS, breaks=seq(30, 38, 1), main="WS5 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS5_ChosenSSS_histo_percent$density<-WS5_ChosenSSS_histo_percent$counts/sum(WS5_ChosenSSS_histo_percent$counts)*100
WS5_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS5_ChosenSSS_histo_percent, freq=FALSE, main="WS5 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS6 Chosen SSS
WS6_ChosenSSS_histo_percent<-hist(ChosenSSS_WS6sub$MeanSSS, breaks=seq(30, 38, 1), main="WS6 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS6_ChosenSSS_histo_percent$density<-WS6_ChosenSSS_histo_percent$counts/sum(WS6_ChosenSSS_histo_percent$counts)*100
WS6_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS6_ChosenSSS_histo_percent, freq=FALSE, main="WS6 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS7 Chosen SSS
WS7_ChosenSSS_histo_percent<-hist(ChosenSSS_WS7sub$MeanSSS, breaks=seq(30, 38, 1), main="WS7 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS7_ChosenSSS_histo_percent$density<-WS7_ChosenSSS_histo_percent$counts/sum(WS7_ChosenSSS_histo_percent$counts)*100
WS7_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS7_ChosenSSS_histo_percent, freq=FALSE, main="WS7 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS8 Chosen SSS
WS8_ChosenSSS_histo_percent<-hist(ChosenSSS_WS8sub$MeanSSS, breaks=seq(30, 38, 1), main="WS8 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS8_ChosenSSS_histo_percent$density<-WS8_ChosenSSS_histo_percent$counts/sum(WS8_ChosenSSS_histo_percent$counts)*100
WS8_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS8_ChosenSSS_histo_percent, freq=FALSE, main="WS8 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS9 Chosen SSS
WS9_ChosenSSS_histo_percent<-hist(ChosenSSS_WS9sub$MeanSSS, breaks=seq(30, 38, 1), main="WS9 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS9_ChosenSSS_histo_percent$density<-WS9_ChosenSSS_histo_percent$counts/sum(WS9_ChosenSSS_histo_percent$counts)*100
WS9_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS9_ChosenSSS_histo_percent, freq=FALSE, main="WS9 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS10 Chosen SSS
WS10_ChosenSSS_histo_percent<-hist(ChosenSSS_WS10sub$MeanSSS, breaks=seq(30, 38, 1), main="WS10 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS10_ChosenSSS_histo_percent$density<-WS10_ChosenSSS_histo_percent$counts/sum(WS10_ChosenSSS_histo_percent$counts)*100
WS10_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS10_ChosenSSS_histo_percent, freq=FALSE, main="WS10 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS11 Chosen SSS
WS11_ChosenSSS_histo_percent<-hist(ChosenSSS_WS11sub$MeanSSS, breaks=seq(30, 38, 1), main="WS11 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS11_ChosenSSS_histo_percent$density<-WS11_ChosenSSS_histo_percent$counts/sum(WS11_ChosenSSS_histo_percent$counts)*100
WS11_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS11_ChosenSSS_histo_percent, freq=FALSE, main="WS11 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS12 Chosen SSS
WS12_ChosenSSS_histo_percent<-hist(ChosenSSS_WS12sub$MeanSSS, breaks=seq(30, 38, 1), main="WS12 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS12_ChosenSSS_histo_percent$density<-WS12_ChosenSSS_histo_percent$counts/sum(WS12_ChosenSSS_histo_percent$counts)*100
WS12_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS12_ChosenSSS_histo_percent, freq=FALSE, main="WS12 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS13 Chosen SSS
WS13_ChosenSSS_histo_percent<-hist(ChosenSSS_WS13sub$MeanSSS, breaks=seq(30, 38, 1), main="WS13 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS13_ChosenSSS_histo_percent$density<-WS13_ChosenSSS_histo_percent$counts/sum(WS13_ChosenSSS_histo_percent$counts)*100
WS13_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS13_ChosenSSS_histo_percent, freq=FALSE, main="WS13 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS14 Chosen SSS
WS14_ChosenSSS_histo_percent<-hist(ChosenSSS_WS14sub$MeanSSS, breaks=seq(30, 38, 1), main="WS14 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS14_ChosenSSS_histo_percent$density<-WS14_ChosenSSS_histo_percent$counts/sum(WS14_ChosenSSS_histo_percent$counts)*100
WS14_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS14_ChosenSSS_histo_percent, freq=FALSE, main="WS14 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS15 Chosen SSS
WS15_ChosenSSS_histo_percent<-hist(ChosenSSS_WS15sub$MeanSSS, breaks=seq(30, 38, 1), main="WS15 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS15_ChosenSSS_histo_percent$density<-WS15_ChosenSSS_histo_percent$counts/sum(WS15_ChosenSSS_histo_percent$counts)*100
WS15_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS15_ChosenSSS_histo_percent, freq=FALSE, main="WS15 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS16 Chosen SSS
WS16_ChosenSSS_histo_percent<-hist(ChosenSSS_WS16sub$MeanSSS, breaks=seq(30, 38, 1), main="WS16 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS16_ChosenSSS_histo_percent$density<-WS16_ChosenSSS_histo_percent$counts/sum(WS16_ChosenSSS_histo_percent$counts)*100
WS16_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS16_ChosenSSS_histo_percent, freq=FALSE, main="WS16 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS17 Chosen SSS
WS17_ChosenSSS_histo_percent<-hist(ChosenSSS_WS17sub$MeanSSS, breaks=seq(30, 38, 1), main="WS17 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS17_ChosenSSS_histo_percent$density<-WS17_ChosenSSS_histo_percent$counts/sum(WS17_ChosenSSS_histo_percent$counts)*100
WS17_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS17_ChosenSSS_histo_percent, freq=FALSE, main="WS17 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS18 Chosen SSS
WS18_ChosenSSS_histo_percent<-hist(ChosenSSS_WS18sub$MeanSSS, breaks=seq(30, 38, 1), main="WS18 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS18_ChosenSSS_histo_percent$density<-WS18_ChosenSSS_histo_percent$counts/sum(WS18_ChosenSSS_histo_percent$counts)*100
WS18_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS18_ChosenSSS_histo_percent, freq=FALSE, main="WS18 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS20 Chosen SSS
WS20_ChosenSSS_histo_percent<-hist(ChosenSSS_WS20sub$MeanSSS, breaks=seq(30, 38, 1), main="WS20 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS20_ChosenSSS_histo_percent$density<-WS20_ChosenSSS_histo_percent$counts/sum(WS20_ChosenSSS_histo_percent$counts)*100
WS20_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS20_ChosenSSS_histo_percent, freq=FALSE, main="WS20 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)

#WS21 Chosen SSS
WS21_ChosenSSS_histo_percent<-hist(ChosenSSS_WS21sub$MeanSSS, breaks=seq(30, 38, 1), main="WS21 Chosen SSS \nAll Years", xlab="SSS (ppt)", col=orange_tr, xaxt="n", ylim=c(0,110))
WS21_ChosenSSS_histo_percent$density<-WS21_ChosenSSS_histo_percent$counts/sum(WS21_ChosenSSS_histo_percent$counts)*100
WS21_ChosenSSS_histo_percent$density
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,32))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,32,4), labels=seq(0,32,4), col="red4", col.axis="red4")
par(new=T)
plot(WS21_ChosenSSS_histo_percent, freq=FALSE, main="WS21 Chosen SSS \nAll Years", col=orange_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
}


#plot individuals on same graph to determine outliers  
#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity", freq=FALSE, ylab="Percentage", ylim=c(0,100))
axis(side=1, at=seq(30, 38, 1), labels=seq(30, 38, 1), las=2)
axis(side=2, at=seq(0,100,10), labels=seq(0,100,10), col="red4", col.axis="red4")
par(new=T)
plot(WS1_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
axis(side = 4, at=seq(0,100,10),labels=seq(0,100,10), col = "orange", col.axis = "orange", ylab = "Percentage")
par(new=T)
plot(WS2_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS3_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS4_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS5_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS6_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS7_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS8_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS9_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS10_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS11_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS12_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS13_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS14_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS15_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS16_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS17_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS18_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS20_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))
par(new=T)
plot(WS21_ChosenSSS_histo_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,100))


################################################################################################################################################
##SSS ***********2 ppt interval*********

#Available SSS all years 
SSSAllData=read.csv("All_SPOTACT_Rerddap_SSSPoly_Results_16_19.csv") #set working directory first to recognize file

AvailSSSAllhisto<-hist(SSSAllData$Salinity, breaks=seq(30, 38, 2), main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,2600))
text(x=AvailSSSAllhisto$mids, y=AvailSSSAllhisto$counts,labels = AvailSSSAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38,2), labels=seq(30, 38, 2), las=2)
summary(SSSAllData$Salinity) # 145641 NAs
AvailSSSAllhisto #max count: 1399


#Available SSS all years PERCENTAGES
AvailSSSAllhisto_percent<-hist(SSSAllData$Salinity, breaks=seq(30, 38, 2), main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,2600))
AvailSSSAllhisto_percent$density<-AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100
AvailSSSAllhisto_percent$density
summary(SSSAllData$Salinity) # 117780 NAs
AvailSSSAllhisto_percent #max count: 
plot(AvailSSSAllhisto_percent, freq=FALSE, ylab="Percentage", main="Available SSS All Years", xlab="SSS (ppt)", col="red4", xaxt="n", ylim=c(0,57))
text(x=AvailSSSAllhisto_percent$mids, y=AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100,labels = AvailSSSAllhisto_percent$counts/sum(AvailSSSAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38,2), labels=seq(30, 38, 2), las=2)

#chosen SSS all years
library(readxl)
All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("DS_Bathy_SST_SSS_Chla_16_19.xlsx")

#subset by WS number
ChosenSSS_WS1sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==1)
ChosenSSS_WS2sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==2)
ChosenSSS_WS3sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==3)
ChosenSSS_WS4sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==4)
ChosenSSS_WS5sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==5)
ChosenSSS_WS6sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==6)
ChosenSSS_WS7sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==7)
ChosenSSS_WS8sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==8)
ChosenSSS_WS9sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==9)
ChosenSSS_WS10sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==10)
ChosenSSS_WS11sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==11)
ChosenSSS_WS12sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==12)
ChosenSSS_WS13sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==13)
ChosenSSS_WS14sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==14)
ChosenSSS_WS15sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==15)
ChosenSSS_WS16sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==16)
ChosenSSS_WS17sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==17)
ChosenSSS_WS18sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==18)
ChosenSSS_WS20sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==20)
ChosenSSS_WS21sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==21)
###########
#All Chosen SSS
ChosenSSSAllhisto<-hist(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS, breaks=seq(30, 38, 2), main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,80))
text(x=ChosenSSSAllhisto$mids, y=ChosenSSSAllhisto$counts,labels = ChosenSSSAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
summary(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS) #1070 NAs
ChosenSSSAllhisto  #max count: 66

#Chosen SSS all years PERCENTAGES
ChosenSSSAllhisto_percent<-hist(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS, breaks=seq(30, 38, 2), main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,170))
ChosenSSSAllhisto_percent$density<-ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100
ChosenSSSAllhisto_percent$density
summary(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS) #3627 NAs
ChosenSSSAllhisto_percent #max count: 
plot(ChosenSSSAllhisto_percent, freq=FALSE, ylab="Percentage",  main="All Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,70))
text(x=ChosenSSSAllhisto_percent$mids, y=ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100,labels = ChosenSSSAllhisto_percent$counts/sum(ChosenSSSAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)

#two axes - 1:1 ratio TRANSPARENT
plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity")
axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
axis(side=2, at=seq(0,2600,100), labels=seq(0,2600,100), col="red4", col.axis="red4")
par(new=T)
plot(ChosenSSSAllhisto, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
axis(side = 4, at=seq(0,80,5),labels=seq(0,80,5), col = "orange", col.axis = "orange", ylab = "Frequency")
legend(36,80, legend=c("Available", "Actual"), col=c("red4", "orange"), pch=15:15, cex=0.9)

#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailSSSAllhisto_percent, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="Available vs Actual Sea Surface Salinity", freq=FALSE, ylab="Percentage", ylim=c(0,58))
axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
axis(side=2, at=seq(0,58,4), labels=seq(0,58,4), col="red4", col.axis="red4")
par(new=T)
plot(ChosenSSSAllhisto_percent, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,52))
axis(side = 4, at=seq(0,52,2),labels=seq(0,52,2), col = "orange", col.axis = "orange")
legend(36,50, legend=c("Available", "Actual"), col=c("red4", "orange"), pch=15:15, cex=0.9)

{
  ###########
  #WS1 Chosen SSS
  WS1_ChosenSSS_histo<-hist(ChosenSSS_WS1sub$MeanSSS, breaks=seq(30, 38, 2), main="WS1 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS1_ChosenSSS_histo$mids, y=WS1_ChosenSSS_histo$counts,labels = WS1_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS1sub$MeanSSS) #85 NAs
  WS1_ChosenSSS_histo  #max count: 10
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS1 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS1_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS1_ChosenSSS_histo$mids, y=WS1_ChosenSSS_histo$counts,labels = WS1_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,10,2),labels=seq(0,10,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS2 Chosen SSS
  WS2_ChosenSSS_histo<-hist(ChosenSSS_WS2sub$MeanSSS, breaks=seq(30, 38, 2), main="WS2 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS2_ChosenSSS_histo$mids, y=WS2_ChosenSSS_histo$counts,labels = WS2_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS2sub$MeanSSS) #59 NAs
  WS2_ChosenSSS_histo  #max count: 29
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS2 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS2_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS2_ChosenSSS_histo$mids, y=WS2_ChosenSSS_histo$counts,labels = WS2_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,30,5),labels=seq(0,30,5), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS3 Chosen SSS
  WS3_ChosenSSS_histo<-hist(ChosenSSS_WS3sub$MeanSSS, breaks=seq(30, 38, 2), main="WS3 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS3_ChosenSSS_histo$mids, y=WS3_ChosenSSS_histo$counts,labels = WS3_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS3sub$MeanSSS) #21 NAs
  WS3_ChosenSSS_histo  #max count: 8
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS3 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS3_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS3_ChosenSSS_histo$mids, y=WS3_ChosenSSS_histo$counts,labels = WS3_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,8,2),labels=seq(0,8,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS4 Chosen SSS
  WS4_ChosenSSS_histo<-hist(ChosenSSS_WS4sub$MeanSSS, breaks=seq(30, 38, 2), main="WS4 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS4_ChosenSSS_histo$mids, y=WS4_ChosenSSS_histo$counts,labels = WS4_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS4sub$MeanSSS) #39 NAs
  WS4_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS4 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS4_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS4_ChosenSSS_histo$mids, y=WS4_ChosenSSS_histo$counts,labels = WS4_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS5 Chosen SSS
  WS5_ChosenSSS_histo<-hist(ChosenSSS_WS5sub$MeanSSS, breaks=seq(30, 38, 2), main="WS5 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS5_ChosenSSS_histo$mids, y=WS5_ChosenSSS_histo$counts,labels = WS5_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS5sub$MeanSSS) #361 NAs
  WS5_ChosenSSS_histo  #max count: 50
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS5 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS5_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS5_ChosenSSS_histo$mids, y=WS5_ChosenSSS_histo$counts,labels = WS5_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,50,10),labels=seq(0,50,10), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS6 Chosen SSS
  WS6_ChosenSSS_histo<-hist(ChosenSSS_WS6sub$MeanSSS, breaks=seq(30, 38, 2), main="WS6 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS6_ChosenSSS_histo$mids, y=WS6_ChosenSSS_histo$counts,labels = WS6_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS6sub$MeanSSS) #502 NAs
  WS6_ChosenSSS_histo  #max count: 14
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS6 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS6_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS6_ChosenSSS_histo$mids, y=WS6_ChosenSSS_histo$counts,labels = WS6_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS7 Chosen SSS
  WS7_ChosenSSS_histo<-hist(ChosenSSS_WS7sub$MeanSSS, breaks=seq(30, 38, 2), main="WS7 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS7_ChosenSSS_histo$mids, y=WS7_ChosenSSS_histo$counts,labels = WS7_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS7sub$MeanSSS) #251 NAs
  WS7_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS7 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS7_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS7_ChosenSSS_histo$mids, y=WS7_ChosenSSS_histo$counts,labels = WS7_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,01),labels=seq(0,1,01), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS8 Chosen SSS
  WS8_ChosenSSS_histo<-hist(ChosenSSS_WS8sub$MeanSSS, breaks=seq(30, 38, 2), main="WS8 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS8_ChosenSSS_histo$mids, y=WS8_ChosenSSS_histo$counts,labels = WS8_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS8sub$MeanSSS) #364 NAs
  WS8_ChosenSSS_histo  #max count: 39
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS8 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS8_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS8_ChosenSSS_histo$mids, y=WS8_ChosenSSS_histo$counts,labels = WS8_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,40,5),labels=seq(0,40,5), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS9 Chosen SSS
  WS9_ChosenSSS_histo<-hist(ChosenSSS_WS9sub$MeanSSS, breaks=seq(30, 38, 2), main="WS9 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS9_ChosenSSS_histo$mids, y=WS9_ChosenSSS_histo$counts,labels = WS9_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS9sub$MeanSSS) #20 NAs
  WS9_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS9 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS9_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS9_ChosenSSS_histo$mids, y=WS9_ChosenSSS_histo$counts,labels = WS9_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS10 Chosen SSS
  WS10_ChosenSSS_histo<-hist(ChosenSSS_WS10sub$MeanSSS, breaks=seq(30, 38, 2), main="WS10 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS10_ChosenSSS_histo$mids, y=WS10_ChosenSSS_histo$counts,labels = WS10_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS10sub$MeanSSS) #142 NAs
  WS10_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS10 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS10_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS10_ChosenSSS_histo$mids, y=WS10_ChosenSSS_histo$counts,labels = WS10_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,01),labels=seq(0,1,01), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS11 Chosen SSS
  WS11_ChosenSSS_histo<-hist(ChosenSSS_WS11sub$MeanSSS, breaks=seq(30, 38, 2), main="WS11 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS11_ChosenSSS_histo$mids, y=WS11_ChosenSSS_histo$counts,labels = WS11_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS11sub$MeanSSS) #61 NAs
  WS11_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS11 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS11_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS11_ChosenSSS_histo$mids, y=WS11_ChosenSSS_histo$counts,labels = WS11_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS12 Chosen SSS
  WS12_ChosenSSS_histo<-hist(ChosenSSS_WS12sub$MeanSSS, breaks=seq(30, 38, 2), main="WS12 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS12_ChosenSSS_histo$mids, y=WS12_ChosenSSS_histo$counts,labels = WS12_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS12sub$MeanSSS) #16 NAs
  WS12_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS12 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS12_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS12_ChosenSSS_histo$mids, y=WS12_ChosenSSS_histo$counts,labels = WS12_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS13 Chosen SSS
  WS13_ChosenSSS_histo<-hist(ChosenSSS_WS13sub$MeanSSS, breaks=seq(30, 38, 2), main="WS13 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS13_ChosenSSS_histo$mids, y=WS13_ChosenSSS_histo$counts,labels = WS13_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS13sub$MeanSSS) #1276 NAs
  WS13_ChosenSSS_histo  #max count: 15
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS13 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS13_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS13_ChosenSSS_histo$mids, y=WS13_ChosenSSS_histo$counts,labels = WS13_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,15,3),labels=seq(0,15,3), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS14 Chosen SSS
  WS14_ChosenSSS_histo<-hist(ChosenSSS_WS14sub$MeanSSS, breaks=seq(30, 38, 2), main="WS14 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS14_ChosenSSS_histo$mids, y=WS14_ChosenSSS_histo$counts,labels = WS14_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS14sub$MeanSSS) #314 NAs
  WS14_ChosenSSS_histo  #max count: 14
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS14 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS14_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS14_ChosenSSS_histo$mids, y=WS14_ChosenSSS_histo$counts,labels = WS14_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS15 Chosen SSS
  WS15_ChosenSSS_histo<-hist(ChosenSSS_WS15sub$MeanSSS, breaks=seq(30, 38, 2), main="WS15 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS15_ChosenSSS_histo$mids, y=WS15_ChosenSSS_histo$counts,labels = WS15_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS15sub$MeanSSS) #62 NAs
  WS15_ChosenSSS_histo  #max count: 2
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS15 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS15_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS15_ChosenSSS_histo$mids, y=WS15_ChosenSSS_histo$counts,labels = WS15_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS16 Chosen SSS
  WS16_ChosenSSS_histo<-hist(ChosenSSS_WS16sub$MeanSSS, breaks=seq(30, 38, 2), main="WS16 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS16_ChosenSSS_histo$mids, y=WS16_ChosenSSS_histo$counts,labels = WS16_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS16sub$MeanSSS) #37 NAs
  WS16_ChosenSSS_histo  #max count: 10
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS16 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS16_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS16_ChosenSSS_histo$mids, y=WS16_ChosenSSS_histo$counts,labels = WS16_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,10,2),labels=seq(0,10,2), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS17 Chosen SSS
  WS17_ChosenSSS_histo<-hist(ChosenSSS_WS17sub$MeanSSS, breaks=seq(30, 38, 2), main="WS17 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS17_ChosenSSS_histo$mids, y=WS17_ChosenSSS_histo$counts,labels = WS17_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS17sub$MeanSSS) #15 NAs
  WS17_ChosenSSS_histo  #max count: 0
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS17 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS17_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS17_ChosenSSS_histo$mids, y=WS17_ChosenSSS_histo$counts,labels = WS17_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  
  ###########
  #WS18 Chosen SSS
  WS18_ChosenSSS_histo<-hist(ChosenSSS_WS18sub$MeanSSS, breaks=seq(30, 38, 2), main="WS18 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS18_ChosenSSS_histo$mids, y=WS18_ChosenSSS_histo$counts,labels = WS18_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS18sub$MeanSSS) #2 NAs
  WS18_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS18 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS18_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS18_ChosenSSS_histo$mids, y=WS18_ChosenSSS_histo$counts,labels = WS18_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS20 Chosen SSS
  WS20_ChosenSSS_histo<-hist(ChosenSSS_WS20sub$MeanSSS, breaks=seq(30, 38, 2), main="WS20 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS20_ChosenSSS_histo$mids, y=WS20_ChosenSSS_histo$counts,labels = WS20_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS20sub$MeanSSS) #2 NAs
  WS20_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS20 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS20_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS20_ChosenSSS_histo$mids, y=WS20_ChosenSSS_histo$counts,labels = WS20_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
  ###########
  #WS21 Chosen SSS
  WS21_ChosenSSS_histo<-hist(ChosenSSS_WS21sub$MeanSSS, breaks=seq(30, 38, 2), main="WS21 Chosen SSS \nAll Years", xlab="SSS (ppt)", col="orange", xaxt="n", ylim=c(0,110))
  text(x=WS21_ChosenSSS_histo$mids, y=WS21_ChosenSSS_histo$counts,labels = WS21_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  summary(ChosenSSS_WS21sub$MeanSSS) #2 NAs
  WS21_ChosenSSS_histo  #max count: 1
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSSAllhisto, col="red4", xaxt="n", yaxt="n", xlab="Sea Surface Salinity (ppt)", main="WS21 Salinity Preference")
  axis(side=1, at=seq(30, 38, 2), labels=seq(30, 38, 2), las=2)
  axis(side=2, at=seq(0,625,50), labels=seq(0,625,50), col="red4", col.axis="red4")
  par(new=T)
  plot(WS21_ChosenSSS_histo, col=orange_tr, main="", xaxt="n", yaxt="n", xlab="")
  text(x=WS21_ChosenSSS_histo$mids, y=WS21_ChosenSSS_histo$counts,labels = WS21_ChosenSSS_histo$counts, cex = .6, pos = 3)
  axis(side = 4, at=seq(0,1,1),labels=seq(0,1,1), col = "orange", col.axis = "orange", ylab = "Frequency")
  
}

#################################################################################################################################################
################################################################################################################################################
###Chla ********2 mgm-3 interval*************

#Chla all
ChlaAllData=read.csv("All_SPOTACT_Rerddap_ChlaPoly_Results_16_19.csv") 

#chla results too wide of range - possible error with satellite
AvailChlaAllhisto<-hist(ChlaAllData$Chla, breaks=seq(0, 100, 2), main="Available Chla All Years", xlab="Chlorophyll (mg^m-3)", col="darkgreen", xaxt="n", yaxt="n", ylim=c(0,65000))
axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
text(x=AvailChlaAllhisto$mids, y=AvailChlaAllhisto$counts,labels = AvailChlaAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(0,100, 5), labels=seq(0,100, 5), las=2)

##so subset to chla < 20 (what is found in ocean)
SubsetChla<-subset(ChlaAllData, ChlaAllData$Chla<=20)

#Available Chla all years 
AvailChlaAllhisto<-hist(SubsetChla$Chla, breaks=seq(0, 20, 2), main="Available Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="darkgreen", xaxt="n", yaxt="n", ylim=c(0,200000))
axis(side=2, at=seq(0, 200000, 10000), labels=seq(0, 200000, 10000), las=2)
text(x=AvailChlaAllhisto$mids, y=AvailChlaAllhisto$counts,labels = AvailChlaAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(0,20, 2), labels=seq(0,20, 2), las=2)
summary(ChlaAllData$Chla) # 5084040 NAs
AvailChlaAllhisto #max count: 190799

#Available Chla all years PERCENTAGES
AvailChlaAllhisto_percent<-hist(SubsetChla$Chla, breaks=seq(0, 20, 2), main="Available Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="darkgreen", xaxt="n", yaxt="n", ylim=c(0,150000))
AvailChlaAllhisto_percent$density<-AvailChlaAllhisto_percent$counts/sum(AvailChlaAllhisto_percent$counts)*100
AvailChlaAllhisto_percent$density
summary(SSSAllData$Salinity) # 117780 NAs
AvailChlaAllhisto_percent #max count: 
plot(AvailChlaAllhisto_percent, freq=FALSE, ylab="Percentage", main="Available Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="darkgreen", xaxt="n", ylim=c(0,80))
text(x=AvailChlaAllhisto_percent$mids, y=AvailChlaAllhisto_percent$counts/sum(AvailChlaAllhisto_percent$counts)*100,labels = AvailChlaAllhisto_percent$counts/sum(AvailChlaAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(0,20, 2), labels=seq(0,20, 2), las=2)

#Chosen Chla all years
library(readxl)
All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("DS_Bathy_SST_SSS_Chla_16_19.xlsx")
SubsetChlaChosen<-subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$Chla_mgmcubed<20)

ChosenChlaAllhisto<-hist(SubsetChlaChosen$Chla_mgmcubed, breaks=seq(0,20,2), main="Chosen Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,250))
text(x=ChosenChlaAllhisto$mids, y=ChosenChlaAllhisto$counts,labels = ChosenChlaAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(0,20, 2), labels=seq(0,20, 2), las=2)
summary(All_SPOTACT_BathySSTChlaSSS_Results$Chla_mgmcubed) #2060 NAs
summary(SubsetChlaChosen$Chla_mgmcubed) #2060 NAs
ChosenChlaAllhisto  #max count: 407

#Chosen Chla all years PERCENTAGES
ChosenChlaAllhisto_percent<-hist(SubsetChlaChosen$Chla_mgmcubed, breaks=seq(0,20,2), main="Chosen Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,700))
ChosenChlaAllhisto_percent$density<-ChosenChlaAllhisto_percent$counts/sum(ChosenChlaAllhisto_percent$counts)*100
ChosenChlaAllhisto_percent$density
summary(SubsetChlaChosen$Chla_mgmcubed) # 117780 NAs
ChosenChlaAllhisto_percent #max count: 
plot(ChosenChlaAllhisto_percent, freq=FALSE, ylab="Percentage", main="Chosen Chla All Years \n(2 mg^m-3 increment)", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,45))
text(x=ChosenChlaAllhisto_percent$mids, y=ChosenChlaAllhisto_percent$counts/sum(ChosenChlaAllhisto_percent$counts)*100,labels = ChosenChlaAllhisto_percent$counts/sum(ChosenChlaAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(0,20, 2), labels=seq(0,20, 2), las=2)


#plot all
#two axes - 1:1 ratio TRANSPARENT
plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="Available vs Actual Chlorophyll a", ylim=c(0, 200000))
axis(side=1, at=seq(0,20, 2), labels=seq(0,20, 2), las=2)
axis(side=2, at=seq(0, 200000, 10000), labels=c("0","10k", "20k", "30k", "40k","50k","60k", "70k", "80k", "90k", "100k", "110k", "120k", "130", "140", "150k", "160k", "170k", "180k", "190k", "200k"), las=2, col = "darkgreen", col.axis = "darkgreen")
par(new=T)
plot(ChosenChlaAllhisto, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
axis(side = 4, at=seq(0,200,50),labels=seq(0,200,50), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
legend(15, 175, legend=c("Available", "Actual"), col=c("darkgreen", "limegreen"), pch=15:15, cex=0.9)

#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="Available vs Actual Chlorophyll a", freq=FALSE, ylab="Percentage", ylim=c(0,78))
axis(side=1, at=seq(0.0,20.0, 2.0), labels=c("0.0", "2.0", "4.0", "6.0", "8.0", "10.0", "12.0", "14.0", "16.0", "18.0", "20.0"), las=2)
axis(side=2, at=seq(0, 78, 4), labels=seq(0, 78, 4), las=2, col = "darkgreen", col.axis = "darkgreen")
par(new=T)
plot(ChosenChlaAllhisto_percent, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,37))
axis(side = 4, at=seq(0,36,2),labels=seq(0,36,2), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
legend(15, 36, legend=c("Available", "Actual"), col=c("darkgreen", "limegreen"), pch=15:15, cex=0.9)

#two axes - 100 vs 100 ratio TRANSPARENT PERCENTAGE
plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="Available vs Actual Chlorophyll a", freq=FALSE, ylab="Percentage", ylim=c(0,80))
axis(side=1, at=seq(0.0,20.0, 2.0), labels=c("0.0", "2.0", "4.0", "6.0", "8.0", "10.0", "12.0", "14.0", "16.0", "18.0", "20.0"), las=2)
axis(side=2, at=seq(0, 80, 5), labels=seq(0, 80, 5), las=2, col = "darkgreen", col.axis = "darkgreen")
par(new=T)
plot(ChosenChlaAllhisto_percent, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", ylim=c(0,80))
axis(side = 4, at=seq(0,80,5),labels=seq(0,80,5), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
legend(14, 75, legend=c("Available", "Actual"), col=c("darkgreen", "limegreen"), pch=15:15, cex=0.9)

#subset by WS number
ChosenChla_WS1sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==1)
ChosenChla_WS2sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==2)
ChosenChla_WS3sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==3)
ChosenChla_WS4sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==4)
ChosenChla_WS5sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==5)
ChosenChla_WS6sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==6)
ChosenChla_WS7sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==7)
ChosenChla_WS8sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==8)
ChosenChla_WS9sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==9)
ChosenChla_WS10sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==10)
ChosenChla_WS11sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==11)
ChosenChla_WS12sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==12)
ChosenChla_WS13sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==13)
ChosenChla_WS14sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==14)
ChosenChla_WS15sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==15)
ChosenChla_WS16sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==16)
ChosenChla_WS17sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==17)
ChosenChla_WS18sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==18)
ChosenChla_WS20sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==20)
ChosenChla_WS21sub <- subset(SubsetChlaChosen, SubsetChlaChosen$WSNumber==21)
{
  ######################
  #WS1 Chosen Chla
  WS1_ChosenChla_histo<-hist(ChosenChla_WS1sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS1 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,15))
  text(x=WS1_ChosenChla_histo$mids, y=WS1_ChosenChla_histo$counts,labels = WS1_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS1sub$Chla_mgmcubed) # NAs
  WS1_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS1 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS1_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS1_ChosenChla_histo$mids, y=WS1_ChosenChla_histo$counts,labels = WS1_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS2 Chosen Chla
  WS2_ChosenChla_histo<-hist(ChosenChla_WS2sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS2 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,25))
  text(x=WS2_ChosenChla_histo$mids, y=WS2_ChosenChla_histo$counts,labels = WS2_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS2sub$Chla_mgmcubed) # NAs
  WS2_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS2 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS2_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,24,4),labels=seq(0,24,4), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS2_ChosenChla_histo$mids, y=WS2_ChosenChla_histo$counts,labels = WS2_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS3 Chosen Chla
  WS3_ChosenChla_histo<-hist(ChosenChla_WS3sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS3 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,5))
  text(x=WS3_ChosenChla_histo$mids, y=WS3_ChosenChla_histo$counts,labels = WS3_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS3sub$Chla_mgmcubed) # NAs
  WS3_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS3 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS3_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,4,1),labels=seq(0,4,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS3_ChosenChla_histo$mids, y=WS3_ChosenChla_histo$counts,labels = WS3_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS4 Chosen Chla
  WS4_ChosenChla_histo<-hist(ChosenChla_WS4sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS4 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,20))
  text(x=WS4_ChosenChla_histo$mids, y=WS4_ChosenChla_histo$counts,labels = WS4_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS4sub$Chla_mgmcubed) # NAs
  WS4_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS4 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS4_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,17,1),labels=seq(0,17,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS4_ChosenChla_histo$mids, y=WS4_ChosenChla_histo$counts,labels = WS4_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS5 Chosen Chla
  WS5_ChosenChla_histo<-hist(ChosenChla_WS5sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS5 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,80))
  text(x=WS5_ChosenChla_histo$mids, y=WS5_ChosenChla_histo$counts,labels = WS5_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS5sub$Chla_mgmcubed) # NAs
  WS5_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS5 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS5_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,78,4),labels=seq(0,78,4), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS5_ChosenChla_histo$mids, y=WS5_ChosenChla_histo$counts,labels = WS5_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS6 Chosen Chla
  WS6_ChosenChla_histo<-hist(ChosenChla_WS6sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS6 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,300))
  text(x=WS6_ChosenChla_histo$mids, y=WS6_ChosenChla_histo$counts,labels = WS6_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS6sub$Chla_mgmcubed) # NAs
  WS6_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS6 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS6_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,300,10),labels=seq(0,300,10), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS6_ChosenChla_histo$mids, y=WS6_ChosenChla_histo$counts,labels = WS6_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  
  ####################
  #WS7 Chosen Chla
  WS7_ChosenChla_histo<-hist(ChosenChla_WS7sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS7 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,30))
  text(x=WS7_ChosenChla_histo$mids, y=WS7_ChosenChla_histo$counts,labels = WS7_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS7sub$Chla_mgmcubed) # NAs
  WS7_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS7 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS7_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,25,5),labels=seq(0,25,5), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS7_ChosenChla_histo$mids, y=WS7_ChosenChla_histo$counts,labels = WS7_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS8 Chosen Chla
  WS8_ChosenChla_histo<-hist(ChosenChla_WS8sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS8 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,10))
  text(x=WS8_ChosenChla_histo$mids, y=WS8_ChosenChla_histo$counts,labels = WS8_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS8sub$Chla_mgmcubed) # NAs
  WS8_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS8 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS8_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,8,2),labels=seq(0,8,2), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS8_ChosenChla_histo$mids, y=WS8_ChosenChla_histo$counts,labels = WS8_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS9 Chosen Chla
  WS9_ChosenChla_histo<-hist(ChosenChla_WS9sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS9 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,15))
  text(x=WS9_ChosenChla_histo$mids, y=WS9_ChosenChla_histo$counts,labels = WS9_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS9sub$Chla_mgmcubed) # NAs
  WS9_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS9 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS9_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,13,1),labels=seq(0,13,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS9_ChosenChla_histo$mids, y=WS9_ChosenChla_histo$counts,labels = WS9_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS10 Chosen Chla
  WS10_ChosenChla_histo<-hist(ChosenChla_WS10sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS10 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,8))
  text(x=WS10_ChosenChla_histo$mids, y=WS10_ChosenChla_histo$counts,labels = WS10_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS10sub$Chla_mgmcubed) # NAs
  WS10_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS10 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS10_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,6,1),labels=seq(0,6,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS10_ChosenChla_histo$mids, y=WS10_ChosenChla_histo$counts,labels = WS10_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS11 Chosen Chla
  WS11_ChosenChla_histo<-hist(ChosenChla_WS11sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS11 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,14))
  text(x=WS11_ChosenChla_histo$mids, y=WS11_ChosenChla_histo$counts,labels = WS11_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS11sub$Chla_mgmcubed) # NAs
  WS11_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS11 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS11_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,12,1),labels=seq(0,12,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS11_ChosenChla_histo$mids, y=WS11_ChosenChla_histo$counts,labels = WS11_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS12 Chosen Chla
  WS12_ChosenChla_histo<-hist(ChosenChla_WS12sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS12 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,6))
  text(x=WS12_ChosenChla_histo$mids, y=WS12_ChosenChla_histo$counts,labels = WS12_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS12sub$Chla_mgmcubed) # NAs
  WS12_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS12 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS12_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,5,1),labels=seq(0,5,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS12_ChosenChla_histo$mids, y=WS12_ChosenChla_histo$counts,labels = WS12_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS13 Chosen Chla
  WS13_ChosenChla_histo<-hist(ChosenChla_WS13sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS13 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,100))
  text(x=WS13_ChosenChla_histo$mids, y=WS13_ChosenChla_histo$counts,labels = WS13_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS13sub$Chla_mgmcubed) # NAs
  WS13_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS13 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS13_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,96,2),labels=seq(0,96,2), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS13_ChosenChla_histo$mids, y=WS13_ChosenChla_histo$counts,labels = WS13_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS14 Chosen Chla
  WS14_ChosenChla_histo<-hist(ChosenChla_WS14sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS14 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,35))
  text(x=WS14_ChosenChla_histo$mids, y=WS14_ChosenChla_histo$counts,labels = WS14_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS14sub$Chla_mgmcubed) # NAs
  WS14_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS14 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS14_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,32,2),labels=seq(0,32,2), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS14_ChosenChla_histo$mids, y=WS14_ChosenChla_histo$counts,labels = WS14_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS15 Chosen Chla
  WS15_ChosenChla_histo<-hist(ChosenChla_WS15sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS15 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,25))
  text(x=WS15_ChosenChla_histo$mids, y=WS15_ChosenChla_histo$counts,labels = WS15_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS15sub$Chla_mgmcubed) # NAs
  WS15_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS15 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS15_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,23,1),labels=seq(0,23,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS15_ChosenChla_histo$mids, y=WS15_ChosenChla_histo$counts,labels = WS15_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS16 Chosen Chla
  WS16_ChosenChla_histo<-hist(ChosenChla_WS16sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS16 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,15))
  text(x=WS16_ChosenChla_histo$mids, y=WS16_ChosenChla_histo$counts,labels = WS16_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS16sub$Chla_mgmcubed) # NAs
  WS16_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS16 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS16_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,11,1),labels=seq(0,11,1), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS16_ChosenChla_histo$mids, y=WS16_ChosenChla_histo$counts,labels = WS16_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS17 Chosen Chla
  WS17_ChosenChla_histo<-hist(ChosenChla_WS17sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS17 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,15))
  text(x=WS17_ChosenChla_histo$mids, y=WS17_ChosenChla_histo$counts,labels = WS17_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS17sub$Chla_mgmcubed) # NAs
  WS17_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS17 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS17_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,0,0),labels=seq(0,0,0), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS17_ChosenChla_histo$mids, y=WS17_ChosenChla_histo$counts,labels = WS17_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS18 Chosen Chla
  WS18_ChosenChla_histo<-hist(ChosenChla_WS18sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS18 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,2))
  text(x=WS18_ChosenChla_histo$mids, y=WS18_ChosenChla_histo$counts,labels = WS18_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS18sub$Chla_mgmcubed) # NAs
  WS18_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS18 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS18_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,1,.5),labels=seq(0,1,.5), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS18_ChosenChla_histo$mids, y=WS18_ChosenChla_histo$counts,labels = WS18_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)

  ####################
  #WS20 Chosen Chla
  WS20_ChosenChla_histo<-hist(ChosenChla_WS20sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS20 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,2))
  text(x=WS20_ChosenChla_histo$mids, y=WS20_ChosenChla_histo$counts,labels = WS20_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS20sub$Chla_mgmcubed) # NAs
  WS20_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS20 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS20_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,1,.5),labels=seq(0,1,.5), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS20_ChosenChla_histo$mids, y=WS20_ChosenChla_histo$counts,labels = WS20_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ####################
  #WS21 Chosen Chla
  WS21_ChosenChla_histo<-hist(ChosenChla_WS21sub$Chla_mgmcubed, breaks=seq(0,20,2), main="WS21 Chosen Chla \nAll Years", xlab="Chlorophyll (mg^m-3)", col="limegreen", xaxt="n", ylim=c(0,2))
  text(x=WS21_ChosenChla_histo$mids, y=WS21_ChosenChla_histo$counts,labels = WS21_ChosenChla_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  summary(ChosenChla_WS21sub$Chla_mgmcubed) # NAs
  WS21_ChosenChla_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailChlaAllhisto, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll (mg^m-3)", main="WS21 Chla Preference", ylim=c(0, 65000))
  axis(side=1, at=seq(0,20, 1), labels=seq(0,20, 1), las=2)
  axis(side=2, at=seq(0, 65000, 5000), labels=c("0","5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k","45k", "50k","55k", "60k", "65k"), las=2)
  par(new=T)
  plot(WS21_ChosenChla_histo, col=limegreen_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,1,.5),labels=seq(0,1,.5), col = "limegreen", col.axis = "limegreen", ylab = "Frequency")
  text(x=WS21_ChosenChla_histo$mids, y=WS21_ChosenChla_histo$counts,labels = WS21_ChosenChla_histo$counts, cex = .7, pos=3, col="black", offset=.1)
}

{
  par(mfrow = c(2, 2))
  #WS1 Chosen Chla
  WS1_ChosenChla_histo_percent<-hist(ChosenChla_WS1sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS1 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS1_ChosenChla_histo_percent$density<-WS1_ChosenChla_histo_percent$counts/sum(WS1_ChosenChla_histo_percent$counts)*100
  WS1_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS1_ChosenChla_histo_percent, freq=FALSE, main="WS1 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS2 Chosen Chla
  WS2_ChosenChla_histo_percent<-hist(ChosenChla_WS2sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS2 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS2_ChosenChla_histo_percent$density<-WS2_ChosenChla_histo_percent$counts/sum(WS2_ChosenChla_histo_percent$counts)*100
  WS2_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS2_ChosenChla_histo_percent, freq=FALSE, main="WS2 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS3 Chosen Chla
  WS3_ChosenChla_histo_percent<-hist(ChosenChla_WS3sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS3 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS3_ChosenChla_histo_percent$density<-WS3_ChosenChla_histo_percent$counts/sum(WS3_ChosenChla_histo_percent$counts)*100
  WS3_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS3_ChosenChla_histo_percent, freq=FALSE,  main="WS3 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS4 Chosen Chla
  WS4_ChosenChla_histo_percent<-hist(ChosenChla_WS4sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS4 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS4_ChosenChla_histo_percent$density<-WS4_ChosenChla_histo_percent$counts/sum(WS4_ChosenChla_histo_percent$counts)*100
  WS4_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS4_ChosenChla_histo_percent, freq=FALSE,  main="WS4 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS5 Chosen Chla
  WS5_ChosenChla_histo_percent<-hist(ChosenChla_WS5sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS5 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS5_ChosenChla_histo_percent$density<-WS5_ChosenChla_histo_percent$counts/sum(WS5_ChosenChla_histo_percent$counts)*100
  WS5_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS5_ChosenChla_histo_percent, freq=FALSE, main="WS5 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS6 Chosen Chla
  WS6_ChosenChla_histo_percent<-hist(ChosenChla_WS6sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS6 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS6_ChosenChla_histo_percent$density<-WS6_ChosenChla_histo_percent$counts/sum(WS6_ChosenChla_histo_percent$counts)*100
  WS6_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS6_ChosenChla_histo_percent, freq=FALSE, main="WS6 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS7 Chosen Chla
  WS7_ChosenChla_histo_percent<-hist(ChosenChla_WS7sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS7 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS7_ChosenChla_histo_percent$density<-WS7_ChosenChla_histo_percent$counts/sum(WS7_ChosenChla_histo_percent$counts)*100
  WS7_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS7_ChosenChla_histo_percent, freq=FALSE, main="WS7 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS8 Chosen Chla
  WS8_ChosenChla_histo_percent<-hist(ChosenChla_WS8sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS8 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS8_ChosenChla_histo_percent$density<-WS8_ChosenChla_histo_percent$counts/sum(WS8_ChosenChla_histo_percent$counts)*100
  WS8_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS8_ChosenChla_histo_percent, freq=FALSE, main="WS8 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS9 Chosen Chla
  WS9_ChosenChla_histo_percent<-hist(ChosenChla_WS9sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS9 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS9_ChosenChla_histo_percent$density<-WS9_ChosenChla_histo_percent$counts/sum(WS9_ChosenChla_histo_percent$counts)*100
  WS9_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS9_ChosenChla_histo_percent, freq=FALSE, main="WS9 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS10 Chosen Chla
  WS10_ChosenChla_histo_percent<-hist(ChosenChla_WS10sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS10 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS10_ChosenChla_histo_percent$density<-WS10_ChosenChla_histo_percent$counts/sum(WS10_ChosenChla_histo_percent$counts)*100
  WS10_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS10_ChosenChla_histo_percent, freq=FALSE, main="WS10 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS11 Chosen Chla
  WS11_ChosenChla_histo_percent<-hist(ChosenChla_WS11sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS11 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS11_ChosenChla_histo_percent$density<-WS11_ChosenChla_histo_percent$counts/sum(WS11_ChosenChla_histo_percent$counts)*100
  WS11_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS11_ChosenChla_histo_percent, freq=FALSE, main="WS11 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS12 Chosen Chla
  WS12_ChosenChla_histo_percent<-hist(ChosenChla_WS12sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS12 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS12_ChosenChla_histo_percent$density<-WS12_ChosenChla_histo_percent$counts/sum(WS12_ChosenChla_histo_percent$counts)*100
  WS12_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS12_ChosenChla_histo_percent, freq=FALSE, main="WS12 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS13 Chosen Chla
  WS13_ChosenChla_histo_percent<-hist(ChosenChla_WS13sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS13 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS13_ChosenChla_histo_percent$density<-WS13_ChosenChla_histo_percent$counts/sum(WS13_ChosenChla_histo_percent$counts)*100
  WS13_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS13_ChosenChla_histo_percent, freq=FALSE, main="WS13 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS14 Chosen Chla
  WS14_ChosenChla_histo_percent<-hist(ChosenChla_WS14sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS14 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS14_ChosenChla_histo_percent$density<-WS14_ChosenChla_histo_percent$counts/sum(WS14_ChosenChla_histo_percent$counts)*100
  WS14_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS14_ChosenChla_histo_percent, freq=FALSE, main="WS14 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS15 Chosen Chla
  WS15_ChosenChla_histo_percent<-hist(ChosenChla_WS15sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS15 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS15_ChosenChla_histo_percent$density<-WS15_ChosenChla_histo_percent$counts/sum(WS15_ChosenChla_histo_percent$counts)*100
  WS15_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS15_ChosenChla_histo_percent, freq=FALSE, main="WS15 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS16 Chosen Chla
  WS16_ChosenChla_histo_percent<-hist(ChosenChla_WS16sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS16 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS16_ChosenChla_histo_percent$density<-WS16_ChosenChla_histo_percent$counts/sum(WS16_ChosenChla_histo_percent$counts)*100
  WS16_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS16_ChosenChla_histo_percent, freq=FALSE, main="WS16 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS17 Chosen Chla
  WS17_ChosenChla_histo_percent<-hist(ChosenChla_WS17sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS17 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS17_ChosenChla_histo_percent$density<-WS17_ChosenChla_histo_percent$counts/sum(WS17_ChosenChla_histo_percent$counts)*100
  WS17_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS17_ChosenChla_histo_percent, freq=FALSE, main="WS17 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS18 Chosen Chla
  WS18_ChosenChla_histo_percent<-hist(ChosenChla_WS18sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS18 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS18_ChosenChla_histo_percent$density<-WS18_ChosenChla_histo_percent$counts/sum(WS18_ChosenChla_histo_percent$counts)*100
  WS18_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS18_ChosenChla_histo_percent, freq=FALSE, main="WS18 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS20 Chosen Chla
  WS20_ChosenChla_histo_percent<-hist(ChosenChla_WS20sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS20 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS20_ChosenChla_histo_percent$density<-WS20_ChosenChla_histo_percent$counts/sum(WS20_ChosenChla_histo_percent$counts)*100
  WS20_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS20_ChosenChla_histo_percent, freq=FALSE, main="WS20 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS21 Chosen Chla
  WS21_ChosenChla_histo_percent<-hist(ChosenChla_WS21sub$Chla_mgmcubed, breaks=seq(0, 20, 2), main="WS21 Chosen Chla \nAll Years", xlab="Chla (ppt)", col=limegreen_tr, xaxt="n", ylim=c(0,110))
  WS21_ChosenChla_histo_percent$density<-WS21_ChosenChla_histo_percent$counts/sum(WS21_ChosenChla_histo_percent$counts)*100
  WS21_ChosenChla_histo_percent$density
  plot(AvailChlaAllhisto_percent, col="darkgreen", xaxt="n", yaxt="n", xlab="Chlorophyll a (mg^m-3)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,76))
  axis(side=1, at=seq(0, 20, 2), labels=seq(0, 20, 2), las=2)
  axis(side=2, at=seq(0,76,4), labels=seq(0,76,4), col="darkgreen", col.axis="darkgreen")
  par(new=T)
  plot(WS21_ChosenChla_histo_percent, freq=FALSE, main="WS21 Chosen Chla \nAll Years", col=limegreen_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
}  
###################################################################################################################################################
###################################################################################################################################################
##SST *************2 degrees C intervals***************

#SST all
SSTAllData=read.csv("All_SPOTACT_Rerddap_SSTPoly_Results_16_19.csv") 

#Available SST all years 
AvailSSTAllhisto<-hist(SSTAllData$SST, breaks=seq(12, 30, 2), main="Available SST All Years", xlab="Sea Surface Temperature (C)", col="purple4", xaxt="n", yaxt="n", ylim=c(0, 3500000))
axis(side=2, at=seq(0, 3500000, 100000), labels=seq(0, 3500000, 100000), las=2)
text(x=AvailSSTAllhisto$mids, y=AvailSSTAllhisto$counts,labels = AvailSSTAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
summary(SSTAllData$SST) # 59471268 NAs
AvailSSTAllhisto #max count: 682719

#Available SST all years PERCENTAGES
AvailSSTAllhisto_percent<-hist(SSTAllData$SST, breaks=seq(12, 30, 2), main="Available SST All Years", xlab="Sea Surface Temperature (C)", col="purple4", xaxt="n", yaxt="n", ylim=c(0, 2500000))
AvailSSTAllhisto_percent$density<-AvailSSTAllhisto_percent$counts/sum(AvailSSTAllhisto_percent$counts)*100
AvailSSTAllhisto_percent$density
summary(SSTAllData$SST) # 117780 NAs
AvailSSTAllhisto_percent #max count: 
plot(AvailSSTAllhisto_percent, freq=FALSE, ylab="Percentage", main="Available SST All Years", xlab="Sea Surface Temperature (C)", col="purple4", xaxt="n", ylim=c(0,25))
text(x=AvailSSTAllhisto_percent$mids, y=AvailSSTAllhisto_percent$counts/sum(AvailSSTAllhisto_percent$counts)*100,labels = AvailSSTAllhisto_percent$counts/sum(AvailSSTAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)

#Chosen SST all years
library(readxl)
All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("DS_Bathy_SST_SSS_Chla_16_19.xlsx")
ChosenSSTAllhisto<-hist(All_SPOTACT_BathySSTChlaSSS_Results$Mean_SST_C, breaks=seq(12, 30, 2), main="Chosen SST All Years", xlab="SST (ppt)", col="violetred1", xaxt="n", ylim=c(0,500))
text(x=ChosenSSTAllhisto$mids, y=ChosenSSTAllhisto$counts,labels = ChosenSSTAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
summary(All_SPOTACT_BathySSTChlaSSS_Results$Mean_SST_C) #no NAs
ChosenSSTAllhisto  #max count: 570

#Chosen SST all years PERCENTAGES
ChosenSSTAllhisto_percent<-hist(All_SPOTACT_BathySSTChlaSSS_Results$Mean_SST_C, breaks=seq(12, 30, 2), main="Chosen SST All Years", xlab="SST (ppt)", col="violetred1", xaxt="n", ylim=c(0,1500))
ChosenSSTAllhisto_percent$density<-ChosenSSTAllhisto_percent$counts/sum(ChosenSSTAllhisto_percent$counts)*100
ChosenSSTAllhisto_percent$density
summary(SSTAllData$SST) # 117780 NAs
ChosenSSTAllhisto_percent #max count: 
plot(ChosenSSTAllhisto_percent, freq=FALSE, ylab="Percentage", main="Chosen SST All Years", xlab="SST (ppt)", col="violetred1", xaxt="n", ylim=c(0,40))
text(x=ChosenSSTAllhisto_percent$mids, y=ChosenSSTAllhisto_percent$counts/sum(ChosenSSTAllhisto_percent$counts)*100,labels = ChosenSSTAllhisto_percent$counts/sum(ChosenSSTAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)


#plot all
#two axes - 1:1 ratio TRANSPARENT
plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="Available vs Actual Sea Surface Temperature", ylim=c(0,3200000), ylab="Frequency (in millions)")
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
axis(side=2, at=seq(0, 3200000, 100000), labels=c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0", "", "1.2", "",
                                                  "1.4", "", "1.6", "","1.8", "", "2.0", "", "2.2", "", "2.4", "", "2.6" ,"", "2.8", "","3.0", "","3.2"), las=2, col="purple4", col.axis="purple4")
par(new=T)
plot(ChosenSSTAllhisto, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side = 4, at=seq(0,450,50),labels=seq(0,450,50), col = violetred1_tr, col.axis = "violetred1", line = -1.5)
mtext("Frequency", side = 4, line = 1)
legend(26, 425, legend=c("Available", "Actual"), col=c("purple4", "violetred1"), pch=15:15, cex=0.9)

#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="Available vs Actual Sea Surface Temperature", freq=FALSE, ylab="Percentage", ylim=c(0,25))
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
axis(side=2, at=seq(0, 25, 5), labels=seq(0, 25, 5), las=2, col="purple4", col.axis="purple4")
par(new=T)
plot(ChosenSSTAllhisto_percent, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="", ylab="", freq=FALSE, ylim=c(0,38))
axis(side = 4, at=seq(0,36,2),labels=seq(0,36,2), col = violetred1_tr, col.axis = "violetred1")
legend(26, 38, legend=c("Available", "Actual"), col=c("purple4", "violetred1"), pch=15:15, cex=0.9)

#two axes - 100 vs 100 ratio TRANSPARENT PERCENTAGE
plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="Available vs Actual Sea Surface Temperature", freq=FALSE, ylab="Percentage", ylim=c(0,40))
axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
axis(side=2, at=seq(0, 40, 5), labels=seq(0, 40, 5), las=2, col="purple4", col.axis="purple4")
par(new=T)
plot(ChosenSSTAllhisto_percent, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="", ylab="", freq=FALSE, ylim=c(0,40))
axis(side = 4, at=seq(0,40,5),labels=seq(0,40,5), col = violetred1_tr, col.axis = "violetred1")
legend(25, 35, legend=c("Available", "Actual"), col=c("purple4", "violetred1"), pch=15:15, cex=0.9)

#subset by WS number
ChosenSST_WS1sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==1)
ChosenSST_WS2sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==2)
ChosenSST_WS3sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==3)
ChosenSST_WS4sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==4)
ChosenSST_WS5sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==5)
ChosenSST_WS6sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==6)
ChosenSST_WS7sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==7)
ChosenSST_WS8sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==8)
ChosenSST_WS9sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==9)
ChosenSST_WS10sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==10)
ChosenSST_WS11sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==11)
ChosenSST_WS12sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==12)
ChosenSST_WS13sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==13)
ChosenSST_WS14sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==14)
ChosenSST_WS15sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==15)
ChosenSST_WS16sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==16)
ChosenSST_WS17sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==17)
ChosenSST_WS18sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==18)
ChosenSST_WS20sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==20)
ChosenSST_WS21sub <- subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$WSNumber==21)
{
  ######################
  #WS1 Chosen SST
  WS1_ChosenSST_histo<-hist(ChosenSST_WS1sub$Mean_SST_C, breaks=seq(12,30,2), main="WS1 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,35))
  text(x=WS1_ChosenSST_histo$mids, y=WS1_ChosenSST_histo$counts,labels = WS1_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS1sub$Mean_SST_C) # NAs
  WS1_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS1 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS1_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,32,4),labels=seq(0,32,4), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS1_ChosenSST_histo$mids, y=WS1_ChosenSST_histo$counts,labels = WS1_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS2 Chosen SST
  WS2_ChosenSST_histo<-hist(ChosenSST_WS2sub$Mean_SST_C, breaks=seq(12,30,2), main="WS2 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,30))
  text(x=WS2_ChosenSST_histo$mids, y=WS2_ChosenSST_histo$counts,labels = WS2_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS2sub$Mean_SST_C) # NAs
  WS2_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS2 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS2_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,29,1),labels=seq(0,29,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS2_ChosenSST_histo$mids, y=WS2_ChosenSST_histo$counts,labels = WS2_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS3 Chosen SST
  WS3_ChosenSST_histo<-hist(ChosenSST_WS3sub$Mean_SST_C, breaks=seq(12,30,2), main="WS3 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,15))
  text(x=WS3_ChosenSST_histo$mids, y=WS3_ChosenSST_histo$counts,labels = WS3_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS3sub$Mean_SST_C) # NAs
  WS3_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS3 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS3_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,12,1),labels=seq(0,12,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS3_ChosenSST_histo$mids, y=WS3_ChosenSST_histo$counts,labels = WS3_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS4 Chosen SST
  WS4_ChosenSST_histo<-hist(ChosenSST_WS4sub$Mean_SST_C, breaks=seq(12,30,2), main="WS4 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,20))
  text(x=WS4_ChosenSST_histo$mids, y=WS4_ChosenSST_histo$counts,labels = WS4_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS4sub$Mean_SST_C) # NAs
  WS4_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS4 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS4_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,17,1),labels=seq(0,17,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS4_ChosenSST_histo$mids, y=WS4_ChosenSST_histo$counts,labels = WS4_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS5 Chosen SST
  WS5_ChosenSST_histo<-hist(ChosenSST_WS5sub$Mean_SST_C, breaks=seq(12,30,2), main="WS5 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,200))
  text(x=WS5_ChosenSST_histo$mids, y=WS5_ChosenSST_histo$counts,labels = WS5_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS5sub$Mean_SST_C) # NAs
  WS5_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS5 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS5_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,199,5),labels=seq(0,199,5), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS5_ChosenSST_histo$mids, y=WS5_ChosenSST_histo$counts,labels = WS5_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS6 Chosen SST
  WS6_ChosenSST_histo<-hist(ChosenSST_WS6sub$Mean_SST_C, breaks=seq(12,30,2), main="WS6 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,210))
  text(x=WS6_ChosenSST_histo$mids, y=WS6_ChosenSST_histo$counts,labels = WS6_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS6sub$Mean_SST_C) # NAs
  WS6_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS6 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS6_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,210,5),labels=seq(0,210,5), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS6_ChosenSST_histo$mids, y=WS6_ChosenSST_histo$counts,labels = WS6_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS7 Chosen SST
  WS7_ChosenSST_histo<-hist(ChosenSST_WS7sub$Mean_SST_C, breaks=seq(12,30,2), main="WS7 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,120))
  text(x=WS7_ChosenSST_histo$mids, y=WS7_ChosenSST_histo$counts,labels = WS7_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS7sub$Mean_SST_C) # NAs
  WS7_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS7 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS7_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,116,2),labels=seq(0,116,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS7_ChosenSST_histo$mids, y=WS7_ChosenSST_histo$counts,labels = WS7_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS8 Chosen SST
  WS8_ChosenSST_histo<-hist(ChosenSST_WS8sub$Mean_SST_C, breaks=seq(12,30,2), main="WS8 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,210))
  text(x=WS8_ChosenSST_histo$mids, y=WS8_ChosenSST_histo$counts,labels = WS8_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS8sub$Mean_SST_C) # NAs
  WS8_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS8 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS8_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,210,5),labels=seq(0,210,5), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS8_ChosenSST_histo$mids, y=WS8_ChosenSST_histo$counts,labels = WS8_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS9 Chosen SST
  WS9_ChosenSST_histo<-hist(ChosenSST_WS9sub$Mean_SST_C, breaks=seq(12,30,2), main="WS9 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,20))
  text(x=WS9_ChosenSST_histo$mids, y=WS9_ChosenSST_histo$counts,labels = WS9_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS9sub$Mean_SST_C) # NAs
  WS9_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS9 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS9_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,15,1),labels=seq(0,15,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS9_ChosenSST_histo$mids, y=WS9_ChosenSST_histo$counts,labels = WS9_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS10 Chosen SST
  WS10_ChosenSST_histo<-hist(ChosenSST_WS10sub$Mean_SST_C, breaks=seq(12,30,2), main="WS10 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,90))
  text(x=WS10_ChosenSST_histo$mids, y=WS10_ChosenSST_histo$counts,labels = WS10_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS10sub$Mean_SST_C) # NAs
  WS10_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS10 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS10_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,86,2),labels=seq(0,86,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS10_ChosenSST_histo$mids, y=WS10_ChosenSST_histo$counts,labels = WS10_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS11 Chosen SST
  WS11_ChosenSST_histo<-hist(ChosenSST_WS11sub$Mean_SST_C, breaks=seq(12,30,2), main="WS11 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,45))
  text(x=WS11_ChosenSST_histo$mids, y=WS11_ChosenSST_histo$counts,labels = WS11_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS11sub$Mean_SST_C) # NAs
  WS11_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS11 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS11_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,42,2),labels=seq(0,42,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS11_ChosenSST_histo$mids, y=WS11_ChosenSST_histo$counts,labels = WS11_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS12 Chosen SST
  WS12_ChosenSST_histo<-hist(ChosenSST_WS12sub$Mean_SST_C, breaks=seq(12,30,2), main="WS12 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,10))
  text(x=WS12_ChosenSST_histo$mids, y=WS12_ChosenSST_histo$counts,labels = WS12_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS12sub$Mean_SST_C) # NAs
  WS12_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS12 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS12_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,6,1),labels=seq(0,6,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS12_ChosenSST_histo$mids, y=WS12_ChosenSST_histo$counts,labels = WS12_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS13 Chosen SST
  WS13_ChosenSST_histo<-hist(ChosenSST_WS13sub$Mean_SST_C, breaks=seq(12,30,2), main="WS13 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,450))
  text(x=WS13_ChosenSST_histo$mids, y=WS13_ChosenSST_histo$counts,labels = WS13_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS13sub$Mean_SST_C) # NAs
  WS13_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS13 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS13_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,441,5),labels=seq(0,441,5), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS13_ChosenSST_histo$mids, y=WS13_ChosenSST_histo$counts,labels = WS13_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS14 Chosen SST
  WS14_ChosenSST_histo<-hist(ChosenSST_WS14sub$Mean_SST_C, breaks=seq(12,30,2), main="WS14 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,75))
  text(x=WS14_ChosenSST_histo$mids, y=WS14_ChosenSST_histo$counts,labels = WS14_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS14sub$Mean_SST_C) # NAs
  WS14_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS14 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS14_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,72,2),labels=seq(0,72,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS14_ChosenSST_histo$mids, y=WS14_ChosenSST_histo$counts,labels = WS14_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS15 Chosen SST
  WS15_ChosenSST_histo<-hist(ChosenSST_WS15sub$Mean_SST_C, breaks=seq(12,30,2), main="WS15 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,30))
  text(x=WS15_ChosenSST_histo$mids, y=WS15_ChosenSST_histo$counts,labels = WS15_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS15sub$Mean_SST_C) # NAs
  WS15_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS15 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS15_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,26,2),labels=seq(0,26,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS15_ChosenSST_histo$mids, y=WS15_ChosenSST_histo$counts,labels = WS15_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS16 Chosen SST
  WS16_ChosenSST_histo<-hist(ChosenSST_WS16sub$Mean_SST_C, breaks=seq(12,30,2), main="WS16 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,15))
  text(x=WS16_ChosenSST_histo$mids, y=WS16_ChosenSST_histo$counts,labels = WS16_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS16sub$Mean_SST_C) # NAs
  WS16_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS16 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS16_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS16_ChosenSST_histo$mids, y=WS16_ChosenSST_histo$counts,labels = WS16_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS17 Chosen SST
  WS17_ChosenSST_histo<-hist(ChosenSST_WS17sub$Mean_SST_C, breaks=seq(12,30,2), main="WS17 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,15))
  text(x=WS17_ChosenSST_histo$mids, y=WS17_ChosenSST_histo$counts,labels = WS17_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS17sub$Mean_SST_C) # NAs
  WS17_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS17 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS17_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,15,1),labels=seq(0,15,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS17_ChosenSST_histo$mids, y=WS17_ChosenSST_histo$counts,labels = WS17_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS18 Chosen SST
  WS18_ChosenSST_histo<-hist(ChosenSST_WS18sub$Mean_SST_C, breaks=seq(12,30,2), main="WS18 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,5))
  text(x=WS18_ChosenSST_histo$mids, y=WS18_ChosenSST_histo$counts,labels = WS18_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS18sub$Mean_SST_C) # NAs
  WS18_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS18 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS18_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS18_ChosenSST_histo$mids, y=WS18_ChosenSST_histo$counts,labels = WS18_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)

  ######################
  #WS20 Chosen SST
  WS20_ChosenSST_histo<-hist(ChosenSST_WS20sub$Mean_SST_C, breaks=seq(12,30,2), main="WS20 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,5))
  text(x=WS20_ChosenSST_histo$mids, y=WS20_ChosenSST_histo$counts,labels = WS20_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS20sub$Mean_SST_C) # NAs
  WS20_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS20 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS20_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS20_ChosenSST_histo$mids, y=WS20_ChosenSST_histo$counts,labels = WS20_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)

  ######################
  #WS21 Chosen SST
  WS21_ChosenSST_histo<-hist(ChosenSST_WS21sub$Mean_SST_C, breaks=seq(12,30,2), main="WS21 Chosen SST \nAll Years", xlab="Sea Surface Temperature (C)", col="violetred1", xaxt="n", ylim=c(0,5))
  text(x=WS21_ChosenSST_histo$mids, y=WS21_ChosenSST_histo$counts,labels = WS21_ChosenSST_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  summary(ChosenSST_WS21sub$Mean_SST_C) # NAs
  WS21_ChosenSST_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailSSTAllhisto, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Tempterature (C)", main="WS21 SST Preference", ylim=c(0, 800000))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0, 800000, 100000), labels=c("0", "100k", "200k", "300k", "400k", "500k", "600k", "700k", "800k"), las=2)
  par(new=T)
  plot(WS21_ChosenSST_histo, col=violetred1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = violetred1_tr, col.axis = "violetred1", ylab = "Frequency")
  text(x=WS21_ChosenSST_histo$mids, y=WS21_ChosenSST_histo$counts,labels = WS21_ChosenSST_histo$counts, cex = .7, pos=3, col="black", offset=.1)
}
  
{
  par(mfrow = c(2, 2))
  #WS1 Chosen SST
  WS1_ChosenSST_histo_percent<-hist(ChosenSST_WS1sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS1 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS1_ChosenSST_histo_percent$density<-WS1_ChosenSST_histo_percent$counts/sum(WS1_ChosenSST_histo_percent$counts)*100
  WS1_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS1_ChosenSST_histo_percent, freq=FALSE, main="WS1 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS2 Chosen SST
  WS2_ChosenSST_histo_percent<-hist(ChosenSST_WS2sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS2 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS2_ChosenSST_histo_percent$density<-WS2_ChosenSST_histo_percent$counts/sum(WS2_ChosenSST_histo_percent$counts)*100
  WS2_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS2_ChosenSST_histo_percent, freq=FALSE, main="WS2 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS3 Chosen SST
  WS3_ChosenSST_histo_percent<-hist(ChosenSST_WS3sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS3 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS3_ChosenSST_histo_percent$density<-WS3_ChosenSST_histo_percent$counts/sum(WS3_ChosenSST_histo_percent$counts)*100
  WS3_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS3_ChosenSST_histo_percent, freq=FALSE,  main="WS3 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n",xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS4 Chosen SST
  WS4_ChosenSST_histo_percent<-hist(ChosenSST_WS4sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS4 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS4_ChosenSST_histo_percent$density<-WS4_ChosenSST_histo_percent$counts/sum(WS4_ChosenSST_histo_percent$counts)*100
  WS4_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS4_ChosenSST_histo_percent, freq=FALSE,  main="WS4 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS5 Chosen SST
  WS5_ChosenSST_histo_percent<-hist(ChosenSST_WS5sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS5 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS5_ChosenSST_histo_percent$density<-WS5_ChosenSST_histo_percent$counts/sum(WS5_ChosenSST_histo_percent$counts)*100
  WS5_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS5_ChosenSST_histo_percent, freq=FALSE, main="WS5 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS6 Chosen SST
  WS6_ChosenSST_histo_percent<-hist(ChosenSST_WS6sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS6 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS6_ChosenSST_histo_percent$density<-WS6_ChosenSST_histo_percent$counts/sum(WS6_ChosenSST_histo_percent$counts)*100
  WS6_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS6_ChosenSST_histo_percent, freq=FALSE, main="WS6 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS7 Chosen SST
  WS7_ChosenSST_histo_percent<-hist(ChosenSST_WS7sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS7 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS7_ChosenSST_histo_percent$density<-WS7_ChosenSST_histo_percent$counts/sum(WS7_ChosenSST_histo_percent$counts)*100
  WS7_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS7_ChosenSST_histo_percent, freq=FALSE, main="WS7 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS8 Chosen SST
  WS8_ChosenSST_histo_percent<-hist(ChosenSST_WS8sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS8 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS8_ChosenSST_histo_percent$density<-WS8_ChosenSST_histo_percent$counts/sum(WS8_ChosenSST_histo_percent$counts)*100
  WS8_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS8_ChosenSST_histo_percent, freq=FALSE, main="WS8 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS9 Chosen SST
  WS9_ChosenSST_histo_percent<-hist(ChosenSST_WS9sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS9 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS9_ChosenSST_histo_percent$density<-WS9_ChosenSST_histo_percent$counts/sum(WS9_ChosenSST_histo_percent$counts)*100
  WS9_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS9_ChosenSST_histo_percent, freq=FALSE, main="WS9 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS10 Chosen SST
  WS10_ChosenSST_histo_percent<-hist(ChosenSST_WS10sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS10 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS10_ChosenSST_histo_percent$density<-WS10_ChosenSST_histo_percent$counts/sum(WS10_ChosenSST_histo_percent$counts)*100
  WS10_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS10_ChosenSST_histo_percent, freq=FALSE, main="WS10 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS11 Chosen SST
  WS11_ChosenSST_histo_percent<-hist(ChosenSST_WS11sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS11 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS11_ChosenSST_histo_percent$density<-WS11_ChosenSST_histo_percent$counts/sum(WS11_ChosenSST_histo_percent$counts)*100
  WS11_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS11_ChosenSST_histo_percent, freq=FALSE, main="WS11 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS12 Chosen SST
  WS12_ChosenSST_histo_percent<-hist(ChosenSST_WS12sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS12 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS12_ChosenSST_histo_percent$density<-WS12_ChosenSST_histo_percent$counts/sum(WS12_ChosenSST_histo_percent$counts)*100
  WS12_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS12_ChosenSST_histo_percent, freq=FALSE, main="WS12 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS13 Chosen SST
  WS13_ChosenSST_histo_percent<-hist(ChosenSST_WS13sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS13 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS13_ChosenSST_histo_percent$density<-WS13_ChosenSST_histo_percent$counts/sum(WS13_ChosenSST_histo_percent$counts)*100
  WS13_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS13_ChosenSST_histo_percent, freq=FALSE, main="WS13 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS14 Chosen SST
  WS14_ChosenSST_histo_percent<-hist(ChosenSST_WS14sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS14 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS14_ChosenSST_histo_percent$density<-WS14_ChosenSST_histo_percent$counts/sum(WS14_ChosenSST_histo_percent$counts)*100
  WS14_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS14_ChosenSST_histo_percent, freq=FALSE, main="WS14 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS15 Chosen SST
  WS15_ChosenSST_histo_percent<-hist(ChosenSST_WS15sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS15 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS15_ChosenSST_histo_percent$density<-WS15_ChosenSST_histo_percent$counts/sum(WS15_ChosenSST_histo_percent$counts)*100
  WS15_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS15_ChosenSST_histo_percent, freq=FALSE, main="WS15 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS16 Chosen SST
  WS16_ChosenSST_histo_percent<-hist(ChosenSST_WS16sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS16 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS16_ChosenSST_histo_percent$density<-WS16_ChosenSST_histo_percent$counts/sum(WS16_ChosenSST_histo_percent$counts)*100
  WS16_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS16_ChosenSST_histo_percent, freq=FALSE, main="WS16 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS17 Chosen SST
  WS17_ChosenSST_histo_percent<-hist(ChosenSST_WS17sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS17 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS17_ChosenSST_histo_percent$density<-WS17_ChosenSST_histo_percent$counts/sum(WS17_ChosenSST_histo_percent$counts)*100
  WS17_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS17_ChosenSST_histo_percent, freq=FALSE, main="WS17 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS18 Chosen SST
  WS18_ChosenSST_histo_percent<-hist(ChosenSST_WS18sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS18 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS18_ChosenSST_histo_percent$density<-WS18_ChosenSST_histo_percent$counts/sum(WS18_ChosenSST_histo_percent$counts)*100
  WS18_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS18_ChosenSST_histo_percent, freq=FALSE, main="WS18 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS20 Chosen SST
  WS20_ChosenSST_histo_percent<-hist(ChosenSST_WS20sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS20 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS20_ChosenSST_histo_percent$density<-WS20_ChosenSST_histo_percent$counts/sum(WS20_ChosenSST_histo_percent$counts)*100
  WS20_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS20_ChosenSST_histo_percent, freq=FALSE, main="WS20 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
  
  #WS21 Chosen SST
  WS21_ChosenSST_histo_percent<-hist(ChosenSST_WS21sub$Mean_SST_C, breaks=seq(12, 30, 2), main="WS21 Chosen SST \nAll Years", xlab="SST (ppt)", col=violetred1_tr, xaxt="n", ylim=c(0,110))
  WS21_ChosenSST_histo_percent$density<-WS21_ChosenSST_histo_percent$counts/sum(WS21_ChosenSST_histo_percent$counts)*100
  WS21_ChosenSST_histo_percent$density
  plot(AvailSSTAllhisto_percent, col="purple4", xaxt="n", yaxt="n", xlab="Sea Surface Temperature (C)", main="",freq=FALSE, ylab="Percentage", ylim=c(0,25))
  axis(side=1, at=seq(12, 30, 2), labels=seq(12, 30, 2), las=2)
  axis(side=2, at=seq(0,25,5), labels=seq(0,25,5), col="purple4", col.axis="purple4")
  par(new=T)
  plot(WS21_ChosenSST_histo_percent, freq=FALSE, main="WS21 Chosen SST \nAll Years", col=violetred1_tr, xaxt="n", ylim=c(0,100), yaxt="n", xlab="", ylab="")
  axis(side=4, at=seq(0, 100, 10), labels=seq(0, 100, 10), las=2)
}  
#####################################################################################################################################################################################################
#####################################################################################################################################################################################################
###GEBCO bathy preferences ***REDONE TO CORRECT INTERVALS***
#GEBCO bathy all
library(readxl)
GEBCOAllData<-read_excel("GEBCO_bathy_NYBightShelf_points.xlsx")

#Available Bathy all years
SubsetAvailableBathy<- subset(GEBCOAllData, GEBCOAllData$GEBCO_m>-300)

AvailBathyAllhisto<-hist(SubsetAvailableBathy$GEBCO_m, breaks=seq(-300, 0, 10), main="Available Bathy All Years", xlab="Depth (m)", col="darkblue", xaxt="n", yaxt="n", ylim=c(0,21000), xlim=c(0,-300))
axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
text(x=AvailBathyAllhisto$mids, y=AvailBathyAllhisto$counts,labels = AvailBathyAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300,0,20), las=2)
summary(SubsetAvailableBathy$GEBCO_m) # no NAs
AvailBathyAllhisto #max count: 20213

#Available Bathy all years PERCENTAGES
AvailBathyAllhisto_percent<-hist(SubsetAvailableBathy$GEBCO_m, breaks=seq(-300, 0, 10), main="Available Bathy All Years", xlab="Depth (m)", col="darkblue", xaxt="n", yaxt="n", ylim=c(0,21000), xlim=c(0,-300))
AvailBathyAllhisto_percent$density<-AvailBathyAllhisto_percent$counts/sum(AvailBathyAllhisto_percent$counts)*100
AvailBathyAllhisto_percent$density
summary(SubsetAvailableBathy$GEBCO_m) # 117780 NAs
AvailBathyAllhisto_percent #max count: 
plot(AvailBathyAllhisto_percent, freq=FALSE, ylab="Percentage", main="Available Bathy All Years", xlab="Depth (m)", col="darkblue", xaxt="n", yaxt="n", ylim=c(0,16), xlim=c(0,-300))
text(x=AvailBathyAllhisto_percent$mids, y=AvailBathyAllhisto_percent$counts/sum(AvailBathyAllhisto_percent$counts)*100,labels = AvailBathyAllhisto_percent$counts/sum(AvailBathyAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300,0,20), las=2)
axis(side=2, at=seq(0, 16, 2), labels=seq(0, 16, 2), las=2, col = "darkblue", col.axis = "darkblue")

#Chosen Bathy all years
All_SPOTACT_BathySSTChlaSSS_Results<-read_excel("DS_Bathy_SST_SSS_Chla_16_19.xlsx")
SubsetChosenBathy<-subset(All_SPOTACT_BathySSTChlaSSS_Results, All_SPOTACT_BathySSTChlaSSS_Results$BottomDepth_m_GEBCO>-300)

ChosenBathyAllhisto<-hist(SubsetChosenBathy$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,600), xlim=c(0,-300))
text(x=ChosenBathyAllhisto$mids, y=ChosenBathyAllhisto$counts,labels = ChosenBathyAllhisto$counts, cex = .6, pos = 3)
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
summary(SubsetChosenBathy$BottomDepth_m_GEBCO) #no NAs
ChosenBathyAllhisto  #max count: 2159

#Chosen Bathy all years PERCENTAGES
ChosenBathyAllhisto_percent<-hist(SubsetChosenBathy$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,2300), xlim=c(0,-300))
ChosenBathyAllhisto_percent$density<-ChosenBathyAllhisto_percent$counts/sum(ChosenBathyAllhisto_percent$counts)*100
ChosenBathyAllhisto_percent$density
summary(All_SPOTACT_BathySSTChlaSSS_Results$MeanSSS) #3627 NAs
ChosenBathyAllhisto_percent #max count: 
plot(ChosenBathyAllhisto_percent, freq=FALSE, ylab="Percentage",  main="Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", yaxt="n", xlim=c(0,-300))
text(x=ChosenBathyAllhisto_percent$mids, y=ChosenBathyAllhisto_percent$counts/sum(ChosenBathyAllhisto_percent$counts)*100,labels = ChosenBathyAllhisto_percent$counts/sum(ChosenBathyAllhisto_percent$counts)*100, cex = .6, pos = 3)
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300,0,20), las=2)
axis(side=2, at=seq(0, 58, 2), labels=seq(0, 58, 2), las=2, col = "turquoise3", col.axis = "turquoise3")


#two axes - 1:1 ratio TRANSPARENT **Flip x-Axis**
plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="Available vs Actual Water Column Depth", ylim=c(0, 21000), xlim=c(0,-300))
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
axis(side=2, at=seq(0, 21000, 3000), labels=c("0","3k", "6k", "9k", "12k", "15k", "18k", "21k"), las=2, col = "darkblue", col.axis = "darkblue")
par(new=T)
plot(ChosenBathyAllhisto, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="", xlim=c(0,-300))
axis(side = 4, at=seq(0,600,100),labels=seq(0,600,100), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
legend(-200, 550, legend=c("Available", "Chosen"), col=c("darkblue", "turquoise3"), pch=15:15, cex=0.9)

#two axes - 1:1 ratio TRANSPARENT PERCENTAGE
plot(AvailBathyAllhisto_percent, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="Available vs Actual Bathymetry", freq=FALSE, ylab="Percentage", xlim=c(0,-300))
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
axis(side=2, at=seq(0, 15, 1), labels=seq(0, 15, 1), las=2, col = "darkblue", col.axis = "darkblue")
par(new=T)
plot(ChosenBathyAllhisto_percent, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", xlim=c(0,-300))
axis(side=4, at=seq(0, 48, 2), labels=seq(0, 48, 2), las=2, col = "turquoise3", col.axis = "turquoise3")
legend(-240, 46, legend=c("Available", "Actual"), col=c("darkblue", "turquoise3"), pch=15:15, cex=0.9)

#two axes - 100 vs 100 ratio TRANSPARENT PERCENTAGE
plot(AvailBathyAllhisto_percent, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="Available vs Actual Bathymetry", freq=FALSE, ylab="Percentage", xlim=c(0,-300), ylim=c(0,50))
axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
axis(side=2, at=seq(0, 50, 5), labels=seq(0, 50, 5), las=2, col = "darkblue", col.axis = "darkblue")
par(new=T)
plot(ChosenBathyAllhisto_percent, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="", freq=FALSE, ylab="", xlim=c(0,-300), ylim=c(0,50))
axis(side=4, at=seq(0, 50, 5), labels=seq(0, 50, 5), las=2, col = "turquoise3", col.axis = "turquoise3")
legend(-200, 45, legend=c("Available", "Actual"), col=c("darkblue", "turquoise3"), pch=15:15, cex=0.9)

#subset by WS number
ChosenBathy_WS1sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==1)
ChosenBathy_WS2sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==2)
ChosenBathy_WS3sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==3)
ChosenBathy_WS4sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==4)
ChosenBathy_WS5sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==5)
ChosenBathy_WS6sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==6)
ChosenBathy_WS7sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==7)
ChosenBathy_WS8sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==8)
ChosenBathy_WS9sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==9)
ChosenBathy_WS10sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==10)
ChosenBathy_WS11sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==11)
ChosenBathy_WS12sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==12)
ChosenBathy_WS13sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==13)
ChosenBathy_WS14sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==14)
ChosenBathy_WS15sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==15)
ChosenBathy_WS16sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==16)
ChosenBathy_WS17sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==17)
ChosenBathy_WS18sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==18)
ChosenBathy_WS20sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==20)
ChosenBathy_WS21sub <- subset(SubsetChosenBathy, SubsetChosenBathy$WSNumber==21)
{
  ######################
  #WS1 Chosen Bathy
  WS1_ChosenBathy_histo<-hist(ChosenBathy_WS1sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS1 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,50), xlim=c(0,-300))
  text(x=WS1_ChosenBathy_histo$mids, y=WS1_ChosenBathy_histo$counts,labels = WS1_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS1sub$BottomDepth_m_GEBCO) #NAs
  WS1_ChosenBathy_histo  #max count: 
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS1 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS1_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,48,2),labels=seq(0,48,2), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS1_ChosenBathy_histo$mids, y=WS1_ChosenBathy_histo$counts,labels = WS1_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS2 Chosen Bathy
  WS2_ChosenBathy_histo<-hist(ChosenBathy_WS2sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS2 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,55))
  text(x=WS2_ChosenBathy_histo$mids, y=WS2_ChosenBathy_histo$counts,labels = WS2_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS2sub$BottomDepth_m_GEBCO) #NAs
  WS2_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS2 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS2_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,53,2),labels=seq(0,53,2), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS2_ChosenBathy_histo$mids, y=WS2_ChosenBathy_histo$counts,labels = WS2_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS3 Chosen Bathy
  WS3_ChosenBathy_histo<-hist(ChosenBathy_WS3sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS3 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,15))
  text(x=WS3_ChosenBathy_histo$mids, y=WS3_ChosenBathy_histo$counts,labels = WS3_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS3sub$BottomDepth_m_GEBCO) #NAs
  WS3_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS3 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS3_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,14,2),labels=seq(0,14,2), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS3_ChosenBathy_histo$mids, y=WS3_ChosenBathy_histo$counts,labels = WS3_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS4 Chosen Bathy
  WS4_ChosenBathy_histo<-hist(ChosenBathy_WS4sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS4 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,20))
  text(x=WS4_ChosenBathy_histo$mids, y=WS4_ChosenBathy_histo$counts,labels = WS4_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS4sub$BottomDepth_m_GEBCO) #NAs
  WS4_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS4 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS4_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,18,2),labels=seq(0,18,2), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS4_ChosenBathy_histo$mids, y=WS4_ChosenBathy_histo$counts,labels = WS4_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS5 Chosen Bathy
  WS5_ChosenBathy_histo<-hist(ChosenBathy_WS5sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS5 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,245))
  text(x=WS5_ChosenBathy_histo$mids, y=WS5_ChosenBathy_histo$counts,labels = WS5_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS5sub$BottomDepth_m_GEBCO) #NAs
  WS5_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS5 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS5_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,241,5),labels=seq(0,241,5), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS5_ChosenBathy_histo$mids, y=WS5_ChosenBathy_histo$counts,labels = WS5_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS6 Chosen Bathy
  WS6_ChosenBathy_histo<-hist(ChosenBathy_WS6sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS6 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,450))
  text(x=WS6_ChosenBathy_histo$mids, y=WS6_ChosenBathy_histo$counts,labels = WS6_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS6sub$BottomDepth_m_GEBCO) #NAs
  WS6_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS6 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS6_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,450,10),labels=seq(0,450,10), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS6_ChosenBathy_histo$mids, y=WS6_ChosenBathy_histo$counts,labels = WS6_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS7 Chosen Bathy
  WS7_ChosenBathy_histo<-hist(ChosenBathy_WS7sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS7 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,210))
  text(x=WS7_ChosenBathy_histo$mids, y=WS7_ChosenBathy_histo$counts,labels = WS7_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS7sub$BottomDepth_m_GEBCO) #NAs
  WS7_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS7 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS7_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,208,10),labels=seq(0,208,10), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS7_ChosenBathy_histo$mids, y=WS7_ChosenBathy_histo$counts,labels = WS7_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS8 Chosen Bathy
  WS8_ChosenBathy_histo<-hist(ChosenBathy_WS8sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS8 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,225))
  text(x=WS8_ChosenBathy_histo$mids, y=WS8_ChosenBathy_histo$counts,labels = WS8_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS8sub$BottomDepth_m_GEBCO) #NAs
  WS8_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS8 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS8_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,220,10),labels=seq(0,220,10), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS8_ChosenBathy_histo$mids, y=WS8_ChosenBathy_histo$counts,labels = WS8_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS9 Chosen Bathy
  WS9_ChosenBathy_histo<-hist(ChosenBathy_WS9sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS9 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,20))
  text(x=WS9_ChosenBathy_histo$mids, y=WS9_ChosenBathy_histo$counts,labels = WS9_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS9sub$BottomDepth_m_GEBCO) #NAs
  WS9_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS9 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS9_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,15,1),labels=seq(0,15,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS9_ChosenBathy_histo$mids, y=WS9_ChosenBathy_histo$counts,labels = WS9_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS10 Chosen Bathy
  WS10_ChosenBathy_histo<-hist(ChosenBathy_WS10sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS10 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,140))
  text(x=WS10_ChosenBathy_histo$mids, y=WS10_ChosenBathy_histo$counts,labels = WS10_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS10sub$BottomDepth_m_GEBCO) #NAs
  WS10_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS10 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS10_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,136,5),labels=seq(0,136,5), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS10_ChosenBathy_histo$mids, y=WS10_ChosenBathy_histo$counts,labels = WS10_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS11 Chosen Bathy
  WS11_ChosenBathy_histo<-hist(ChosenBathy_WS11sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS11 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,45))
  text(x=WS11_ChosenBathy_histo$mids, y=WS11_ChosenBathy_histo$counts,labels = WS11_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS11sub$BottomDepth_m_GEBCO) #NAs
  WS11_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS11 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS11_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,42,2),labels=seq(0,42,2), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS11_ChosenBathy_histo$mids, y=WS11_ChosenBathy_histo$counts,labels = WS11_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS12 Chosen Bathy
  WS12_ChosenBathy_histo<-hist(ChosenBathy_WS12sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS12 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,10))
  text(x=WS12_ChosenBathy_histo$mids, y=WS12_ChosenBathy_histo$counts,labels = WS12_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS12sub$BottomDepth_m_GEBCO) #NAs
  WS12_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS12 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS12_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,8,1),labels=seq(0,8,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS12_ChosenBathy_histo$mids, y=WS12_ChosenBathy_histo$counts,labels = WS12_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS13 Chosen Bathy
  WS13_ChosenBathy_histo<-hist(ChosenBathy_WS13sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS13 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,1100))
  text(x=WS13_ChosenBathy_histo$mids, y=WS13_ChosenBathy_histo$counts,labels = WS13_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS13sub$BottomDepth_m_GEBCO) #NAs
  WS13_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS13 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS13_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,1080,10),labels=seq(0,1080,10), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS13_ChosenBathy_histo$mids, y=WS13_ChosenBathy_histo$counts,labels = WS13_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS14 Chosen Bathy
  WS14_ChosenBathy_histo<-hist(ChosenBathy_WS14sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS14 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,200))
  text(x=WS14_ChosenBathy_histo$mids, y=WS14_ChosenBathy_histo$counts,labels = WS14_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS14sub$BottomDepth_m_GEBCO) #NAs
  WS14_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS14 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS14_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,194,10),labels=seq(0,194,10), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS14_ChosenBathy_histo$mids, y=WS14_ChosenBathy_histo$counts,labels = WS14_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS15 Chosen Bathy
  WS15_ChosenBathy_histo<-hist(ChosenBathy_WS15sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS15 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,40))
  text(x=WS15_ChosenBathy_histo$mids, y=WS15_ChosenBathy_histo$counts,labels = WS15_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS15sub$BottomDepth_m_GEBCO) #NAs
  WS15_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS15 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS15_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,33,1),labels=seq(0,33,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS15_ChosenBathy_histo$mids, y=WS15_ChosenBathy_histo$counts,labels = WS15_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS16 Chosen Bathy
  WS16_ChosenBathy_histo<-hist(ChosenBathy_WS16sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS16 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,30))
  text(x=WS16_ChosenBathy_histo$mids, y=WS16_ChosenBathy_histo$counts,labels = WS16_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS16sub$BottomDepth_m_GEBCO) #NAs
  WS16_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS16 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS16_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,22,1),labels=seq(0,22,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS16_ChosenBathy_histo$mids, y=WS16_ChosenBathy_histo$counts,labels = WS16_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS17 Chosen Bathy
  WS17_ChosenBathy_histo<-hist(ChosenBathy_WS17sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS17 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,20))
  text(x=WS17_ChosenBathy_histo$mids, y=WS17_ChosenBathy_histo$counts,labels = WS17_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS17sub$BottomDepth_m_GEBCO) #NAs
  WS17_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS17 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS17_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,15,1),labels=seq(0,15,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS17_ChosenBathy_histo$mids, y=WS17_ChosenBathy_histo$counts,labels = WS17_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS18 Chosen Bathy
  WS18_ChosenBathy_histo<-hist(ChosenBathy_WS18sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS18 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,5))
  text(x=WS18_ChosenBathy_histo$mids, y=WS18_ChosenBathy_histo$counts,labels = WS18_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS18sub$BottomDepth_m_GEBCO) #NAs
  WS18_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS18 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS18_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS18_ChosenBathy_histo$mids, y=WS18_ChosenBathy_histo$counts,labels = WS18_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS20 Chosen Bathy
  WS20_ChosenBathy_histo<-hist(ChosenBathy_WS20sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS20 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,5))
  text(x=WS20_ChosenBathy_histo$mids, y=WS20_ChosenBathy_histo$counts,labels = WS20_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS20sub$BottomDepth_m_GEBCO) #NAs
  WS20_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS20 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS20_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS20_ChosenBathy_histo$mids, y=WS20_ChosenBathy_histo$counts,labels = WS20_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  ######################
  #WS21 Chosen Bathy
  WS21_ChosenBathy_histo<-hist(ChosenBathy_WS21sub$BottomDepth_m_GEBCO, breaks=seq(-300, 0, 10), main="WS21 Chosen Bathy All Years", xlab="Bottom Depth (m)", col="turquoise1", xaxt="n", ylim=c(0,5))
  text(x=WS21_ChosenBathy_histo$mids, y=WS21_ChosenBathy_histo$counts,labels = WS21_ChosenBathy_histo$counts, cex = .6, pos = 3)
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  summary(ChosenBathy_WS21sub$BottomDepth_m_GEBCO) #NAs
  WS21_ChosenBathy_histo  #max count:
  
  #two axes - 1:1 ratio TRANSPARENT
  plot(AvailBathyAllhisto, col="darkblue", xaxt="n", yaxt="n", xlab="Depth (m)", main="WS21 Bathy Preference", ylim=c(0,21000))
  axis(side=1, at=seq(-300, 0, 20), labels=seq(-300, 0, 20), las=2)
  axis(side=2, at=seq(0, 21000, 3000), labels=seq(0, 21000, 3000), las=2)
  par(new=T)
  plot(WS21_ChosenBathy_histo, col=turquoise1_tr, main="", xaxt="n", yaxt="n", xlab="")
  axis(side = 4, at=seq(0,2,1),labels=seq(0,2,1), col = "turquoise3", col.axis = "turquoise3", ylab = "Frequency")
  text(x=WS21_ChosenBathy_histo$mids, y=WS21_ChosenBathy_histo$counts,labels = WS21_ChosenBathy_histo$counts, cex = .7, pos=3, col="black", offset=.1)
  
  










}