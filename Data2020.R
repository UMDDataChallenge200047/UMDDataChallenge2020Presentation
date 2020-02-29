setwd("/Users/richa/Desktop/Random Crap/School Stuff/")
library(readxl)
HUDData<-read_xlsx("/Users/richa/Desktop/Random Crap/School Stuff/Data_Level2_HUD_HUDPrograms_Fulldataset.xlsx")

drops <- c("Year", "HEAD_ID","CHLDRN_AGE_0_3_CNT", "CHLDRN_AGE_4_5_CNT","CHLDRN_AGE_6_12_CNT", "CHLDRN_AGE_13_17_CNT",
           "ADLT_AGE_18_21_CNT", "ADLT_AGE_22_25_CNT", "ADLT_AGE_26_35_CNT", "ADLT_AGE_35_49_CNT",
           "ADLT_AGE_50_61_CNT", "ADLT_AGE_62_85_CNT", "ADLT_AGE_ABOVE85_CNT", "PVRTY_PRCNT", "MNRTY_PRCNT", "BLACK_PRCNT",
           "HISPANIC_PRCNT", "WHITE_PRCNT")
HUDData[, !names(HUDData) %in% drops]

#as.character(HUDData$HEAD_RACE_CD)
#as.character(HUDData$pgm_type_edited)
#replace.value(HUDData, HUDData$HEAD_RACE_CD, c("1", "2", "3", "4", "5", "6"), c("White", "Black", "Native American", "Asian", "Hawaiian or Pacific Islander", "More than one race"))

#Tree Analysis
#something<-read.table("Data_Level2_HUD_HUDPrograms_Fulldataset.xlsx")
something <- as.data.frame(HUDData)
library("rpart")
treeAnalysis<-rpart(HUDData$pgm_type_edited~HUDData$HEAD_RACE_CD + HUDData$HEAD_ETHNCY_CD + HUDData$TOTAL_DPNDNT_CNT + HUDData$HEAD_GNDR_CD + HUDData$CHLDRN_MBR_CNT + HUDData$HEAD_DSBLTY_INDR, data=something, cp = -1, minsplit = 2500, minbucket = 2500)
#+HUDData$TOTAL_DPNDNT_CNT + HUDData$HEAD_GNDR_CD 
library("rpart.plot")
options("scipen" = 100, "digits" = 4)
rpart.plot(treeAnalysis, extra = 1)

