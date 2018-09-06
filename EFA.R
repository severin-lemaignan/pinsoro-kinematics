# Analysis - PLY001 data
# Author: M. E. Bartlett

## SETUP -------------------------------------------------------------------------------------------
rm(list=ls()) # Clear all variables in the workspace

# Packages
library(tidyverse)
library(psych)
library(nFactors)
library(GPArotation)
library(irr)
library(REdaS)

#LIKERT ANALYSIS - PRINCIPAL COMPONENT ANALYSIS
#Create Matrix of responses to Likert Questions
data_highVar = read.csv("E:\\RAnalysis\\00High_Var.csv")

matrixData <- data_highVar[data_highVar$condition==4,][,c(1,8,12,13,14)]

matrixData$clipName <- as.factor(matrixData$clipName)
levels(matrixData$clipName) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
#[1] "1Laugh1PushAway"          "1Laughing"                "1PushBothExcited"         "1PushesHand"              "1StopsOtherPlay"         
#[6] "1Task1Watch"              "bothDistressedTable"      "discussAndTogether"       "excitedChild"             "independent"             
#[11] "independent1TryStopOther" "talk1Watch1Play"          "talkAnd1StopsOther"       "talkAndIndependent"       "talkAndSameArea"         
#[16] "talking1PushHand"         "togetherAndCrocodile"     "togetherAndTalk"          "turnTaking"               "turnTaking2"   

matrixData$question <- as.factor(matrixData$question)
levels(matrixData$question) <- c("2", "3", "14", "26", "24", "10", "22", "8", "6", "16", "18", "12", "20", "1", "4", "15", "27", "25", "11", "23", "9", "7", "17", "19", "13", "21", "5")
#[1] "age"                           "gettingOnWell"                 "leftAggressive"                "leftAmused"                   
#[5] "leftAngry"                     "leftCompetitive"               "leftContent"                   "leftCooperative"              
#[9] "leftDominant"                  "leftEngaged"                   "leftFearful"                   "leftFriendly"                 
#[13] "leftSad"                       "openBox"                       "relationshipBetweenCharacters" "rightAggressive"              
#[17] "rightAmused"                   "rightAngry"                    "rightCompetitive"              "rightContent"                 
#[21] "rightCooperative"              "rightDominant"                 "rightEngaged"                  "rightFearful"                 
#[25] "rightFriendly"                 "rightSad"                      "workingTogether"  
matrixData$question <- as.numeric(as.character(matrixData$question))

matrixData$response <- as.numeric(as.character(matrixData$response))

matrixData2 <- spread(matrixData, question, response)

#Factor Analysis
rMatrix <- (matrixData2[,c(6:30)])
corMatrix <- cor(rMatrix, use = "complete.obs")

KMOS(rMatrix, use = "complete.obs")
bart_spher(rMatrix, use = "complete.obs")

ev <- eigen(cor(rMatrix, use = "complete.obs"))
ap <- parallel(subject=nrow(rMatrix),var=ncol(rMatrix),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


pc1 <- fa(rMatrix, nfactors = 3, rotate = 'none')
pc1
print( pc1$loadings, cutoff=0.4)
