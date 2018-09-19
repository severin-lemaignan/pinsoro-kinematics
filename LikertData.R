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
library(car)
library(cocron)

#Load dataframe
data = read.csv("E:\\RAnalysis\\00PLY001CompleteData.csv")

#Create dataframe for conditions 2 and 4 only
data$response = as.numeric(as.character(data$response))

#CLEAN UP LIKERT-RESPONSE DATA
#Response variance
varianceData <- data %>%group_by(pptID, clipName, condition)%>% slice(6:27)%>%na.omit()%>%dplyr::summarise(Variance=var(response))
data["variance"] <- NA
data$variance <- varianceData$Variance[match(interaction(data$pptID, data$clipName), interaction(varianceData$pptID, varianceData$clipName))]
data["lowVariance"] <- NA
data$lowVariance <- ifelse(data$variance<0.5, 1, 0)

#Remove all participants with variance less than 1
cond24Data <- data[data$condition == 2|data$condition == 4,]
data_highVar <- subset(cond24Data,!pptID %in% pptID[cond24Data$lowVariance == 1])
write.csv(data_highVar, "E:\\RAnalysis\\00High_Var.csv", row.names=F)

#REPORTING
#Number of p's included
length(unique(cond24Data$pptID))
length(unique(data_highVar$pptID))
#Number of p's per condition - P's included in likert-question analysis
onePptPerRow2 <- data_highVar[match(unique(data_highVar$pptID), data_highVar$pptID),]
length(unique(data_highVar$pptID[data_highVar$condition==2]))
summary(onePptPerRow2$age[onePptPerRow2$condition==2])
summary(onePptPerRow2$gender[onePptPerRow2$condition==2])
summary(onePptPerRow2$nationality[onePptPerRow2$condition==2])
summary(onePptPerRow2$firstLang[onePptPerRow2$condition==2])
length(unique(data_highVar$pptID[data_highVar$condition==4]))
summary(onePptPerRow2$age[onePptPerRow2$condition==4])
summary(onePptPerRow2$gender[onePptPerRow2$condition==4])
summary(onePptPerRow2$nationality[onePptPerRow2$condition==4])
summary(onePptPerRow2$firstLang[onePptPerRow2$condition==4])

#VARIANCE
#T-test = do conditions differ in the amount of variance in responses
ggplot(data_highVar, aes(variance)) + geom_histogram(binwidth=0.5) + facet_wrap(~condition)

responseDataCond2 <- data_highVar[data_highVar$condition == 2,] 
responseDataCond4 <- data_highVar[data_highVar$condition == 4,]
x = as.numeric(as.character(responseDataCond2$variance))
y = as.numeric(as.character(responseDataCond4$variance))
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

#INTER-RATER AGREEMENT
#T-test = do conditions differ in how much the raters agree for each clip
agreeData <- cond24Data %>%group_by(clipName, condition)%>% slice(3:27)%>%dplyr::summarise(raterAgree = kripp.alpha(t(unstack(cond24Data[(cond24Data$clipName==clipName & cond24Data$condition==condition),c('response','pptID')])), method=c("ordinal"))$value)

mainEffect <- aov(raterAgree ~ condition, agreeData)
summary(mainEffect)

#Feldt Test for difference between alpha coefficients
length(unique(cond24Data$pptID[cond24Data$condition==4 & cond24Data$clipName=="turnTaking2"]))
cocron.two.coefficients(alpha=c(.433,.527), n=c(19,19), dep=FALSE)


interaction.plot(agreeData$condition, agreeData$clipName, agreeData$raterAgree)


#LIKERT ANALYSIS - FACTOR ANALYSIS
#Create Matrix of responses to Likert Questions
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
rMatrix <- (matrixData2[,c(9:30)])
corMatrix <- cor(rMatrix, use = "complete.obs")

KMOS(rMatrix, use = "complete.obs")
bart_spher(rMatrix, use = "complete.obs")

ev <- eigen(cor(rMatrix, use = "complete.obs"))
ap <- parallel(subject=nrow(rMatrix),var=ncol(rMatrix),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


pc1 <- fa(rMatrix, nfactors = 6, rotate = 'none')
pc1
print( pc1$loadings, cutoff=0.35)
