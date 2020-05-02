labdata <- read.csv("/Users/shaoyiran/OneDrive\ -\ The\ University\ of\ Western\ Ontario/Academics/Biology\ 2244/Assignments/Assignment\ 3/labdata.csv")
attach(labdata)
str(labdata)
heartrate_num<-as.numeric(as.character(heartrate))


# Q1
# Make a new wide format dataframe `staiData` containing only “participant”, “group”, and three sets of stai_scores at different timepoint 
staiData<-labdata[c("participant","group","timepoint","stai_score")]
staiData <- reshape(staiData, direction="wide", idvar="participant",timevar="timepoint")
staiData <- subset(staiData, select=-c(`group.week 4`,`group.week 8`))
names(staiData) <- c("participant","group","stai_baseline","stai_week4","stai_week8")

# Decide whether their anxiety level improved from baseline to after the intervention for every participant
change <- logical()
for (i in 1:(nrow(staiData))){
  if (staiData$group[i]=="A"){  # if the participant is in group A
    change[[i]] <- (staiData$stai_baseline[i]-staiData$stai_week4[i])>=3  # compare data obtained from baseline and week 4
  }
  if (staiData$group[i]=="B"){  # if the participant is in group B
    change[[i]] <- (staiData$stai_baseline[i]-staiData$stai_week8[i])>=3  # compare data obtained from baseline and week 8
  }
}

# Make a table of change
table(change)


# Q2
labdata$heartrate <- as.integer(labdata$heartrate)
heartrateAfter = labdata$heartrate[(labdata$group=="A"&labdata$timepoint=="week 4")|(labdata$group=="B"&labdata$timepoint=="week 8")]
heartrateAfter <- heartrateAfter[!is.na(heartrateAfter)]
hist(heartrateAfter, main="Heart Rate after Massage", xlab="Heart Rate", ylab="Frequency")
qqnorm(heartrateAfter, main="Q-Q Plot on Heartrate after Massage")
qqline(heartrateAfter, col="steelblue")

# Q3
labdata$heartrate <- as.integer(labdata$heartrate)
heartrateAfter = labdata$heartrate[(labdata$group=="A"&labdata$timepoint=="week 4")|(labdata$group=="B"&labdata$timepoint=="week 8")]
heartrateAfter <- heartrateAfter[!is.na(heartrateAfter)]
mean = mean(heartrateAfter)
sd <- sqrt(var(heartrateAfter))


