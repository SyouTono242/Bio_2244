# import labdata.csv and store it as a database
labdata <- read.csv("/Users/shaoyiran/OneDrive\ -\ The\ University\ of\ Western\ Ontario/Academics/Biology\ 2244/Assignments/Assignment\ 2/labdata.csv")
attach(labdata)
str(labdata)

# Q1 a)
# rearrange the labdata by correcting all the datatypes
heartrate <- as.integer(heartrate)
drinking <- as.factor(drinking)
smoking <- as.factor(smoking)
sleep <- as.factor(sleep)
exercise <- as.factor(exercise)
optimistic <- as.factor(optimistic)

# Q2 a)
# change the long format labdata to wide format and store it in labdata_wide
labdata$participant <- factor(labdata$participant)
labdata_wide <- reshape(labdata, direction = "wide", idvar = "participant", timevar = "timepoint")

# Q3 a)
# count people habitually participate in exercise and sort them out based on their age
# interval = 10 years
exercisedata <- subset(labdata, labdata$exercise==2)
breaks <- c(20,30,40,50,60,70,80)
exercise_t <- cut(exercisedata$age, breaks=breaks)
summary(exercise_t)

# Q4 a)
baselinedata <- subset(labdata, (labdata$group == "A" & labdata$timepoint == "baseline")|(labdata$group == "B" & labdata$timepoint == "week 4"))
bmi <- baselinedata$weight/baselinedata$height**2
bmidata <- data.frame(group = baselinedata$group, sex = baselinedata$sex, bmi=bmi)
boxplot(bmi~sex:group, data = bmidata, ylab = "BMI kg/m^2", col = c("#FCE6E8","#B9DFFE"), at = c(1,2,4,5))

