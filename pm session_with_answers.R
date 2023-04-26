### PM SESSION ###

# read in new data
newdata <- read.csv("pm_data.csv", header=TRUE)
#have a look what's in this dataset, examine the variables
#e.g.
View(newdata)
names(newdata)
is.factor(newdata$group)

# merge two data frames
data <- read.csv("new_data.csv", header=TRUE) #read in morning dataset
merged_data <- merge(data, newdata, by="id", all=TRUE)

#oops, looks like an error message! can you tell what went wrong?

names(data)
names(newdata)
#examine both id variables
summary(data$X)
summary(newdata$IDNO)

# there is one wrong id value, substitute with the correct one
# data.frame[row_number, column_number] = new_value

data
data[20,1] = 20
data

#another way using variable (column) name
data$id[20]=20 #row number in square brackets

#we also need to rename one of the id variables to match the other
names(newdata)[1] <- "id" #renaming the id variable in the second data frame to match the first

# now we can finally merge
merged_data <- merge(data, newdata, by="id", all=TRUE) # all.x T; all.y=T

names(merged_data)
head(merged_data)

# create data frame with no missing values
final_data <- na.omit(merged_data)

# independent t-test
# generic formula
model <- t.test(outcome ~ predictor, data = mydata, paired= T/F)
model

# Do depression scores differ between men and women?
m1 <- t.test(depression ~ sex, data = final_data)
m1

# Do cognition scores differ between those with and without disability?
m2 <- t.test(cognition ~ disability, data = final_data)
m2

# calculate the effect size
model <- t.test(depression ~ sex, data=final_data) #run the model
t <- model$statistic[[1]] # extract the value of t
df <- model$parameter[[1]] # extract degrees of freedom
r <- sqrt(t^2/(t^2+df)) #calculate the effect size r based on t and df
round(r, digits=2) #round to two decimal places

# ANOVA
model2 <- aov(depression ~ group, data=final_data)
summary(model2)
plot(model2)

#effect size e.g. omega squared can be calculated based on the values provided in the summary()
# posthoc tests
pairwise.t.test(final_data$depression, final_data$group, paired=FALSE, p.adjust.method = "bonferroni")

### Regression ###
# continuous outcome
# does age predicts cognition
m1 <- lm(cognition ~ age, data=final_data)
summary(m1)

#if we want to standardise a variable for ease of interpretation
m2 <- lm(scale(cognition) ~ age, data=final_data)
summary(m2)

# does age predict cognition, controlling for sex and disability
m3 <- lm(scale(cognition) ~ age + as.factor(sex) + as.factor(disability), data=final_data)
summary(m3)

# does age predict depression
m3 <- lm(scale(depression) ~ age, data=final_data)
summary(m3)

# does age predict depression, controlling for sex and disability
m4 <- lm(scale(depression) ~ age + as.factor(sex) + as.factor(disability), data=final_data)
summary(m4)


# binary outcome: glm function
# use the categorical depression variable we created earlier, dep_cat

m5 <- glm(dep_cat ~ scale(age), data = final_data, family = "binomial") 
summary(m5)

# does cognition score predict depression?
m6 <- glm(dep_cat ~ cognition, data = final_data, family = "binomial") 
summary(m6)

# remove everything from the environment
rm(list=ls())

### Exercise ###

# read in first dataset week4dataa
data2 <- read.csv("exercise_data_a.csv", header=TRUE) 

# inspect variables (variable type, range/frequency, distribution/histograms, 
#weird values, NAs)
# clean up if needed

names(data2) # see what variables are there
View(data2) # show dataframe as spreadsheet
summary(data2$id) #check if there are any weird values in first variable
summary(data2$age) # 3 NAs
table(data2$sex) 
summary(data2$bmi) # weird values, need to inspect further
hist(data2$bmi)
is.na(data2$bmi) = data2$bmi == -999
is.na(data2$bmi) = data2$bmi == 804
summary(data2$bmi) # still one unlikely value
is.na(data2$bmi) = data2$bmi == 7
summary(data2$bmi) # looking OK now


# read in second dataset, exercise_data_b, repeat like for dataset 1
data3 <- read.csv("exercise_data_b.csv", header=TRUE)

# recode anxiety variable into a new one with three categories:
# 0-3 no anxiety; 4-6 moderate; 7+ severe; assign labels
data3$anx_cat <- NA
data3$anx_cat[(data3$anxiety <= 3)] <- 0
data3$anx_cat[(data3$anxiety > 3 & data3$anxiety <= 6  )] <- 1
data3$anx_cat[(data3$anxiety >= 7  )] <- 2
table(data3$anx_cat)

data3$anx_cat <- factor(data3$anx_cat, levels = c(0,1,2), 
                       labels = c("No anxiety", "Moderate anxiety", "Severe anxiety"))
table(data3$anx_cat)

# prepare both datasets for merging
#check if the names of ID variable is the same in both datasets
names(data2)
names(data3)
#id is lower case in data2, upper case in data3 - need to rename one of them to match
names(data3)[1] <- "id"

#check if values of id vars are matching in both data frames
summary(data2$id)
summary(data3$id)
#looks OK, ready to merge

# merge data sets
data_merged <- merge(data2, data3, by="id", all=T)

# main analyses:

# produce table of descriptives for all variables in the study, stratified by dementia status and total
library(tableone)
names(data_merged) #see all variables in the merged dataset

#continuous vars
tablevars <- c("age", "bmi", "iq")
tableOne <- CreateTableOne(vars=tablevars, strata=c("dementia"),data=data_merged) 
tableOne

tableOne <- CreateTableOne(vars=tablevars,data=data_merged) #total
tableOne

#categorical vars
catVars <- c("sex", "treatment", "anx_cat")
catTableOverall <- CreateCatTable(vars = catVars,strata=c("dementia"), data = data_merged) 
catTableOverall

catTableOverall <- CreateCatTable(vars = catVars, data = data_merged) #total
catTableOverall


# what is the correlation between bmi and iq. produce scatterplot. 
#correlation
library(Hmisc)
cor.test(data_merged$bmi, data_merged$iq) #r=-0.08, p=0.768
plot(data_merged$bmi, data_merged$iq)

# does bmi differ between those with and without dementia
t.test(data_merged$bmi, data_merged$dementia) #yes, p < 0.001

# do mean levels of anxiety differ between treatment groups (1 no treatment; 2 placebo; 3 intervention)
m <- aov(anxiety ~ treatment, data=data_merged)
summary(m) #yes, p=0.0037

# does iq predict dementia?

m1 <- glm(dementia ~ scale(iq), data=data_merged, family="binomial")
summary(m1) #yes, p=0.026

# does iq predict dementia, controlling for age, sex and bmi?
m2 <- glm(dementia ~ scale(iq) + age + scale(bmi) + as.factor(sex), data=data_merged, family="binomial")
summary(m2) #no, p=0.188, the association was confounded





