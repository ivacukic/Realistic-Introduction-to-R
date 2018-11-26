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


# assign labels to factor levels

# read in second dataset, exercise_data_b, repeat like for dataset 1
data3 <- read.csv("exercise_data_b.csv", header=TRUE)

# recode anxiety variable into a new one with three categories:
# 0-3 no anxiety; 4-6 moderate; 7+ severe; assign labels

# prepare both datasets for merging
# merge data sets

# main analyses:
# produce table of descriptives for all variables in the study, stratified by dementia status and total
# what is the correlation between bmi and iq. produce scatterplot. 
# does bmi differ between those with and without dementia
# do mean levels of anxiety differ between treatment groups (1 no treatment; 2 placebo; 3 intervention)
# does iq predict dementia?
# does iq predict dementia, controlling for age, sex and bmi?





