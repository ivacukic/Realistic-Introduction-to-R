                              ### MORNING SESSION ###

#Set working directory

# your own path to folder
setwd("~/Dropbox/R Course/") # Mac
setwd("C:/Users/Iva Cukic/Dropbox/R Course/") # Windows
setwd("/Users/1033123/Dropbox/R Course/") # Managed Windows


# Read in data
data <- read.csv("am_data.csv", header=TRUE) 

#Inspect the data set
head(data)
names(data)
View(data)

# variable types
is.factor(data$sex) # is it categorical?
is.factor(data$age)
is.numeric(data$mmse) # is it continuous?
is.numeric(data$sex)
data$sex <- as.factor(data$sex)

# how many cases per category?
table(data$sex) # 1 == Men 2 == Women
table(data$disability) # 0 == absent 1 == present
table(data$sex, data$disability)

# how about numeric variables?
summary(data$age)
summary(data$mmse)
summary(data$ces.d)

### Basic manipulation ###

# rename a variable
names(data)
names(data)[1] <- "id" #telling R to go to the first variable, and give it name "id"
names(data)

# rename all variables
names(data) <- c("id","sex","age", "cognition", 
                 "depression", "disability")
names(data)

# rename levels of a factor
data$sex <- factor(data$sex, levels = c(1, 2), 
                   labels = c("Male", "Female"))
levels(data$sex) # 1 = Male, 2 = Female
table(data$sex)

# select only data for men
men <- subset(data, data$sex=="Male")

# DIY task:
# select only data for women
# select only data for non-disabled men
# what is the age range?

#change the odd value into NA
summary(data$age)
is.na(data$age) = data$age == 219
summary(data$age)

# How many NAs in cognition 
# and depression variables?

# What is the range for cognition and 
# deperession scores? 

#check distributions for continuous variables
hist() #variable you want to check in parentheses

#split into categories according to clinical cut-offs

#greater or equal 16 clinical depression
data$dep_cat <- NA
data$dep_cat[(data$depression>=16)] <- 1
data$dep_cat[(data$depression<16)] <- 0
table(data$dep_cat)

#DIY:make a categorical variable for cognition according to
# clinical cut off scores: 
#24-30 no impairment; 18-23 mild; 0-17 severe cognitive impairment
#add labels

#scatterplot cognition and depression
plot(data$cognition, data$depression)
?(plot) #see help for this function to see what else you can do
# ?() asks for help for any function, write the function in ()
#add labels to x and y axes
plot(data$cognition, data$depression, xlab="Cognition", ylab="Depression")

#save as image in your working directory
png(filename="first-plot.png")
plot(data$cognition, data$depression, xlab="Cognition", ylab="Depression")
dev.off()

#export an excel spreadsheet that contains new and changed variables
write.csv(data, file = "new_data.csv")

### correlation coefficient ###

install.packages("Hmisc")
library("Hmisc")
cor.test(data$cognition, data$depression)


### table of descriptives ###

install.packages("tableone")
library(tableone)

#continuous vars
tablevars <- c("age", "cognition", "depression")
tableOne <- CreateTableOne(vars=tablevars, strata=c("sex"),data=data) #stratified by sex
tableOne

tableOne <- CreateTableOne(vars=tablevars,data=data) #total
tableOne

#categorical vars
catVars <- c("disability")
catTableOverall <- CreateCatTable(vars = catVars,strata=c("sex"), data = data) #stratified by sex
catTableOverall

catTableOverall <- CreateCatTable(vars = catVars, data = data) #total
catTableOverall

