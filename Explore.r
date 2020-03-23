install.packages("ggplot2")
install.packages("knitr")
install.packages("plyr")
install.packages("dplyr")
install.packages("corrplot")
install.packages("caret")
install.packages("gridExtra")
install.packages("scales")
install.packages("Rmisc")
install.packages("ggrepel")
install.packages("randomForest")
install.packages("psych")
install.packages("xgboost")
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)


testdata = read.csv("test.csv", header = T)
traindata = read.csv("train.csv", header = T)
sampleSubmission = read.csv("sample_submission.csv", header = T)

summary(testdata)

## MSSubClass: Identiifies the type of dwelling involved in sale
testdata$MSSubClass = as.factor(testdata$MSSubClass)
traindata$MSSubClass = as.factor(testdata$MSSubClass)

#### Need to you assign values to factor numbers, see data_description


### Some exploratory data analysis

dim(traindata)

### remove ID of houses and store for submission later
Test_IDs = testdata$Id
testdata$Id = NULL
traindata$Id = NULL

testdata$SalePrice = NA
all =  rbind(traindata, testdata)

dim(all)

################ Examing the Distrubution of Sales Data

ggplot(data = all[!is.na(all$SalePrice),],
       aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by=100000))

summary(all$SalePrice)
##### we can see here a very left skew, the data is not noramlly distibuted
##### we will need to account for this when we build our model


####### Before Our Modelling We Will Need to Examine Missing Values By Each Variable

### Pool NAs #######
summary(all$PoolQC)
all$PoolQC = as.character(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)] = 'None'
##### Now we will assign numeric values to the different qualities

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC = as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
##### from this table we can see that the three houses with a pool area but no pool quality
### Looking at the house qualities of those houses we can assign pool quality value based on house value

all$PoolQC[2421] = 2
all$PoolQC[2504] = 3
all$PoolQC[2600] = 2

### End of Pool NAs
###################
