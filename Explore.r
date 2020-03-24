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

############## Misc Features NAs
summary(all$MiscFeature)
## 2814 NAs meaening no misc features, we'll replace that with none

all$MiscFeature = as.character(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)] = 'None'
all$MiscFeature = as.factor(all$MiscFeature)
table(all$MiscFeature)

### Alley NAs
summary(all$Alley)
all$Alley = as.character(all$Alley)
all$Alley[is.na(all$Alley)] = 'None'
all$Alley = as.factor(all$Alley)
table(all$Alley)

##Fence NAs
summary(all$Fence)
all$Fence = as.character(all$Fence)
all$Fence[is.na(all$Fence)] = 'None'
table(all$Fence)
all$Fence = as.factor(all$Fence)

## All Fireplaces, all NAs have no fireplaces
summary(all$FireplaceQu)

all$FireplaceQu = as.character(all$FireplaceQu)
all$FireplaceQu[is.na(all$FireplaceQu)] = 'None'
all$FireplaceQu = as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

## no missing value in fireplace num as it is a number, NAs being 0
table(all$Fireplaces)

############ Garage variables ###################
summary(all$GarageArea) ## 1 NA Value
summary(all$GarageCars) ##  1 NA Value
summary(all$GarageCond) ## 159 NA values
summary(all$GarageFinish) ## 159 NA values
summary(all$GarageQual) ## 159 NA values
summary(all$GarageType) ## 157 NA values
summary(all$GarageYrBlt) ## 159 NA values
summary(all$GrLivArea) ## no NAs,

### Garage Built Variabe is easy, we can replace this with year built
all$GarageYrBlt[is.na(all$GarageYrBlt)] = all$YearBuilt[is.na(all$GarageYrBlt)]
summary(all$GarageYrBlt)

## NA mean No garage, now lets check why there is a difference in garage type
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
### The 57 NAs in Garage Type are NA in the other four 159 NAs



kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish),
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

### House 2127 has a garage car, area and type but 2577 has no other garage variables

## impute common vars in house 2127
all$GarageCond[2127] = names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] = names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] = names(sort(-table(all$GarageFinish)))[1]

## show the results on house
kable(all[2127,c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType',
                 'GarageCond', 'GarageQual', 'GarageFinish')])

### Garage car and Garage area both have 1 NA
## we need to fix house 2577
all$GarageCars[2577] = 0
all$GarageArea[2577] = 0
all$GarageType[2577] = NA

## check if NAs of the variables are now 158
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

summary(all$GarageType)
### in this case NA means No Garage
all$GarageType = as.character(all$GarageType)
all$GarageType[is.na(all$GarageType)] = 'No Garage'
all$GarageType  = as.factor(all$GarageType)
table(all$GarageType)

#### Garage Finish means no garage, values are ordinal

all$GarageFinish = as.character(all$GarageFinish)
all$GarageFinish[is.na(all$GarageFinish)] = 'None'
Finish = c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

all$GarageFinish = as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)

### garage quality is ordinal 

all$GarageQual = as.character(all$GarageQual)
all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

### garage condition is ordinal 
all$GarageCond = as.character(all$GarageQual)
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond = as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)
