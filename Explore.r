##install.packages("ggplot2")
##install.packages("knitr")
##install.packages("plyr")
##install.packages("dplyr")
##install.packages("corrplot")
##install.packages("caret")
##install.packages("gridExtra")
##install.packages("scales")
##install.packages("Rmisc")
##install.packages("ggrepel")
##install.packages("randomForest")
##install.packages("psych")
##install.packages("xgboost")
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
####### We will use integer values if columns are ordinal
####### we will use factors if character columns are non-oridinal
#### we can use model.matrix function later to convert factors to numbers


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

table(all$GarageCond)
### garage condition is ordinal 
all$GarageCond = as.character(all$GarageCond)
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond = as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

############# Basement Values
summary(all$BsmtCond) ## 82 NA
summary(all$BsmtExposure) ## 82 NA
summary(all$BsmtFinSF1) # 1 NA
summary(all$BsmtFinType1) # 79 NA
summary(all$BsmtFinSF2) ### 1 NA
summary(all$BsmtFinType2) ## 80 NA
summary(all$BsmtFullBath) ## 2 NA
summary(all$BsmtHalfBath) ## 2 NA
summary(all$BsmtQual) ## 81 NA
summary(all$BsmtUnfSF) ## 1 NA
summary(all$TotalBsmtSF) ## 1 NA

### BsmtFinType1 has 79 NAs, lets look at this
all[!is.na(all$BsmtFinType1) & 
      (is.na(all$BsmtCond)|
         is.na(all$BsmtQual)|
         is.na(all$BsmtExposure)|
         is.na(all$BsmtFinType2)),
    c('BsmtFinType1','BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType2')]

## Based on these rows, it seems that 79 house have no basement,
## all the other rows with NAs are mostly complete, we can impute values here
### using most common variable values
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

## Basement Quality is Ordinal value
all$BsmtQual = as.character(all$BsmtQual)
all$BsmtQual[is.na(all$BsmtQual)] = 'None'
all$BsmtQual = as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

## Basement Condition is Ordinal
all$BsmtCond = as.character(all$BsmtQual)
all$BsmtCond[is.na(all$BsmtCond)] = 'None'
table(all$BsmtCond)
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

### Basement Exposore Ordinal
table(all$BsmtExposure)
all$BsmtExposure = as.character(all$BsmtExposure)
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

## Basement Finish Type 1 is Ordinal, fin type values from data desccription
all$BsmtFinType1 = as.character(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)

### Basement Finished Type 2 Oridinal, living conditions from data description
all$BsmtFinType2 = as.character(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)


#### Now lets look at whats left in the NAs
all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

##### these dont have basements and we will impute them as 0
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
table(all$BsmtFullBath)
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
table(all$BsmtHalfBath)
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


#### Lot Variables
summary(all$LotArea)
summary(all$LotConfig)
summary(all$LotShape)
summary(all$LotFrontage) # 486 NAs!!

#### For the Lot Frontage Variable, we could impute the median per neighbourhood
## this is because lot frontage is the feet of street connected to property

### Plot which shows median lot frontage by neighbourhood
## we can use these figures for imputation
ggplot(all[!is.na(all$LotFrontage),],
       aes(x=as.factor(Neighborhood),
           y = LotFrontage)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  xlab("Neighbourhood") + ylab("Lot Frontage") + 
  theme(axis.text.x = element_text(angle = 95, hjust = 1))
### for all rows with NA as LotFrontage, impute median value of Lot Frontage By what the Neighbourhood is
for (i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
  }
}

### No more NAs for Lot Frontage!!

### LotShape is Ordinal so we will follow int approach here
all$LotShape = as.character(all$LotShape)

all$LotShape = as.integer(revalue(all$LotShape, c('IR3' =0, 'IR2' =1, 'IR1' =2, 'Reg' =3)))
table(all$LotShape)
sum(table(all$LotShape))
#####

### Lot Config, non-ordinal, so we will convert to a factor

all$LotConfig = as.factor(all$LotConfig)
table(all$LotConfig)
sum(table(all$LotConfig))

summary(all$LotArea)
summary(all$LotConfig)
summary(all$LotShape)
summary(all$LotFrontage)

#### No More NAs for Lot Config!!!

#### Masonry Variable NAs
summary(all$MasVnrArea) # 23 NAs
summary(all$MasVnrType) # 24 NAs
summary(all$MSSubClass)

### strange here, if there is a veneer type there should be an area
### lets check is veneer area NA are also veneer type NA
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

## lets find the one that should have veneer type
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

### house 2611, lets impute using the most common Vnr type
names(sort(-table(all$MasVnrType)))
## most common one so we will use BrkFace as the value
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2]
all[2611, c('MasVnrType', 'MasVnrArea')]
## fixed Masonry Veneer Type!

### 23 other houses

all$MasVnrType = as.character(all$MasVnrType)
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

### lets see if there is ordinality here by looking at the median veneer type
all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)

### Brick Common and None seems different to other types in terms of sales price
### we could assume stone and wood houses are considerably cheaper

### we will make this value ordinal

Masonry = c('None' =0, 'BrkCmn'=0, 'BrkFace' =1, 'Stone' =2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)


### Masonry Veneer area
# lets sub in 0 for NA
all$MasVnrArea[is.na(all$MasVnrArea)] = 0

summary(all$MasVnrArea) # 23 NAs
summary(all$MasVnrType) # 24 NAs

#### Fixed NAs for Masonry

###### Zoning NAs
table(all$MSZoning)
sum(is.na(all$MSZoning)) ## 4 NAs

##impute most occuring values
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)
sum(is.na(all$MSZoning))
## no more NAs for Zoning

### Kitchen NAs
summary(all$KitchenQual) ## 1 NA
summary(all$KitchenAbvGr) ## Complete, no NAs

### lets replace the 1 NA with most common value 'TA'
all$KitchenQual[is.na(all$KitchenQual)] = 'TA'
all$KitchenQual = as.character(all$KitchenQual)
### Ordinal Value
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)

## Kitchen Values Dealt with

### Utilities NAs
summary(all$Utilities) ## 2 NA
table(all$Utilities)
 #### only one house does not have all utilities
## whats noticible here is that this house is only in the train

## none are in the test set, makes it useless for predictions
### lets see the NAs and the NoSweWa
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])

#### this isnt helpful for prediction, so we should remove Utilities
all$Utilities = NULL

#### Got ridden of utilities

### Home Functionality
summary(all$Functional) ## 2NAs
## ordinal according to functionality
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional = as.character(all$Functional)
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))                                   
table(all$Functional)                           

### Functional NAs dealt with

#### Exterior Variables
summary(all$ExterCond) # no NAs
summary(all$Exterior1st) # 1 NA
summary(all$Exterior2nd) # 1 NA
summary(all$ExterQual) # no NAs

#### these values are categorical describing the exterior of a house
### use common value for imputation

all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)
sum(table(all$Exterior1st)) ## no NAs

all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)
sum(table(all$Exterior2nd)) ## no NAs

## ExterQual
## we should make this ordinal
all$ExterQual = as.character(all$ExterQual)
all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
table(all$ExterQual)

## Exter Cond
## ordinal value
all$ExterCond = as.character(all$ExterCond)
all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)
summary(all$ExterCond)
