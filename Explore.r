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
##install.packages("ISLR")
##install.packages("glmnet")
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
library(ISLR)
library(glmnet)

testdata = read.csv("test.csv", header = T)
traindata = read.csv("train.csv", header = T)
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
require(scales)
ggplot(data = all[!is.na(all$SalePrice),],
       aes(x=SalePrice)) +
  geom_histogram(color='black',fill="red", binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by=250000), labels = scales::comma) +
  xlab("Sales Price in US Dollar ($)") + ylab("Frequency") +
  ggtitle("Histogram of Sale Price Distribution Ames, Iowa")

summary(all$SalePrice)
colSums(is.na(traindata))
apply(is.na(traindata), 2, sum)
sum(is.na(traindata))
##### we can see here a very left skew, the data is not noramlly distibuted
##### we will need to account for this when we build our model
#### we could get the log of this sales data to perhaps get rid of the skewness

####### Before Our Modelling We Will Need to Examine Missing Values By Each Variable
####### We will use integer values if columns are ordinal
####### we will use factors if character columns are non-oridinal
#### we can use model.matrix function later to convert factors to numbers (one hot encoding)
######## From line 72 to 487 we deal with our NA values #######################


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


##Electrical Systems
summary(all$Electrical) # 1 NA
## impute most common categorical value
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)
summary(all$Electrical) # no more NA

#### Type of Sales
summary(all$SaleType) # 1 NA
### impute most common type of sales value
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
summary(all$SaleType)
## no more NAs

#####################################
##### No More NAs !!!!!! ############
#####################################



#####################################
### We need to factorise variables with no NAs
### Examine if they are ordinal or categorical
########################################

### Lets Examine the other variables
summary(all$Foundation)
class(all$Foundation)
### not ordinal, leave as a factor

summary(all$Heating)
class(all$Heating)
### not ordianl, leave as factor

summary(all$RoofStyle)
class(all$RoofStyle)
## not ordinal, leave as factor

summary(all$RoofMatl)
class(all$RoofMatl)
### not ordinal, leave as factor

summary(all$LandContour)
class(all$LandContour)
### not ordinal, leave as factor

summary(all$LandSlope)
class(all$LandSlope)
### this is ordinal ordinal, label encoding needed
all$LandSlope = as.character(all$LandSlope)
all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(all$LandSlope)

summary(all$BldgType)
### from the data_description.txt file this may be ordinal
### we'll use a plot to examine thi
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

## Graph Shows no ordinality
all$BldgType = as.factor(all$BldgType)
table(all$BldgType)

summary(all$HouseStyle)
## no ordinality, use factors
class(all$HouseStyle)

summary(all$Neighborhood)
### lots of values here, not ordinal
class(all$Neighborhood)
## keep as factors

summary(all$Condition1)
## not ordinal, remain as factors
class(all$Condition1)

summary(all$Condition2)
## not ordinal, remain as factors
class(all$Condition2)

summary(all$Street)
### interesting in that it is ordinal but with two values
## we should label encode this

all$Street = as.character(all$Street)
all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)

summary(all$PavedDrive)
## ordinal value here, label encoding
all$PavedDrive = as.character(all$PavedDrive)
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$PavedDrive)


####################
### All Varaibles complete, and char vars are either factors or numeric labels
##### Year and Month Sold Should be Categorical
#### MSSub class uses numbers to code classes, these are categories of type of house
class(all$MoSold)
all$MoSold = as.factor(all$MoSold)
### we wont make Year a factor yet because we want to compute age

all$MSSubClass = as.factor(all$MSSubClass)
### using values from data_description.txt, we can revalue the factors
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
summary(all$MSSubClass)

###############################################################

### All char variables factored or label encoded using numbers

###############################################################


#####################################################################
### Now we should examine numeric Correlations against Sales Price
#####################################################################

#####  Get Correlation Values on All Numeric variables against SalesPrice
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars)
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))


#### Only take higly correlated variables
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]


#### Plot the Correlations
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

##################################
## we can see from here that the two highest correlations are
## 1. Overall Quality
## 2. Above Grade Living Area
#### I want to examine these against Sales price

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

##### We can see some outliers for this plot, look at grade 4 and grade 10, possibly even grade 9
##### Nothing that is very far from the rest of the values however


ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  xlab("Ground Level Area (Sqaure Ft.)") + ylab('Sale Price ($)') + ggtitle('Correlation Between Sale Price and Ground Level Area') +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))

#### Looking at this we can see house 525 and 1299 are outliers in terms of Livng Area
### Lets look at the Quality
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
### Both of these are quality 10, which means we may have to remove them as outliers
all <- all[-c(524, 1299),]

#############################
##### From Year of House Construction and Remodelling Year
##### We can create a house age variable
##############################

all$Remod = ifelse(all$YearBuilt==all$YearRemodAdd,0,1) # 0 is no remodel, 1 is remodel

all$Age = as.numeric(all$YrSold) - all$YearRemodAdd

cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])

################
## Create an isNew variable if house is ne
################

all$IsNew = ifelse(all$YrSold==all$YearBuilt,1,0)
table(all$IsNew) # 116 new houses

############
## We have Sqaure Feet for Living Space, but also basement Space
# we could consider basement as living space

all$TotalSqaureFeet = all$GrLivArea + all$TotalBsmtSF
cor(all$SalePrice, all$TotalSqaureFeet, use = 'pairwise.complete.obs')
#### Very high correlation here 0.779 rougly



####### High Correlated Variables, we need to examine this further
## We need to get rid of multicolinearity!!!!!!
#### These columns are from looking at the graph generated earlier
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
all = all[,!(names(all) %in% dropVars)]

### Year Remod add Correlated with: Exter Quality, Kitchen Quality (0.61)
## Garage Year Built correlated with: Year Built (0.85)
## Garage Area correlated with: Garage Cars (0.89)
## Total Basement SF =  X1st Floor SF (0.8)
##Rooms above ground correlated with: grl area (0.81)
#######################
## Seperate out numeric variables for preprocessing
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] 

numericVarNames = numericVarNames <- append(numericVarNames, c('Age'))

Numerics = all[, names(all) %in% numericVarNames]
Factors = all[,!(names(all) %in% numericVarNames)]
Factors = Factors[, names(Factors) != 'SalePrice']

######################
### Skewness #########
########################

## alot of the nueric data is skewed so we should examine this
##### using a loop we will examine skewness and take log of skewed data

for(i in 1:ncol(Numerics)){
  if (abs(skew(Numerics[,i]))>0.7){
    DFnumeric[,i] <- log(Numerics[,i] +1)
  }
}
PreNum = preProcess(Numerics, method=c("center", "scale"))
print(PreNum)
Normalised <- predict(PreNum, Numerics)
dim(Normalised)

#######################
### One Hot Encoding of Categorical vars
#### This is handy for machine learning algorithms

FactorsOneHot = as.data.frame(model.matrix(~.-1, Factors))
dim(FactorsOneHot)

######check if values were not in test set
##no point in keeping them if they're not
ZerocolTest <- which(colSums(FactorsOneHot[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(FactorsOneHot[ZerocolTest])

FactorsOneHot =FactorsOneHot[,-ZerocolTest]

ZerocolTrain <- which(colSums(FactorsOneHot[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(FactorsOneHot[ZerocolTrain])

FactorsOneHot <- FactorsOneHot[,-ZerocolTrain]

######### Lets also remove cols with less than 12 1's in the train set
fewOnes = which(colSums(FactorsOneHot[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(FactorsOneHot[fewOnes])
FactorsOneHot =  FactorsOneHot[,-fewOnes] #removing predictors
dim(FactorsOneHot)

newAll = cbind(Normalised, FactorsOneHot)


########## Skewness of Sales Price
######### Graph above showed right skew to sales price

skew(all$SalePrice)
par(mfrow=c(1,2))
qqnorm(all$SalePrice)
qqline(all$SalePrice)

####### skew here too high, we should take the log

all$SalePrice = log(all$SalePrice)

skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

ggplot(data = all[!is.na(all$SalePrice),],
       aes(x=SalePrice)) +
  geom_histogram(color='black',fill="red", binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by=250000), labels = scales::comma) +
  xlab("Sales Price in US Dollar ($)") + ylab("Frequency") +
  ggtitle("Histogram of Sale Price Distribution Ames, Iowa")


#########################################################################################
#### Lets put back the train and test data sets
newAll = cbind(newAll, all$SalePrice)
newTrain = newAll[!is.na(all$SalePrice),]
newTest = newAll[is.na(all$SalePrice),]
#########################################################################################
########### Test Data Needs to be Submitted as Answers on Kaggle
##########################################################################################
##### We will need to divide our train data to examine accuracy locally using sampling
##########################################################################################

eval_results <- function(true, predicted, df) {
  MAE = (sum(abs(true - predicted)))*(1/nrow(df))
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  x = 1 - R_square
  y = nrow(df) -1
  z = nrow(df) -  ncol(df) - 1 -1
  R_squareAdj = 1- (((x)*(y))/z)
  
  
  # Model performance metrics
  data.frame(
    MAE = MAE,
    RMSE = RMSE,
    Rsquare = R_square,
    RsqaureAdj = R_squareAdj
  )
  
}




## Lets sample data
head(newTrain$`all$SalePrice`)
newTrain = newTrain %>%
  rename(
    SalePrice = `all$SalePrice`
  )
head(newTrain$SalePrice)
set.seed(12345)
trainSubSet = sample(nrow(newTrain), 0.7*nrow(newTrain), replace = FALSE)
TrainSet = newTrain[trainSubSet,]
TestSet = newTrain[-trainSubSet,]
summary(TrainSet)
summary(TestSet)



## Extreme Gradient Boosting
XGBModel = train(SalePrice~.,
              data = TrainSet,
              method = 'xgbLinear',
              trControl = trainControl(method = 'cv', number = 5))



BestLambdaXGBoost = XGBModel$bestTune
BestLambdaXGBoost
plot(XGBModel, xvar='lambda', label = TRUE)
plot(XGBModel, xvar='eta', label = TRUE)
## nRounds = 100, lambda = 0.1, alpha = 0, eta = 0.3
## nRounds, max number of iteration
## eta: 0.3, 
##learning rate, low eta is slower computation, needs increased rounds
##value  between 0.1 and 0.3
## lambda: 1e-4 controls L2 regularisation on weights, used to avoid overfitting

#### Lets test this

Predictions = predict(XGBModel, TestSet)
eval_results(TestSet$SalePrice, XGBModel, TestSet)
RMSE(pred = Predictions, TestSet$SalePrice)
MAE(pred = Predictions, TestSet$SalePrice)
### MAE 0.09924094
## Root Mean Square Error (RMSE) of 0.1433918
RSqauredXGBoost =cor(TestSet$SalePrice,Predictions) ^ 2
## R Sqaured 0.08988504
RSqauredXGBoostAdj = 1-(1-RSqauredXGBoost)*((nrow(TestSet)-1)/(nrow(TestSet)-1-(ncol(TestSet)-1)))
RSqauredXGBoostAdj
#### 0.8165064
MAEXGB = (sum(abs(TestSet$SalePrice - Predictions)))*(1/nrow(TestSet))
MAEXGB
## 0.09924094
#### Ridge Regression
RidgeRegCaret = train(SalePrice~., data = TrainSet, method = 'glmnet',
                      trControl = trainControl(method = 'cv', number = 5),
                      tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by=0.0005)))
summary(RidgeRegCaret)
optimalLambda = RidgeRegCaret$bestTune$lambda
plot(RidgeRegCaret, xvar='lambda', label = TRUE)

lambda_best = LassoReg$bestTune$lambda
lambda_best

lassoVarImp <- varImp(RidgeRegCaret,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
lassoImportance
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')



RidgePred = predict(RidgeRegCaret, TestSet,s= optimalLambda)
eval_results(TestSet$SalePrice, RidgePred, TestSet)

### Ridge Regression
###   MAE      RMSE       Rsquare    RsqaureAdj
## 0.07935714 0.1167128 0.9277864  0.8772087

###### Lasso Regression: Least Absolute Shrinkage and Selection Operator


LassoReg = train(SalePrice~.,data = TrainSet, method = 'glmnet',
                 trControl = trainControl(method = 'cv', number = 5),
                 tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by=0.0005)))

plot(LassoReg, xvar='lambda', label = TRUE)


## Best Lambda
lambda_best = LassoReg$bestTune
lambda_best
#### lambda 0.0035, alpha 1

lassoVarImp <- varImp(LassoReg,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
lassoImportance
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
#### used 80 variables, did not use 98 variables
PredLasso = predict(LassoReg, TestSet, s= lambda_best)
eval_results(TestSet$SalePrice, PredLasso, TestSet)

#      MAE      RMSE      Rsquare   RsqaureAdj
#  0.07890707 0.1150837 0.9297882  0.8806126


######## Elastic Net Model

ENet = train(SalePrice~.,
              data = TrainSet,
              method = 'enet')
ENet$bestTune
### fraction 0.525, lambda 0.1
plot(ENet,
     plotType = 'scatter',
     metric = ENet$perfNames[1],
     digits = getOption('digits') - 5,
     xTrans = NULL)
######
## fraction: 0.525
### lambda 0.1
ENetPred = predict(ENet, TestSet, lamdba = 0.1, fraction= 0.525)
eval_results(TestSet$SalePrice, ENetPred, TestSet)
#      MAE      RMSE     Rsquare  RsqaureAdj
# 0.08446026 0.1181303 0.9260215  0.8742078

ENetTest = cbind(ENetPred, TestSet$SalePrice)
LassoTest = cbind(PredLasso, TestSet$SalePrice)
RidgeTest = cbind(RidgePred, TestSet$SalePrice)
XGBTest = cbind(Predictions, TestSet$SalePrice)
ENetTest = as.data.frame(ENetTest)
LassoTest = as.data.frame(LassoTest)
RidgeTest = as.data.frame(RidgeTest)
XGBTest = as.data.frame(XGBTest)

options(scipen=999)
par(mfrow=c(2,2))
ENetGraph = ggplot(data = ENetTest,
       aes(x = exp(V2), y = exp(ENetPred))) +
  geom_point(color='red') + geom_abline(intercept = 0) +
  xlab('Actual Sale Price ($)') + ylab('Predicted Sale Price ($)') + ggtitle('ENet') +
  scale_x_continuous(breaks = seq(0, 800000, by=500000), labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)
  

LassoGraph = ggplot(data = LassoTest,
       aes(x = exp(V2), y = exp(PredLasso))) +
  geom_point(color='blue') + geom_abline(intercept = 0) +
  xlab('Actual Sale Price ($)') + ylab('Predicted Sale Price ($)') + ggtitle('LASSO') +
  scale_x_continuous(breaks = seq(0, 800000, by=500000), labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

RidgeGraph= ggplot(data = RidgeTest,
       aes(x = exp(V2), y = exp(RidgePred))) +
  geom_point(color='green') + geom_abline(intercept = 0) +
  xlab('Actual Sale Price ($)') + ylab('Predicted Sale Price ($)') + ggtitle('RIDGE') +
  scale_x_continuous(breaks = seq(0, 800000, by=500000), labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

XGBGraph= ggplot(data = XGBTest,
       aes(x = exp(V2), y = exp(Predictions))) +
  geom_point(color='orange') + geom_abline(intercept = 0) +
  xlab('Actual Sale Price ($)') + ylab('Predicted Sale Price ($)') + ggtitle('XGBoost') +
  scale_x_continuous(breaks = seq(0, 800000, by=500000), labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

install.packages('ggpubr')
library(ggpubr)
grid.arrange(ENetGraph, LassoGraph, RidgeGraph, XGBGraph,
             ncol=2,nrow=2)

options(scipen=999)
par(mfrow=c(2,2))

plot(exp(TestSet$SalePrice), exp(ENetPred), pch = 16, cex = 0.8,
     col = 'orange', main = 'Elastic Net',
     xlab = 'Test Data House Sale Price',
     ylab = 'Predicted Sale Price')
abline(coef = c(0,1))


plot(exp(TestSet$SalePrice), exp(PredLasso), pch = 16, cex = 0.8,
     col = 'red', main = 'Lasso Model',
     xlab = 'Test Data House Sale Price',
     ylab = 'Predicted Sale Price')
abline(coef = c(0,1))


plot(exp(TestSet$SalePrice), exp(RidgePred), pch = 16, cex = 0.8,
     col = 'green', main = 'Ridge Model',
     xlab = 'Test Data House Sale Price',
     ylab = 'Predicted Sale Price')
abline(coef = c(0,1))



plot(exp(TestSet$SalePrice), exp(Predictions), pch = 16, cex = 0.8,
     col = 'blue', main = 'XGB Model',
     xlab = 'Test Data House Sale Price',
     ylab = 'Predicted Sale Price')
abline(coef = c(0,1))


