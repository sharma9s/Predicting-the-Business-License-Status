## R - Project : ZS-ML-40
# Topic - Predict Business License Status
# Submitted by - Saurabh Sharma

# Importing the Required Libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("DataExplorer")
install.packages("caTools")
install.packages("caret")
install.packages("randomForest")
library(randomForest)       # For Random Forest Function
library(caret)              # For Statistical Computations
library(stringr)            # For String Manipulations
library(DataExplorer)       # For plot_missing() function to check for NA's
library(dplyr)              # Filtering Functions
library(ggplot2)            # Graphs - Especially Boxplot
library(caTools)            # Also for Statistical Computing


# Importing the training and Testing Data Set

train <- read.csv("C:/Users/Sharma/Documents/train_file.csv",
                              sep = ",", header = T)
test <- read.csv("C:/Users/Sharma/Documents/test_file.csv",
                 sep = ",",header = T)

plot_missing(train)         # Tells us about the Missing Values (NA's) in dataset
plot_missing(test)

## Copying the dataset as we never alter or modify the Original dataset.
trainpra <- train 

# Structure/Summary/Details of the Data Set
str(trainpra)               
summary(trainpra)
glimpse(trainpra)

glimpse(trainpra)
summary(trainpra)

## EDA (Exploratory Data Analysis)

## To Check for outliers in SSA - draw a boxplot
boxplot(trainpra$SSA) # No Outliers

## Function to calculate Mode for Continuos and Categorical Variable
getmode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

# Handling the NA's

#1. WARD - replacing them by mode for avoiding skewness
boxplot(trainpra$WARD)$out
trainpra$WARD[is.na(trainpra$WARD)] <- getmode(trainpra$WARD)

#2.PRECINCT
trainpra$PRECINCT[is.na(trainpra$PRECINCT)] <- getmode(trainpra$PRECINCT)

#3. WARD.PRECINCT - dropping the column as it is a amalgamation of ward and precinct
trainpra$WARD.PRECINCT <- NULL

#4. POLICE.DISTRICT
trainpra$POLICE.DISTRICT[is.na(trainpra$POLICE.DISTRICT)] <- getmode(trainpra$POLICE.DISTRICT)

#5. LICENSE.NUMBER
trainpra <- trainpra[(!is.na(trainpra$LICENSE.NUMBER)),]

#6. SSA
trainpra$SSA[is.na(trainpra$SSA)] <- 0 # Replacing the NA's by 0; since these are do not contribute to
# cheat funds

#7. LATITUDE & LONGITUDE
trainpra$LATITUDE[is.na(trainpra$LATITUDE)] <- getmode(trainpra$LATITUDE)
trainpra$LONGITUDE[is.na(trainpra$LONGITUDE)] <- getmode(trainpra$LONGITUDE)

plot_missing(trainpra) #checking whether NA's remain or not.

# Converting the Datatype of concerned Variables to their appropriate data type
trainpra$ID <- as.character(trainpra$ID)
trainpra$LEGAL.NAME <- as.character(trainpra$LEGAL.NAME)
trainpra$APPLICATION.TYPE <- as.character(trainpra$APPLICATION.TYPE)
trainpra$DOING.BUSINESS.AS.NAME <- as.character(trainpra$DOING.BUSINESS.AS.NAME)
trainpra$ADDRESS <- as.character(trainpra$ADDRESS)
trainpra$CITY <- as.character(trainpra$CITY)
trainpra$STATE <- as.character(trainpra$STATE)
trainpra$ZIP.CODE <- as.double(trainpra$ZIP.CODE)
trainpra$LICENSE.DESCRIPTION <- as.character(trainpra$LICENSE.DESCRIPTION)
trainpra$APPLICATION.TYPE <- as.character(trainpra$APPLICATION.TYPE)
trainpra$APPLICATION.CREATED.DATE <- NULL
trainpra$APPLICATION.REQUIREMENTS.COMPLETE <- as.character(trainpra$APPLICATION.REQUIREMENTS.COMPLETE)
trainpra$PAYMENT.DATE <- as.character(trainpra$PAYMENT.DATE)
trainpra$LICENSE.TERM.START.DATE <- as.character(trainpra$LICENSE.TERM.START.DATE)
trainpra$LICENSE.TERM.EXPIRATION.DATE <- as.character(trainpra$LICENSE.TERM.EXPIRATION.DATE)
trainpra$LICENSE.APPROVED.FOR.ISSUANCE <- as.character(trainpra$LICENSE.APPROVED.FOR.ISSUANCE)
trainpra$DATE.ISSUED <- as.character(trainpra$DATE.ISSUED)
trainpra$LICENSE.STATUS.CHANGE.DATE <- NULL
trainpra$LOCATION <- NULL


####### Cleaning The Test Data Set
### Origingal Test Data set
testpr <- test
summary(testpr)
glimpse(testpr)
glimpse(trainpra)

## Checking levels of train and test data
levels(testpr$CONDITIONAL.APPROVAL)
levels(trainpra$CONDITIONAL.APPROVAL)


# Handling NA's of the Test Data set

#1. ZIP-CODe
testpr$ZIP.CODE[is.na(testpr$ZIP.CODE)] <- getmode(testpr$ZIP.CODE)

#2. WARD
testpr$WARD[is.na(testpr$WARD)] <- getmode(testpr$WARD) 

#3. Precinct
testpr$PRECINCT[is.na(testpr$PRECINCT)] <- getmode(testpr$PRECINCT)

#4. Police Disrict
testpr$POLICE.DISTRICT[is.na(testpr$POLICE.DISTRICT)] <- getmode(testpr$POLICE.DISTRICT)

#5. SSA
testpr$SSA[is.na(testpr$SSA)] <- 0

#6. Latitude and Longitude
testpr$LATITUDE[is.na(testpr$LATITUDE)] <- getmode(testpr$LATITUDE)
testpr$LONGITUDE[is.na(testpr$LONGITUDE)] <- getmode(testpr$LONGITUDE)


# Deleting the Useless Columns
testpr$WARD.PRECINCT <- NULL
testpr$LICENSE.STATUS.CHANGE.DATE <- NULL
testpr$LOCATION <- NULL
testpr$APPLICATION.CREATED.DATE <- NULL

glimpse(testpr)
glimpse(trainpra)

# Test data set - data type conversion
testpr$ID <- as.character(testpr$ID)
testpr$LEGAL.NAME <- as.character(testpr$LEGAL.NAME)
testpr$DOING.BUSINESS.AS.NAME <- as.character(testpr$DOING.BUSINESS.AS.NAME)
testpr$ADDRESS <- as.character(testpr$ADDRESS)
testpr$CITY <- as.character(testpr$CITY)
testpr$STATE <- as.character(testpr$STATE)
testpr$ZIP.CODE <- as.double(testpr$ZIP.CODE)
testpr$APPLICATION.TYPE <- as.character(testpr$APPLICATION.TYPE)
testpr$WARD <- as.double(testpr$WARD)
testpr$PRECINCT <- as.double(testpr$PRECINCT)
testpr$POLICE.DISTRICT <- as.double(testpr$POLICE.DISTRICT)
testpr$LICENSE.DESCRIPTION <- as.character(testpr$LICENSE.DESCRIPTION)
testpr$LICENSE.NUMBER <- as.double(testpr$LICENSE.NUMBER)
testpr$APPLICATION.REQUIREMENTS.COMPLETE <- as.character(testpr$APPLICATION.REQUIREMENTS.COMPLETE)
testpr$PAYMENT.DATE <- as.character(testpr$PAYMENT.DATE)
testpr$LICENSE.TERM.START.DATE <- as.character(testpr$LICENSE.TERM.START.DATE)
testpr$LICENSE.TERM.EXPIRATION.DATE <- as.character(testpr$LICENSE.TERM.EXPIRATION.DATE)
testpr$LICENSE.APPROVED.FOR.ISSUANCE <- as.character(testpr$LICENSE.APPROVED.FOR.ISSUANCE)
testpr$DATE.ISSUED <- as.character(testpr$DATE.ISSUED)



### Splitting the trainpr data set into train and test ###

inTrain <- createDataPartition(y = trainpra$LICENSE.STATUS, p = 0.70, list = F)
training <- trainpra[inTrain,]
testing <- trainpra[-inTrain,]
dim(training)
dim(testing)
glimpse(training)


## Building the Model on the Training Data Set

rf1 <- randomForest(LICENSE.STATUS ~ LICENSE.ID + ACCOUNT.NUMBER + SITE.NUMBER +
                   ZIP.CODE + WARD + PRECINCT + POLICE.DISTRICT + LICENSE.CODE +
                   LICENSE.NUMBER + CONDITIONAL.APPROVAL +
                   SSA,   
                   data = training, ntree = 500, mtry = 4, importance =TRUE)
## mtry = 4 is the optimum value.
## Print the Values as well as confusion Matrix
print(rf1)

## Predicting on Test Data set drawn of trainpra
glimpse(testing)
predtest <- predict(rf1, testing , type = "class")
predtest

# Checking classification accuracy
mean(predtest == testing$LICENSE.STATUS)                    
table(predtest,testing$LICENSE.STATUS)

## Getting the Important Variables
varImpPlot(rf1)

## Now predicting the original test data set ##
output <- predict(rf1, testpr, type = "class")
output
## This output Variable contains the LICENSE.STATUS predicted bye the Model.

testpr$ID

## Writing the Final Output (Predicted License Status by ID) to a .csv File
x <- data.frame("ID" = testpr$ID, "LICENSE STATUS" = output, stringsAsFactors = FALSE )
x
write.csv(x, "ZS-ML-40-OutputFile1.csv", row.names = FALSE)

