# R script used in development
library(caret);library(stats);library(lattice);library(ggplot2);library(randomForest)
set.seed(32343)

### read in data, filter out columns with missing data, and first 6
SDATA       <- read.csv("pml-training.csv")         
SDATA       <- SDATA[,-(1:7)]
SDATA       <- SDATA[ , apply(SDATA, 2, function(x) !any(is.na(x)))]
SDATA       <- SDATA[ , SDATA[1,]!="" ]

inTrain     <- createDataPartition(SDATA$classe, p = 0.75, list = FALSE)
training    <- SDATA[inTrain, ]
testing     <- SDATA[-inTrain,]

MdlFit      <- randomForest(classe ~ ., data = training, ntree = 100) 
MdlPrd      <- predict(MdlFit,testing)
CM          <- confusionMatrix(MdlPrd,testing$classe)
CMTOT       <- CM$table

for (i in 1:15){
    inTrain     <- createDataPartition(SDATA$classe, p = 0.75, list = FALSE)
    training    <- SDATA[inTrain, ]
    testing     <- SDATA[-inTrain,]
    
    MdlFit      <- randomForest(classe ~ ., data = training, ntree = 100) 
    MdlPrd      <- predict(MdlFit,testing)
    CM          <- confusionMatrix(MdlPrd,testing$classe)
    CMTOT       <- CMTOT + CM$table
}

OoSError <- 1-sum(diag(CMTOT))/sum(CMTOT)
print(OoSError)