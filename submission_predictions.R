TESTDATA    <- read.csv("pml-testing.csv")
ODATA       <- read.csv("pml-training.csv") # filter columns based on original data set
TDATA       <- TESTDATA[,-(1:7)]
ODATA       <- ODATA  [,-(1:7)]
TDATA       <- TDATA[ , apply(TDATA, 2, function(x) !any(is.na(x)))]
TDATA       <- TDATA[ , TDATA[1,]!="" ]

MODELPREDICTION      <- predict(MdlFit,TDATA)
x <- MODELPREDICTION

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}