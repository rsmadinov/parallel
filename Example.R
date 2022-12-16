# Install Necessary Packages.

packages <- c("doSNOW", "foreach")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Clean global environment
rm(list=ls())

# Registering cores for parallel process
library(doSNOW)
library(foreach)
# Example without parallel
inputData <- matrix(1:80000, ncol=4)
output_serial <- numeric() 
# Example with parallel
cl <-  makeCluster(4)
registerDoSNOW(cl)
allRowIndices <-
  c(1:nrow(inputData)) 
series_time <- system.time(
  for (rowNum in c(1:nrow(inputData))) {
    calculatedOutput <- inputData[rowNum, 1] - inputData[rowNum, 2] + inputData[rowNum, 3] / inputData[rowNum, 4] 
    output_serial <- c(output_serial, calculatedOutput) # append to output variable
  }
)
parallel_time <- system.time(
  output_parallel <- foreach (rowNum = allRowIndices, .combine = c) %dopar% {
    calculatedOutput <- inputData[rowNum, 1] - inputData[rowNum, 2] + inputData[rowNum, 3] / inputData[rowNum, 4] # compute output
    return (calculatedOutput)
  }
)






inputData <- matrix(1:1000000, ncol=4)
# Example without parallel
series_time <- system.time(
  for (rowNum in c(1:nrow(inputData))) {
    calculatedOutput <- inputData[rowNum, 1] - inputData[rowNum, 2] + inputData[rowNum, 3] / inputData[rowNum, 4] 
    
    output_serial <- c(output_serial, calculatedOutput)
  }
)
stopCluster(cl)
cl <- makeCluster(2, type = "SOCK") 
registerDoSNOW(cl)
allRowIndices <-
  c(1:nrow(inputData)) 
# Example with parallel
parallel_time <- system.time(
  output_parallel <- foreach (rowNum = allRowIndices, .combine = c) %dopar% {
    calculatedOutput <- inputData[rowNum, 1] - inputData[rowNum, 2] + inputData[rowNum, 3] / inputData[rowNum, 4] # compute output
    return (calculatedOutput)
  }
)










plotdata <- as.data.frame(matrix(rep(1:4),ncol=2))
plotdata$type <-  c("Series","Parallel")
plotdata$elapsed <- c(series_time[3],parallel_time[3])
openxlsx::write.xlsx(plotdata, "plotdata3.xlsx")

library(ggplot2)

ggplot(data=plotdata,aes(x=type,y=elapsed)) + geom_col()
