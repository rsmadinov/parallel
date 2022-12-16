# Install Necessary Packages.

packages <- c("parallel", "dplyr", "future","microbenchmark","janeaustenr","tidyverse","foreach","doFuture","data.table","doMC","caret")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}




# Clean global environment
rm(list=ls())



# LOAD LIBRARIES
library(parallel)
library(tidyverse)
library(doParallel)





# Detect Cores 
detectCores()
num_cores <- detectCores(logical = FALSE) # detect number of physical cores
num_cores


# Function #1
generate_data <- function(nitem, nexaminee, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)} 
  else {
    warning("No seed provided!", call. = FALSE)
    seed <- sample.int(10000, 1)
    set.seed(seed)
    message("Random seed = ", seed, "\n")
  }
  itempar <- cbind(
    rnorm(nitem, mean = 1.13, sd = 0.25), #a
    rnorm(nitem, mean = 0.21, sd = 0.51), #b
    rnorm(nitem, mean = 0.16, sd = 0.05)) #c
  ability <- rnorm(nexaminee, mean = 0, sd = 1)
  respdata <- irtoys::sim(ip = itempar, x = ability)
  colnames(respdata) <- paste0("item", 1:nitem)
  data <- list(itempar = itempar,
               ability = ability,
               seed = seed,
               respdata = respdata)
  return(data)
}

# Function 2
estimate_par <- function(data, guess = -1) {
  # If guessing is fixed
  if(guess >= 0) {
    # Model set up
    mod3PL <- mirt::mirt(data, # response data
                         1,    # unidimensional model
                         guess = guess, # fixed guessing
                         verbose = FALSE, # Don't print verbose
                         # Increase the number of EM cycles
                         # Turn off estimation messages
                         technical = list(NCYCLES = 1000,
                                          message = FALSE)) 
  } else {
    mod3PL <- mirt::mirt(data, # response data
                         1,    # unidimensional model
                         itemtype = "3PL", # IRT model
                         verbose = FALSE, # Don't print verbose
                         # Increase the number of EM cycles
                         # Turn off estimation messages
                         technical = list(NCYCLES = 1000,
                                          message = FALSE)) 
  }
  # Extract item parameters in typical IRT metric
  itempar_est <- as.data.frame(mirt::coef(mod3PL, IRTpars = TRUE, simplify = TRUE)$item[,1:3])
  return(itempar_est)
}

# Function #3
summarize <- function(est_params, true_params) {
  result <- data.frame(
    parameter = c("a", "b", "c"),
    bias = sapply(1L:3L, function(i) mean((est_params[, i] - true_params[,i]))),
    rmse = sapply(1L:3L, function(i) sqrt(mean((est_params[, i] - true_params[,i])^2))),
    correlation = sapply(1L:3L, function(i) cor(est_params[, i], true_params[,i])))
  return(result)
}




iterations = 100 #Eventually this will be 100
seed = sample.int(10000, 100)
nitem = 20 #10, 15, 20, or 25
nexaminee = 1000 #250, 500, 750, or 1000
guess = -1 # A negative value or a value from 0 to 1

# No parallel computing
start_time <- Sys.time() # Starting time
series_time <- system.time(
  simresults <- foreach(i=1:iterations, 
                        .packages = c("mirt", "doParallel"),
                        .combine = rbind) %do% {
                          # Generate item parameters and data
                          step1 <- generate_data(nitem=nitem, nexaminee=nexaminee, seed=seed[i])
                          # Estimate item parameters
                          step2 <- estimate_par(step1$respdata, guess = guess)
                          # Summarize results
                          summarize(step2, step1$itempar)
                        }
)

# With parallel computing
cl <- makeCluster(4) # Register four clusters
registerDoParallel(cl)

start_time <- Sys.time() # Starting time
parallel_time <- system.time(
  simresults <- foreach(i = 1:iterations, 
                        .packages = c("mirt", "doParallel"),
                        .combine = rbind) %dopar% {
                          # Generate item parameters and data
                          step1 <- generate_data(nitem=nitem, nexaminee=nexaminee, seed=seed[i])
                          # Estimate item parameters
                          step2 <- estimate_par(step1$respdata, guess = guess)
                          # Summarize results
                          summarize(step2, step1$itempar)
                        }
  
)
stopCluster(cl)





# Plot graph

plotdata <- as.data.frame(matrix(rep(1:4),ncol=2))
plotdata$type <-  c("Series","Parallel")
plotdata$elapsed <- c(round(series_time[3], digits = 2),
                      round(parallel_time[3], digits = 2))

ggplot(data = plotdata,aes(x = type,y = elapsed))  +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes( label = elapsed), vjust = 1.6, color = "white", size = 3.5)+
  theme_minimal()


openxlsx::write.xlsx(plotdata, "plotdata.xlsx")

