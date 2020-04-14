# R Studio API Code
#don't need this for MSI

# Libraries
library(tidyverse)
library(Hmisc)
library(caret)
# library(microbenchmark) using microbenchmark was taking much too long to run on the model, so I used tictoc instead
library(tictoc)
library(parallel)
library(doParallel)

# Data Import and Cleaning (this section's code is copied from my project 11; since I requested an extension for Project 11 and turned it in late, I was not able to use feedback to make any changes for this project!)
data <- as_tibble(spss.get("GSS2006.sav", use.value.labels=TRUE)) 


# using tic() and toc() multiple times, I found that selecting the columns as a vector in base R, instead of using tidyverse, saves approximately 0.3-0.5 seconds, which is significant relative to the overall time taken of the data import section (base R version takes approximately 0.1 sec and the tidyverse version takes 0.5 sec)

importFast <- data[, c(604, 561, 569, 565, 570, 566, 567, 562, 587, 577, 149)] %>%
  filter_all(any_vars(!is.na(.))) %>%  #removes rows where ALL values in row are NA
  drop_na(HEALTH) %>%  # removes rows where our response variable, HEALTH, is NA
  mutate_all(. %>% 
               factor() %>% 
               as.numeric()
  )


clean <- importFast[rowSums(is.na(importFast)) != 10,]

#try <- sapply(importFast, factor)

# Analysis

tic()
extreme <- train(
  HEALTH ~ .*.*., 
  clean,
  method = "xgbLinear",
  tunelength = 8, 
  preProcess = c("center", "scale", "zv", "medianImpute"),
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  ),
  na.action = na.pass
)
tocNP <- toc()
exec_time_np <-tocNP$toc - tocNP$tic


#running the two lines below allows us to take advantage of within-library parallelization
local_cluster <- makeCluster(60) #batch, instructed to use 60 cores
registerDoParallel(local_cluster)


tic()
extreme <- train(
  HEALTH ~ .*.*., 
  clean,
  method = "xgbLinear",
  tunelength = 8, 
  preProcess = c("center", "scale", "zv", "medianImpute"),
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  ),
  na.action = na.pass
)
tocP <- toc()
exec_time_p <-tocP$toc - tocP$tic

stopCluster(local_cluster)
registerDoSEQ()


timediff <- exec_time_np - exec_time_p

# The time taken for the non-parallel run of the model was 513.77 seconds and the time taken for the parallel run was 238.75 seconds. The parallel run was 275.02 seconds faster than the non parallel run.

# Adding to comment after running this file through Mangi: It ran through all my libraries and then showed the train model output (the list of folds and lambdas and alphas, etc). This output only showed up once (though the model appears twice in my code). After that, it said 'Aggregrating results' and showed the tuning parameters and sec elapsed for the non-parallel version and then the parallel version. I then went and checked my interactive.csv (using PuTTY) and saw that those exec times were saved appropriately (non-parallel run was 476.696 sec and parallel run was 194.858 sec).

#saving exec times

write.csv(c(execNP = exec_time_np, execP = exec_time_p), "../batch.csv") #saving in home directory, which is my Project12 folder
