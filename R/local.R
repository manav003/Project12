# R Studio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(Hmisc)
library(caret)
# library(microbenchmark) using microbenchmark was taking much too long to run, so I used tictoc instead
library(tictoc)
library(parallel)
library(doParallel)

# Data Import and Cleaning (this section's code is copied from my project 11; since I requested an extension for Project 11 and turned it in late, I was not able to use feedback to make any changes for this project!)
data <- as_tibble(spss.get("../data/GSS2006.sav", use.value.labels=TRUE)) 

import <- data %>% 
  select(BIG5A1, BIG5A2, BIG5B1, BIG5B2, BIG5C1, BIG5C2, BIG5D1, BIG5D2, BIG5E1, BIG5E2, HEALTH) %>%
  filter_all(any_vars(!is.na(.))) %>%  #removes rows where ALL values in row are NA
  drop_na(HEALTH) %>%  # removes rows where our response variable, HEALTH, is NA
  mutate_all(. %>% 
               factor() %>% 
               as.numeric()
  )

clean <- import[!(rowSums(is.na(import)) == 10),]


# Analysis

tic()
extreme <- train(
  HEALTH ~ .*.*., 
  clean,
  method = "xgbLinear",
  tunelength = 2, 
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


cores <- detectCores()
local_cluster <- makeCluster(cores - 1)
registerDoParallel(local_cluster)

tic()
extreme <- train(
  HEALTH ~ .*.*., 
  clean,
  method = "xgbLinear",
  tunelength = 2, 
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

