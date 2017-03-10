### ------------------------------------------------------- ###
### - PREDICTIVE MODELS FOR MEM CATEGORY DAILY SALE ------- ###
### - FIGURES PREDICTION FOR SPECIFIC STATES -------------- ###
### ------------------------------------------------------- ###

# in following cases, validation is not taken into account but will be later


library(lubridate) # to get number of days of a given month
# list the dates of months in order to use days_in_month() function from lubridate
dts <- as.Date(c("2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01"))

# select the months to construct training/validation/test datasets
months_trn <- 1:4 # first 4 months as training data
months_val <- 5 # 5th month as validation data
months_tst <- 6 # 6th month as test data

# form indices
train_ind <- seq(sum(days_in_month(dts)[months_trn]))
val_ind <- seq(days_in_month(dts)[months_val])+sum(days_in_month(dts)[months_trn]) 
test_ind <- seq(days_in_month(dts)[months_tst])+sum(days_in_month(dts)[seq(months_val)])

# Raw_MEM <- data.frame()
stats_perstate <- data.frame() # will be main dataset

### ------------------------------------------------------- ###
### - IMPORT DATA AND PREPROCESS FOR STATES --------------- ### DATASET 1
### ------------------------------------------------------- ###

# FIRST: GET ONLY THE MEM CATEGORY SALE FIGURES AND GET LOCATION INFO
# SECOND: GET ONLY MEM CATEGORY SALE FIGURES FOR ALL STATES (WASHINGTON WILL BE PREDICTED)
par.which.containingString <- function(x,ask,sep = " ",cl = NULL,nthreads = NULL){ # make the cluster as export
  require(parallel) # export the library
  force(x);force(ask);force(sep);
  if(is.null(cl)){ # if no cluster specified, make a new one
    if(is.null(nthreads)) nthreads = detectCores() # if number of cores is not specified, use all of them
    cl <- makeCluster(nthreads)
    clusterExport(cl,c("x","ask","sep"),envir = environment()) # introduce variables to cluster
    stopcl <- TRUE
  }
  Result <- na.omit(parSapply(cl,seq.int(x), function(i) # list the indexes which is containing given string
    ifelse(any(as.character(unlist(strsplit(x[i],split = sep))) %in% ask),i,NA)))
  if(stopcl)
    stopCluster(cl)
  return(Result)
}

# DATA FLOW 1 FOR DATA SET 1
for (i in c("Month7","Month8","Month9","Month10","Month11","Month12")){
  Raw_SaleData <- Import.SaleData(paste(i,"txt",sep=".")) # import and format raw sale data
  Raw_MEM <- Raw_SaleData[
    par.which.containingString(Raw_SaleData$ItemDescription,"MEM"),] # take only the MEM category
  unified_MEM <- cbind(Raw_MEM,par.LocationData(Raw_MEM,col.sen = 3,col.rec = 4)) # get additional data
  # get daily stats for a given category, for all states
  stats_perstate <- rbind(stats_perstate,par.sales.daily.perElement(raw_sale = unified_MEM,state.abb,16))
  remove(Raw_SaleData) # reduce memory usage
}

# unified_MEM <- rbind(unified_MEM,cbind(Raw_MEM,par.LocationData(Raw_MEM,col.sen = 3,col.rec = 4))) # get additional data)
# stats_perstate <- rbind(stats_perstate,sales.daily.perElement(raw_sale = unified_MEM,state.abb,16))
# remove(Raw_MEM) # reduce memory usage
print(stats_perstate)

# back up the content
write.csv(stats_perstate,"stats_perstate.csv",row.names = FALSE) # daily "MEM" category sale figures for each state
### ------------------------------------------------------- ###

### ------------------------------------------------------- ###
### - GET DAILY STATS FOR ONE STATE FOR 100 CATEGORIES ---- ### DATASET 2
### - FOR WASHINGTON -------------------------------------- ###
### ------------------------------------------------------- ###

# FIRST: GET LOCATION INFO AND TAKE JUST WASHINGTON STATE
# SECOND: GET DAILY SALE FIGURES OF ALL CATEGORIES FOR WASHINGTON
stats_daily_WA <- data.frame()
init_cats <- read.csv("categories_100.csv",row.names = NULL) # get bestseller 100 categories info

# DATA FLOW 2 FOR DATA SET 2
for (i in c("Month7","Month8","Month9","Month10","Month11","Month12")){
  Raw_SaleData <- Import.SaleData(paste(i,"txt",sep=".")) # import raw dataformat raw sale data
  cat("row number of raw sale data : ",nrow(Raw_SaleData),"\n")
  unified_MEM <- cbind(Raw_SaleData,par.LocationData(Raw_SaleData,col.sen = 3,col.rec = 4)) # get additional data
  cat("row number of unified sale data : ",nrow(unified_MEM),"\n")
  unified_MEM <- unified_MEM[which(unified_MEM$R.StateCode=="WA"),] # pull out just washington
  # Raw_MEM <- Raw_SaleData[
    # par.which.containingString(Raw_SaleData$ItemDescription,"MEM"),] # take only the MEM category
  # get 100 categories daily sales info for just WASHINGTON
  stats_daily_WA <- rbind(stats_daily_WA,sales.daily(unified_MEM,init_cats))
  remove(Raw_SaleData) # reduce memory usage
}

write.csv(stats_daily_WA,"stats_daily_WA.csv",row.names = FALSE) # back up the content
### ------------------------------------------------------- ###

### ------------------------------------------------------- ###
### - PREPROCESS FOR FINAL SHAPE TO PREPARE TRAINING, ----- ###
### - VALIDATION AND TEST DATA ---------------------------- ###
### ------------------------------------------------------- ###

library(lubridate) # to get number of days of a given month
# list the dates of months in order to use days_in_month() function from lubridate
dts <- as.Date(c("2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01"))

# select the months to construct training/validation/test datasets
months_trn <- 1:4 # first 4 months as training data
months_val <- 5 # 5th month as validation data
months_tst <- 6 # 6th month as test data

# form indices
train_ind <- seq(sum(days_in_month(dts)[months_trn])) 
val_ind <- seq(days_in_month(dts)[months_val])+sum(days_in_month(dts)[months_trn]) 
test_ind <- seq(days_in_month(dts)[months_tst])+sum(days_in_month(dts)[seq(months_val)])

### ------------------------------------------------------- ###
### - INITIATE A CLUSTER AND IMPORT DATASETS TO IT -------- ### FOR DATASET 1
### ------------------------------------------------------- ###

# REST IS NOT ARRANGED
library(h2o) # import h2o R package
# initiate a h2o cluster
h2o.init(
  nthreads=-1,            ## -1: use all available cores
  min_mem_size = "5G")    ## specify the memory size for the H2O cloud

# form datasets (in case the data has an unnecessary date column at 102, so exclude it)
train <- as.h2o(stats_perstate[train_ind,]) # take the first 4 months
valid <- as.h2o(stats_perstate[val_ind,])
test <- as.h2o(stats_perstate[test_ind,]) # predict last month

# assign values
train <- h2o.assign(train, "train.hex") ## assign the first result the R variable train and the H2O name train.hex
valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")

head(train)
# the index for the dataset, this column will be predicted
# in this case its is chosen as 2, the column index for MEM category
output_ind <- 48 # for WASHINGTON STATE
input_ind <- seq(ncol(train))[-output_ind]

# input a category name to include in a model's ID
model_tag <- "MEM"

# following datasets will be used for supplying data for plots
train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,output_ind]) # just take days and MEM sale figures
valid_df <- data.frame(Days=1:nrow(valid)+nrow(train),MEM=as.data.frame(valid)[,output_ind])
test_df <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(test)[,output_ind]) # just take days and MEM sale figures

# store in csv files if needed
# write.csv(train_df,"temp_train_df.csv",row.names = FALSE)
# write.csv(valid_df,"temp_valid_df.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df.csv",row.names = FALSE)

### ---------------------------------------------------------------- ###
### - RANDOM FOREST ------------------------------------------------ ### FOR DATASET 1
### ---------------------------------------------------------------- ###

rf1_states <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=input_ind,                  ## the predictor columns, by column index
  y=output_ind,                           ## the target index (what we are predicting)
  model_id = paste("RF",model_tag,sep = "_"), ## name the model in H2O not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1e6)                ## Set the random seed so that this can be reproduced.

###############################################################################
summary(rf1_states)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1_states@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1_states,valid = TRUE)[1,2] # 0.933
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalRf_predictions_state <- h2o.predict(object = rf1_states,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions_state

pred <- as.data.frame(finalRf_predictions_state) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_rf <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_rf)


### ---------------------------------------------------------------- ###
### - NEURAL NETWORKS ---------------------------------------------- ### FOR DATASET 1
### ---------------------------------------------------------------- ###

nn1_states <- h2o.deeplearning(training_frame = train,
                        validation_frame = valid,
                        x=input_ind,                  ## the predictor columns, by column index
                        y=output_ind,                 ## the target index (what we are predicting)
                        model_id = paste("nn",model_tag,sep = "_"),         ## name the model in H2O
                        epochs = 1000,                 ## number of iterations
                        hidden = c(200,200,200),       ## number and degrees of hidden layers
                        seed = 1e6)                    ## set a seed to reproduce the same results again

###############################################################################
summary(nn1_states)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

nn1_states@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(nn1_states,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalNN_predictions_states <- h2o.predict(
  object = nn1_states
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalNN_predictions_states
pred <- as.data.frame(finalNN_predictions_states) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_nn <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_nn)


### ---------------------------------------------------------------- ###
### - GRADIENT BOOST MACHINE --------------------------------------- ### FOR DATASET 1
### ---------------------------------------------------------------- ###

gbm1_states <- h2o.gbm(training_frame = train,
                validation_frame = valid,
                learn_rate = 0.5,
                x=input_ind,                  ## the predictor columns, by column index
                y=output_ind,
                model_id = paste("gbm",model_tag,sep = "_")         ## name the model in H2O
)
###############################################################################
summary(gbm1_states)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

gbm1_states@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
# h2o.hit_ratio_table(nn1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalGBM_predictions_states <- h2o.predict(
  object = gbm1_states
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalGBM_predictions_states
pred <- as.data.frame(finalGBM_predictions_states) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_gbm <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_gbm)

# write file for back up
write.csv(pred_rf,"MEM_WA_pred_rf.csv",row.names = FALSE)
write.csv(pred_nn,"MEM_WA_pred_nn.csv",row.names = FALSE)
write.csv(pred_gbm,"MEM_WA_pred_gbm.csv",row.names = FALSE)

# import ffrom file (transfer from server)
pred_rf <- read.csv("MEM_WA_pred_rf.csv",row.names = NULL)
pred_nn <- read.csv("MEM_WA_pred_nn.csv",row.names = NULL)
pred_gbm <- read.csv("MEM_WA_pred_gbm.csv",row.names = NULL)

### VISUALIZE RESULTS ####
library("plotly")

grob_ds_1 <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Train") %>%
  add_lines(x = valid_df$Days, y = valid_df$MEM, color = I("navy"), name = "Validation") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Test") %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  layout(title = "MEM Category Sales Prediction for WA<br>Training Data: MEM Sales in All States",
         xaxis = list(title = "Days"),#rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob_ds_1

htmlwidgets::saveWidget(as.widget(grob_ds_1),"MEM_SALEPREDICTION_VARIOUS_WA.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real",line=list(width=3.5)) %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  
  layout(title = "MEM Category Sales Prediction for WA<br>Training Data: MEM Sales in All States",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_rf$predict/test_df$MEM,color = I("red"),
                 name = "Ratio: RF",type = "scatter",mode = "lines",line = list(dash = "dash")) %>%
  add_lines(x = test_df$Days,y = pred_nn$predict/test_df$MEM,color = I("blue"),name = "Ratio: NN",line=list(dash = "dash")) %>%
  add_lines(x = test_df$Days,y = pred_gbm$predict/test_df$MEM,color = I("green"),name = "Ratio: GBM",line=list(dash = "dash")) %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_VARIOUS_WA.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)



### ------------------------------------------------------- ###
### - INITIATE A CLUSTER AND IMPORT DATASETS TO IT -------- ### FOR DATASET 2
### ------------------------------------------------------- ###

library(h2o) # import h2o R package
# initiate a h2o cluster
h2o.init(
  nthreads=-1,            ## -1: use all available cores
  min_mem_size = "5G")    ## specify the memory size for the H2O cloud

# form datasets (in case the data has an unnecessary date column at 102, so exclude it)
train <- as.h2o(stats_daily_WA[train_ind,]) # take the first 4 months
valid <- as.h2o(stats_daily_WA[val_ind,])
test <- as.h2o(stats_daily_WA[test_ind,]) # predict last month

# assign values
train <- h2o.assign(train, "train.hex") ## assign the first result the R variable train and the H2O name train.hex
valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")

head(train)
# the index for the dataset, this column will be predicted
# in this case its is chosen as 2, the column index for MEM category
output_ind <- 2
input_ind <- seq(ncol(train))[-output_ind]

# input a category name to include in a model's ID
model_tag <- "MEM"

# following datasets will be used for supplying data for plots
train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,output_ind]) # just take days and MEM sale figures
valid_df <- data.frame(Days=1:nrow(valid)+nrow(train),MEM=as.data.frame(valid)[,output_ind])
test_df <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(test)[,output_ind]) # just take days and MEM sale figures

# store in csv files if needed
# write.csv(train_df,"temp_train_df.csv",row.names = FALSE)
# write.csv(valid_df,"temp_valid_df.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df.csv",row.names = FALSE)

### ---------------------------------------------------------------- ###
### - RANDOM FOREST ------------------------------------------------ ### FOR DATASET 2
### ---------------------------------------------------------------- ###

rf1_states_2 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=input_ind,                  ## the predictor columns, by column index
  y=output_ind,                           ## the target index (what we are predicting)
  model_id = paste("RF",model_tag,sep = "_"), ## name the model in H2O not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1e6)                ## Set the random seed so that this can be reproduced.

###############################################################################
summary(rf1_states_2)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1_states_2@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1_states_2,valid = TRUE)[1,2] # 0.933
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalRf_predictions_state <- h2o.predict(object = rf1_states_2,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions_state

pred <- as.data.frame(finalRf_predictions_state) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_rf <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_rf)


### ---------------------------------------------------------------- ###
### - NEURAL NETWORKS ---------------------------------------------- ### FOR DATASET 2
### ---------------------------------------------------------------- ###

nn1_states_2 <- h2o.deeplearning(training_frame = train,
                               validation_frame = valid,
                               x=input_ind,                  ## the predictor columns, by column index
                               y=output_ind,                 ## the target index (what we are predicting)
                               model_id = paste("nn",model_tag,sep = "_"),         ## name the model in H2O
                               epochs = 1000,                 ## number of iterations
                               hidden = c(200,200,200),       ## number and degrees of hidden layers
                               seed = 1e6)                    ## set a seed to reproduce the same results again

###############################################################################
summary(nn1_states_2)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

nn1_states_2@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(nn1_states_2,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalNN_predictions_states <- h2o.predict(
  object = nn1_states_2
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalNN_predictions_states
pred <- as.data.frame(finalNN_predictions_states) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_nn <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_nn)


### ---------------------------------------------------------------- ###
### - GRADIENT BOOST MACHINE --------------------------------------- ###
### ---------------------------------------------------------------- ###

gbm1_states_2 <- h2o.gbm(training_frame = train,
                       validation_frame = valid,
                       learn_rate = 0.5,
                       x=input_ind,                  ## the predictor columns, by column index
                       y=output_ind,
                       model_id = paste("gbm",model_tag,sep = "_")         ## name the model in H2O
)
###############################################################################
summary(gbm1_states_2)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

gbm1_states_2@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
# h2o.hit_ratio_table(nn1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalGBM_predictions_states <- h2o.predict(
  object = gbm1_states_2
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalGBM_predictions_states
pred <- as.data.frame(finalGBM_predictions_states) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_gbm <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_gbm)

# back up the content
write.csv(pred_rf,"MEM_WA_2_pred_rf.csv",row.names = FALSE)
write.csv(pred_nn,"MEM_WA_2_pred_nn.csv",row.names = FALSE)
write.csv(pred_gbm,"MEM_WA_2_pred_gbm.csv",row.names = FALSE)

# import from file (to transfer from server)
pred_rf <- read.csv("MEM_WA_2_pred_rf.csv",row.names = NULL)
pred_nn <- read.csv("MEM_WA_2_pred_nn.csv",row.names = NULL)
pred_gbm <- read.csv("MEM_WA_2_pred_gbm.csv",row.names = NULL)


### VISUALIZE RESULTS ####
library("plotly")

grob_ds_2 <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Train") %>%
  add_lines(x = valid_df$Days, y = valid_df$MEM, color = I("navy"), name = "Validation") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Test") %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  layout(title = "MEM Category Sales Prediction for WA<br>Training Data: All Categories Sales in WA",
         xaxis = list(title = "Days"),#rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob_ds_2

htmlwidgets::saveWidget(as.widget(grob_ds_1),"MEM_SALEPREDICTION_VARIOUS_WA_2.html")

grob_merged <- subplot(grob_ds_1,grob_ds_2,nrows = 2,shareX = TRUE,margin = 0.01)
grob_merged

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real",line=list(width=3.5)) %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  
  layout(title = "MEM Category Sales Prediction for WA<br>Training Data: All Categories Sales in WA",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_rf$predict/test_df$MEM,color = I("red"),
                 name = "Ratio: RF",type = "scatter",mode = "lines",line = list(dash = "dash")) %>%
  add_lines(x = test_df$Days,y = pred_nn$predict/test_df$MEM,color = I("blue"),name = "Ratio: NN",line=list(dash = "dash")) %>%
  add_lines(x = test_df$Days,y = pred_gbm$predict/test_df$MEM,color = I("green"),name = "Ratio: GBM",line=list(dash = "dash")) %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_VARIOUS_WA.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)