### ------------------------------------------------------- ###
### - PREDICTIVE MODELS FOR MEM CATEGORY DAILY SALE ------- ###
### - FIGURES PREDICTION ---------------------------------- ###
### ------------------------------------------------------- ###

# PARALLEL VERSION
# THIS IS NOT FOR MULTIPLE CATEGORIES, WORKS FOR JUST ONE CATEGORY, ONE MONTH!
# input a list, function will search its elements in raw_sale in the column index col.target
# and return the number of sales per given element, daily
# for example: input the some zip numbers, it will list how many products are sold in one month to those zips
par.sales.daily.perElement <- function(raw_sale,source,col.target,cl = NULL,nthreads = NULL){
  require(lubridate)
  require(parallel)
  daynum <- days_in_month(raw_sale$ShippingDate[sample.int(nrow(raw_sale),1)])
  force(c(raw_sale,source,col.target))#;force(ask);force(sep);
  if(is.null(cl)){ # if no cluster specified, make a new one
    if(is.null(nthreads)) nthreads = detectCores() # if number of cores is not specified, use all of them
    cl <- makeCluster(nthreads)
    clusterExport(cl,c("raw_sale","source","col.target","day"),envir = environment()) # introduce variables to cluster, from current environment
    stopcl <- TRUE
  }
  Result <- parSapply(cl,source,function(x){ 
    temp <- raw_sale[which(raw_sale[,col.target]==x),c(2,7)] # only shipping date and unitsshipped
    sapply(seq.int(daynum),function(y) 
      sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })
  if(stopcl)
    stopCluster(cl)
  return(data.frame(Days=seq.int(daynum),Result))
}

### ------------------------------------------------------- ###
### - IMPORT DATA AND PREPROCESS FOR ZIPS ----------------- ###
### ------------------------------------------------------- ###

zipstocheck <- list()
for (i in c("Month7","Month8","Month9","Month10","Month11","Month12")){
  Raw_SaleData <- Import.SaleData(paste(i,"txt",sep=".")) # import raw sale data
  zipstocheck <- c(zipstocheck,levels(factor(Raw_MEM$ReceipentZip)))
  remove(Raw_SaleData) # reduce memory usage
}
postal <- unique(zipstocheck) # remove the repeating values

stats_perzip <- data.frame() # initiate
# for all months
for (i in c("Month7","Month8","Month9","Month10","Month11","Month12")){
  Raw_SaleData <- Import.SaleData(paste(i,"txt",sep=".")) # import raw sale data
  Raw_MEM <- Raw_SaleData[
    par.which.containingString(Raw_SaleData$ItemDescription,"MEM"),] # take only the MEM category
  # get daily stats for a given category, for receipent zips
  stats_perzip <- rbind(stats_perzip,par.sales.daily.perElement(raw_sale = Raw_MEM,postal,4))
  remove(Raw_SaleData) # reduce memory usage
}

colnames(stats_perzip) <- c("Days",postal)
print(stats_perzip[1:5,1:5]) # print values

write.csv(stats_perzip,"stats_perzip.csv",row.names = FALSE) # write to file for backup
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

library(h2o) # import h2o R package
# initiate a h2o cluster
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "6G")    ## specify the memory size for the H2O cloud

# form datasets in R environment
stats_perzip <- read.csv("stats_perzip.csv",row.names = NULL)
df_raw <- stats_perzip

# form datasets (in case the data has an unnecessary date column at 102, so exclude it)
train <- as.h2o(df_raw[train_ind,]) # take the first 4 months
valid <- as.h2o(df_raw[val_ind,])
test <- as.h2o(df_raw[test_ind,]) # predict last month

# assign values
train <- h2o.assign(train, "train.hex") ## assign the first result the R variable train and the H2O name train.hex
valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")

# the index for the dataset, this column will be predicted
# in this case we want to predict Seattle, WA and its zip code is 98104
output_ind <- which(colnames(train)=="98104")
input_ind <- seq(ncol(train))[-output_ind]

# input a category name to include in a model's ID
model_tag <- "MEM_Zip"

# following datasets will be used for supplying data for plots
train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,output_ind]) # just take days and sale figures
valid_df <- data.frame(Days=1:nrow(valid)+nrow(train),MEM=as.data.frame(valid)[,output_ind])
test_df <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(test)[,output_ind]) # just take days and MEM sale figures

# store in csv files if needed
# write.csv(train_df,"temp_train_df.csv",row.names = FALSE)
# write.csv(valid_df,"temp_valid_df.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df.csv",row.names = FALSE)

### ---------------------------------------------------------------- ###
### - RANDOM FOREST ------------------------------------------------ ###
### ---------------------------------------------------------------- ###

rf1 <- h2o.randomForest(         ## h2o.randomForest function
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
summary(rf1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
perf <- h2o.performance(rf1,test)
err_rf <- data.frame(MSE = h2o.mse(perf),RMSE = h2o.rmse(perf),MAE = h2o.mae(perf),RMSLE = h2o.rmsle(perf),
                  COR = cor(init_preds$RF,init_preds$Real))
###############################################################################

## Create predictions using our latest RF model against the test set.
finalRf_predictions<-h2o.predict(object = rf1,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions

pred <- as.data.frame(finalRf_predictions) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_rf <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_rf)


### ---------------------------------------------------------------- ###
### - NEURAL NETWORKS ---------------------------------------------- ###
### ---------------------------------------------------------------- ###

## epoch = 1000, hidden = c(100,100,100) is good. err -> 29715.37 without validation
## epoch = 1000, hidden = c(200,200,200) is good. err -> 28360.73 without validation
## epoch = 1000, hidden = c(200,200,200) is good. err -> 42341.01 with 10-fold validation
nn1 <- h2o.deeplearning(training_frame = train,
                        validation_frame = valid,
                        x=input_ind,                  ## the predictor columns, by column index
                        y=output_ind,                 ## the target index (what we are predicting)
                        model_id = paste("nn",model_tag,sep = "_"),         ## name the model in H2O
                        epochs = 100,                 ## number of iterations
                        hidden = c(200,200,200),       ## number and degrees of hidden layers
                        seed = 1e6)                    ## set a seed to reproduce the same results again

###############################################################################
summary(nn1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

nn1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(nn1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
perf <- h2o.performance(nn1,test)
err_nn <- data.frame(MSE = h2o.mse(perf),RMSE = h2o.rmse(perf),MAE = h2o.mae(perf),RMSLE = h2o.rmsle(perf),
                     COR = cor(init_preds$NN,init_preds$Real))
###############################################################################

## Create predictions using our latest RF model against the test set.
finalNN_predictions<-h2o.predict(
  object = nn1
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalNN_predictions
pred <- as.data.frame(finalNN_predictions) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_nn <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_nn)


### ---------------------------------------------------------------- ###
### - GRADIENT BOOST MACHINE --------------------------------------- ###
### ---------------------------------------------------------------- ###

gbm1 <- h2o.gbm(training_frame = train,
                validation_frame = valid,
                learn_rate = 0.5,
                x=input_ind,                  ## the predictor columns, by column index
                y=output_ind,
                model_id = paste("gbm",model_tag,sep = "_")         ## name the model in H2O
)
###############################################################################
summary(gbm1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

gbm1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
# h2o.hit_ratio_table(nn1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
perf <- h2o.performance(gbm1,test)
err_gbm <- data.frame(MSE = h2o.mse(perf),RMSE = h2o.rmse(perf),MAE = h2o.mae(perf),RMSLE = h2o.rmsle(perf),
                     COR = cor(init_preds$GBM,init_preds$Real))
###############################################################################

## Create predictions using our latest RF model against the test set.
finalGBM_predictions<-h2o.predict(
  object = gbm1
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalGBM_predictions
pred <- as.data.frame(finalGBM_predictions) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

# store the predicted values for plotting
pred_gbm <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_gbm)

# write all to file as a data frame for back up
init_train <- train_df
init_valid <- valid_df
init_preds <- data.frame(Days=test_df$Days,
                       Real=test_df$MEM,
                       NN=pred_nn$predict,
                       RF=pred_rf$predict,
                       GBM=pred_gbm$predict)
errors <- data.frame(RF = t(err_rf),NN = t(err_nn),GBM = t(err_gbm))

write.csv(init_train,"VAR_PRED_SEATTLE_TRAIN.csv",row.names = FALSE)
write.csv(init_valid,"VAR_PRED_SEATTLE_VALID.csv",row.names = FALSE)
write.csv(init_preds,"VAR_PRED_SEATTLE_PREDS.csv",row.names = FALSE)
write.csv(errors,"VAR_ERRORS_SEATTLE_PREDS.csv")

# load results, in this case the data is transferred from server
# init_train <- read.csv("VAR_PRED_SEATTLE_TRAIN.csv",row.names = NULL)
# init_valid <- read.csv("VAR_PRED_SEATTLE_VALID.csv",row.names = NULL)
# init_preds <- read.csv("VAR_PRED_SEATTLE_PREDS.csv",row.names = NULL)
errors <- read.csv("VAR_ERRORS_SEATTLE_PREDS.csv",row.names = 1)

### VISUALIZE RESULTS ####
library("plotly")

# all inclusive graph
grob <- plot_ly() %>%
  add_lines(x = init_train$Days, y = init_train$MEM, color = I("darkorchid"), name = "Train") %>%
  add_lines(x = init_valid$Days, y = init_valid$MEM, color = I("navy"), name = "Validation") %>%
  add_lines(x = init_preds$Days, y = init_preds$MEM, color = I("gray"), name = "Test",line = list(width=3.5)) %>%
  add_lines(x = init_preds$Days, y = init_preds$NN, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = init_preds$Days, y = init_preds$RF, color = I("red"), name = "Random Forest") %>%
  add_lines(x = init_preds$Days, y = init_preds$GBM, color = I("green"), name = "Gradient Boost Machine") %>%
  layout(title = "MEM Category Sales Prediction for Seattle,WA",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_SEATTLE_VARIOUSMODELS.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = init_preds$Days, y = init_preds$Real, color = I("black"), name = "Real") %>%
  add_lines(x = init_preds$Days, y = init_preds$RF, color = I("red"), name = "Random Forest") %>%
  add_lines(x = init_preds$Days, y = init_preds$NN, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = init_preds$Days, y = init_preds$GBM, color = I("green"), name = "Gradient Boost Machine") %>%
  
  layout(title = "MEM Category Sales Prediction for Seattle",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main # render the graph

p_sub <- plot_ly(x = init_preds$Days,y =init_preds$RF/init_preds$Real,color = I("red"),
                 name = "Ratio: RF",type = "scatter",mode = "lines") %>%
  add_lines(x = init_preds$Days,y = init_preds$NN/init_preds$Real,color = I("blue"),name = "Ratio: NN") %>%
  add_lines(x = init_preds$Days,y = init_preds$GBM/init_preds$Real,color = I("green"),name = "Ratio: GBM") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub # render the graph

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_SEATTLE_VARIOUS.html")

# comparison graph for prediction errors
p_err <- plot_ly() %>%
  add_lines(x = init_preds$Days, y = init_preds$Real, color = I("black"), name = "Real") %>%
  add_lines(x = init_preds$Days, y = init_preds$RF, color = I("red"), name = "Random Forest") %>%
  add_lines(x = init_preds$Days, y = init_preds$NN, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = init_preds$Days, y = init_preds$GBM, color = I("green"), name = "Gradient Boost Machine") %>%
  
  layout(title = "MEM Category Sales Prediction for Seattle",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_err # render the graph

# rearrange this part, always input y sa a column vector

p <- plot_ly(x = rownames(errors), y = errors$RF, type = 'bar', name = 'RF',color = I("red")) %>%
  add_trace(y = errors$NN, name = 'NN',color = I("blue")) %>%
  add_trace(y = errors$GBM, name = 'GBM',color = I("green")) %>%
  layout(yaxis = list(title = 'Count',type = "log"), barmode = 'group')
p

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
