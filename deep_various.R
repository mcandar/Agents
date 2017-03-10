### ------------------------------------------------------- ###
### - PREDICTIVE MODELS FOR MEM CATEGORY DAILY SALE ------- ###
### - FIGURES PREDICTION ---------------------------------- ###
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

library(h2o) # import h2o R package
# initiate a h2o cluster
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "4G")    ## specify the memory size for the H2O cloud

# form datasets (in case the data has an unnecessary date column at 102, so exclude it)
train <- as.h2o(daily_stats[train_ind,-102]) # take the first 4 months
valid <- as.h2o(daily_stats[val_ind,-102])
test <- as.h2o(daily_stats[test_ind,-102]) # predict last month

# assign values
train <- h2o.assign(train, "train.hex") ## assign the first result the R variable train and the H2O name train.hex
valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")

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
                        epochs = 1000,                 ## number of iterations
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


### ---------------------------------------------------------------- ###
### - GENERALIZED LINEAR MODEL ------------------------------------- ###
### ---------------------------------------------------------------- ###

glm1 <- h2o.glm(training_frame = train,
                validation_frame = valid,
                max_iterations = 1e3,
                x=input_ind,                  ## the predictor columns, by column index
                y=output_ind,
                model_id = paste("gbm",model_tag,sep = "_")         ## name the model in H2O
)

###############################################################################
summary(glm1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

glm1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
# h2o.hit_ratio_table(nn1,valid = TRUE)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Create predictions using our latest RF model against the test set.
finalGLM_predictions<-h2o.predict(
  object = glm1
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalGLM_predictions
pred <- as.data.frame(finalGLM_predictions) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

pred_glm <- data.frame(Days=1:nrow(test)+nrow(train)+nrow(valid),MEM=as.data.frame(pred)) # GET PREDICTIONS
print(pred_glm)

write.csv(pred_rf,"temp_pred_rf.csv",row.names = FALSE)
write.csv(pred_nn,"temp_pred_nn.csv",row.names = FALSE)
write.csv(pred_gbm,"temp_pred_gbm.csv",row.names = FALSE)
write.csv(pred_glm,"temp_pred_glm.csv",row.names = FALSE)


### VISUALIZE RESULTS ####
library("plotly")

grob <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Train") %>%
  add_lines(x = valid_df$Days, y = valid_df$MEM, color = I("navy"), name = "Validation") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Test") %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  add_lines(x = test_df$Days, y = pred_glm$predict, color = I("yellow"), name = "Generalized Linear Model") %>%
  layout(title = "MEM Category Sales Prediction with Various Models",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_VARIOUSMODELS.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("black"), name = "Real") %>%
  add_lines(x = test_df$Days, y = pred_nn$predict, color = I("blue"), name = "Neural Nets") %>%
  add_lines(x = test_df$Days, y = pred_rf$predict, color = I("red"), name = "Random Forest") %>%
  add_lines(x = test_df$Days, y = pred_gbm$predict, color = I("green"), name = "Gradient Boost Machine") %>%
  add_lines(x = test_df$Days, y = pred_glm$predict, color = I("yellow"), name = "Generalized Linear Model") %>%
  
  layout(title = "MEM Category Sales Prediction with Various Models<br>Ratio Graph",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_rf$predict/test_df$MEM,color = I("red"),
                 name = "Ratio: RF",type = "scatter",mode = "lines") %>%
  add_lines(x = test_df$Days,y = pred_nn$predict/test_df$MEM,color = I("blue"),name = "Ratio: NN") %>%
  add_lines(x = test_df$Days,y = pred_gbm$predict/test_df$MEM,color = I("green"),name = "Ratio: GBM") %>%
  add_lines(x = test_df$Days,y = pred_glm$predict/test_df$MEM,color = I("yellow"),name = "Ratio: GLM") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_VARIOUS.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

# just to transfer datasets from remote server
train_df <- read.csv("temp_train_df.csv",row.names = NULL)
valid_df <- read.csv("temp_valid_df.csv",row.names = NULL)
test_df <- read.csv("temp_test_df.csv",row.names = NULL)
pred_rf <- read.csv("temp_pred_rf.csv",row.names = NULL)
pred_nn <- read.csv("temp_pred_nn.csv",row.names = NULL)
pred_gbm <- read.csv("temp_pred_gbm.csv",row.names = NULL)
pred_glm <- read.csv("temp_pred_glm.csv",row.names = NULL)

# aggreagate all variables into one data frame in order to import into file
init_all <- data.frame(Days=test_df$Days,
                       Real=test_df$MEM,
                       NN=pred_nn$predict,
                       RF=pred_rf$predict,
                       GBM=pred_gbm$predict,
                       GLM=pred_glm$predict)

write.table(init_all,"various_predictions.txt",sep=" ",row.names = FALSE)

# train_df <- read.csv("temp_train_df_gbm.csv",row.names = NULL)
# test_df <- read.csv("temp_test_df_gbm.csv",row.names = NULL)
# pred_df <- read.csv("temp_pred_df_gbm.csv",row.names = NULL)
# 
# train_df <- read.csv("temp_train_df_glm.csv",row.names = NULL)
# test_df <- read.csv("temp_test_df_glm.csv",row.names = NULL)
# pred_df <- read.csv("temp_pred_df_glm.csv",row.names = NULL)
# 
# train_df <- read.csv("temp_train_df_nn.csv",row.names = NULL)
# test_df <- read.csv("temp_test_df_nn.csv",row.names = NULL)
# pred_df <- read.csv("temp_pred_df_nn.csv",row.names = NULL)
# 
# train_df <- read.csv("temp_train_df.csv",row.names = NULL)
# test_df <- read.csv("temp_test_df.csv",row.names = NULL)
# pred_df <- read.csv("temp_pred_df.csv",row.names = NULL)



###################################
### ---------------------------------------------------------------- ###
### - GENERALIZED LINEAR MODEL ------------------------------------- ###
### ---------------------------------------------------------------- ###



matched_m10 <- par.Match.ShipData(Ship,month = 10,Raw_SaleData,filename = "PAR_TEST_MATCHED_MONTH10.csv")


