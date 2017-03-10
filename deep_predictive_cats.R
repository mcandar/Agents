###################################################################################
### Goal: demonstrate usage of H2O's Random Forest and GBM algorithms
### Task: Predicting forest cover type from cartographic variables only
###       The actual forest cover type for a given observation 
###       (30 x 30 meter cell) was determined from the US Forest Service (USFS).

### Note: If run from plain R, execute R in the directory of this script. If run from RStudio, 
### be sure to setwd() to the location of this script. h2o.init() starts H2O in R's current 
### working directory. h2o.importFile() looks for files from the perspective of where H2O was 
### started.


## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

## Load a file from disk
# df <- h2o.importFile(path = normalizePath("../data/covtype.full.csv"))
df <- h2o.importFile(path = "https://raw.githubusercontent.com/h2oai/h2o-tutorials/master/tutorials/data/covtype.full.csv")
df

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process. 
splits <- h2o.splitFrame(
  df,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
## assign the first result the R variable train
## and the H2O name train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:12,                        ## the predictor columns, by column index
  y=13,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
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
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.
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
h2o.hit_ratio_table(rf1,valid = T)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Now we will try GBM. 
## First we will use all default settings, and then make some changes,
##  where the parameters and defaults are described.

gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:12,                        ## the predictor columns, by column index
  y=13,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

h2o.hit_ratio_table(gbm1,valid = T)[1,2]
## Overall accuracy.

## This default GBM is much worse than our original random forest.
## The GBM is far from converging, so there are three primary knobs to adjust
##  to get our performance up if we want to keep a similar run time.
## 1: Adding trees will help. The default is 50.
## 2: Increasing the learning rate will also help. The contribution of each
##  tree will be stronger, so the model will move further away from the
##  overall mean.
## 3: Increasing the depth will help. This is the parameter that is the least
##  straightforward. Tuning trees and learning rate both have direct impact
##  that is easy to understand. Changing the depth means you are adjusting
##  the "weakness" of each learner. Adding depth makes each tree fit the data
##  closer. 
##
## The first configuration will attack depth the most, since we've seen the
##  random forest focus on a continuous variable (elevation) and 40-class factor
##  (soil type) the most.
##
## Also we will take a look at how to review a model while it is running.

###############################################################################
gbm2 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=1:12,                     ##
  y=13,                       ## 
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##

#### While this is running, we can actually look at the model.
#### To do this we simply need a new connection to H2O.
#### This R console will run the model, so we need either another R console
####   or the web browser (or python, etc.).
#### In the demo, we will use Flow in our web browser
####  http://localhost:54321
#### And the focus will be to look at model performance, since we are using R to 
####  control H2O. So we can simply type in:
####  getModel "gbm_covType2"
###############################################################################

summary(gbm2)
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the new model's accuracy
###############################################################################

## This has moved us in the right direction, but still lower accuracy 
##  than the random forest.
## And it still has not converged, so we can make it more aggressive.
## We can now add the stochastic nature of random forest into the GBM
##  using some of the new H2O settings. This will help generalize 
##  and also provide a quicker runtime, so we can add a few more trees.

gbm3 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=1:12,                     ##
  y=13,                       ## 
  ntrees = 30,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.3,           ## increase the learning rate even further
  max_depth = 10,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)             ##
###############################################################################

summary(gbm3)
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest model's accuracy
###############################################################################

## Now the GBM is close to the initial random forest.
## However, we used a default random forest. 
## Random forest's primary strength is how well it runs with standard
##  parameters. And while there are only a few parameters to tune, we can 
##  experiment with those to see if it will make a difference.
## The main parameters to tune are the tree depth and the mtries, which
##  is the number of predictors to use.
## The default depth of trees is 20. It is common to increase this number,
##  to the point that in some implementations, the depth is unlimited.
##  We will increase ours from 20 to 30.
## Note that the default mtries depends on whether classification or regression
##  is being run. The default for classification is one-third of the columns.
##  The default for regression is the square root of the number of columns.

rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  validation_frame = valid,     ##
  x=1:12,                       ##
  y=13,                         ##
  model_id = "rf_covType2",     ## 
  ntrees = 200,                 ##
  max_depth = 30,               ## Increase depth, from 20
  stopping_rounds = 2,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,     ##
  seed=3000000)                 ##
###############################################################################
summary(rf2)
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest GBM accuracy
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## original random forest accuracy
h2o.hit_ratio_table(rf2,valid = T)[1,2]     ## newest random forest accuracy
###############################################################################

## So we now have our accuracy up beyond 95%. 
## We have witheld an extra test set to ensure that after all the parameter
##  tuning we have done, repeatedly applied to the validation data, that our
##  model produces similar results against the third data set. 

## Create predictions using our latest RF model against the test set.
finalRf_predictions<-h2o.predict(
  object = rf2
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions

## Compare these predictions to the accuracy we got from our experimentation
h2o.hit_ratio_table(rf2,valid = T)[1,2]             ## validation set accuracy
mean(finalRf_predictions$predict==test$Cover_Type)  ## test set accuracy

## We have very similar error rates on both sets, so it would not seem
##  that we have overfit the validation set through our experimentation.
##
## This concludes the demo, but what might we try next, if we were to continue?
##
## We could further experiment with deeper trees or a higher percentage of
##  columns used (mtries).
## Also we could experiment with the nbins and nbins_cats settings to control
##  the H2O splitting.
## The general guidance is to lower the number to increase generalization
##  (avoid overfitting), increase to better fit the distribution.
## A good example of adjusting this value is for nbins_cats to be increased
##  to match the number of values in a category. Though usually unnecessary,
##  if a problem has a very important categorical predictor, this can 
##  improve performance.
##
## Also, we can tune our GBM more and surely get better performance.
## The GBM will converge a little slower for optimal accuracy, so if we 
##  were to relax our runtime requirements a little bit, we could balance
##  the learn rate and number of trees used.
## In a production setting where fine-grain accuracy is beneficial, it is 
##  common to set the learn rate to a very small number, such as 0.01 or less,
##  and add trees to match. Use of early stopping is very powerful to allow 
##  the setting of a low learning rate and then building as many trees as 
##  needed until the desired convergence is met.
## As with random forest, we can also adjust nbins and nbins_cats.


### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)




######## A TEST MODEL ATTEMPT #########

## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "4G")    ## specify the memory size for the H2O cloud

train <- as.h2o(daily_stats[1:153,]) # take the first five months
# valid <- as.h2o(daily_stats[124:153,])
test <- as.h2o(daily_stats[154:184,]) # predict last month

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process. 

train <- h2o.assign(train, "train.hex") ## assign the first result the R variable train
## and the H2O name train.hex
# valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex") 

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## IN ORDER TO PREDICT "MEM" CATEGORY SALES, column index is 2
## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  # validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(1,3:101),                  ## the predictor columns, by column index
  y=2,                           ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
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
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.
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
finalRf_predictions<-h2o.predict(
  object = rf1
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions

pred <- as.data.frame(finalRf_predictions) # prediction results
pred[which(pred$predict < 0),] <- 0 # correct the values under 0

train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,2]) # just take days and MEM sale figures
test_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(test)[,2]) # just take days and MEM sale figures
pred_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(pred)) # GET PREDICTIONS

err <- pred/as.data.frame(test_df[,2])
summary(err)
sum(na.omit(abs(pred-as.data.frame(test_df[,2]))))

# # just for data transfer form server to local
# write.csv(train_df,"temp_train_df.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df.csv",row.names = FALSE)
# write.csv(pred_df,"temp_pred_df.csv",row.names = FALSE)

# just a transfer from server
train_df <- read.csv("temp_train_df.csv",row.names = NULL)
test_df <- read.csv("temp_test_df.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df.csv",row.names = NULL)


### VISUALIZE RESULTS ####
library("plotly")

grob <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "train") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              # color = I("gray95"), name = "95% confidence") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              # color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "test") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "prediction") %>%
  layout(title = "MEM Category Sales Prediction with Random Forest<br>(60% Train,%20 Validation,%30 Test)",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_RANDOMFOREST.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "test") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "prediction") %>%
  layout(title = "MEM Category Sales Prediction with Random Forest<br>Ratio Graph for Consistency",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_df$predict/test_df$MEM,name = "Ratio",type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_Consistency_MEM.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)



### A TEST MODEL ATTEMPT WITH DEEP NEURAL NETWORKS #### ON LINUX SERVER
## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "6G")    ## specify the memory size for the H2O cloud
# h2o.removeAll() # Clean slate - just in case the cluster was already running

# h2o.clusterInfo()
## Load a file from disk
# df <- as.h2o(daily_stats) # import as h2o
train <- as.h2o(daily_stats[1:153,]) # take the first five months
# valid <- as.h2o(daily_stats[124:153,])
test <- as.h2o(daily_stats[154:184,]) # predict last month

train <- as.h2o(daily_stats[1:31,]) # take the first five months
test <- as.h2o(daily_stats[32:62,]) # predict last month

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process. 

train <- h2o.assign(train, "train.hex")
## assign the first result the R variable train
## and the H2O name train.hex
# valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## IN ORDER TO PREDICT "MEM" CATEGORY SALES, column index is 2
## run our first predictive model
# rf1 <- h2o.randomForest(         ## h2o.randomForest function
#   training_frame = train,        ## the H2O frame for training
#   validation_frame = valid,      ## the H2O frame for validation (not required)
#   x=c(1,3:101),                  ## the predictor columns, by column index
#   y=2,                           ## the target index (what we are predicting)
#   model_id = "rf_covType_v1",    ## name the model in H2O
#   ##   not required, but helps use Flow
#   ntrees = 200,                  ## use a maximum of 200 trees to create the
#   ##  random forest model. The default is 50.
#   ##  I have increased it because I will let 
#   ##  the early stopping criteria decide when
#   ##  the random forest is sufficiently accurate
#   stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
#   ##  average is within 0.001 (default) of 
#   ##  the prior two 2-tree averages.
#   ##  Can be thought of as a convergence setting
#   score_each_iteration = T,      ## Predict against training and validation for
#   ##  each tree. Default will skip several.
#   seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.

## epoch = 1000, hidden = c(100,100,100) is good. err -> 29715.37 without validation
## epoch = 1000, hidden = c(200,200,200) is good. err -> 28360.73 without validation
## epoch = 1000, hidden = c(200,200,200) is good. err -> 42341.01 with 10-fold validation
nn1 <- h2o.deeplearning(training_frame = train,
                        # validation_frame = valid,
                        # nfolds = 10,
                        x=c(1,3:101),                  ## the predictor columns, by column index
                        y=2,                           ## the target index (what we are predicting)
                        model_id = "nn_MEM_1",         ## name the model in H2O
                        epochs = 1000,                ## number of iterations
                        hidden = c(200,200,200),     ## number and degrees of hidden layers
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

train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,2]) # just take days and MEM sale figures
test_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(test)[,2]) # just take days and MEM sale figures
pred_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(pred)) # GET PREDICTIONS

err <- pred/as.data.frame(test_df[,2])
summary(err)
sum(na.omit(abs(pred-as.data.frame(test_df[,2]))))

# # just for data transfer form server to local
# write.csv(train_df,"temp_train_df_nn.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df_nn.csv",row.names = FALSE)
# write.csv(pred_df,"temp_pred_df_nn.csv",row.names = FALSE)

# just a transfer from server
train_df <- read.csv("temp_train_df_nn.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_nn.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_nn.csv",row.names = NULL)


### VISUALIZE RESULTS ####
library("plotly")

grob <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Real (Train)") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
  # color = I("gray95"), name = "95% confidence") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
  # color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real (Test)") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Neural Network<br>(First month is traindata, second month is testdata)",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_NEURALNETWORK_1_1.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Neural Network<br>Ratio Graph",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_df$predict/test_df$MEM,name = "Ratio",type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_NN_1_1.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)


### A TEST WITH GRADIENT BOOST MACHINE

## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "6G")    ## specify the memory size for the H2O cloud
# h2o.removeAll() # Clean slate - just in case the cluster was already running

# h2o.clusterInfo()
## Load a file from disk
# df <- as.h2o(daily_stats) # import as h2o
train <- as.h2o(daily_stats[1:153,]) # take the first five months
# valid <- as.h2o(daily_stats[124:153,])
test <- as.h2o(daily_stats[154:184,]) # predict last month

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process. 

train <- h2o.assign(train, "train.hex")
## assign the first result the R variable train
## and the H2O name train.hex
# valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## IN ORDER TO PREDICT "MEM" CATEGORY SALES, column index is 2
## run our first predictive model

gbm1 <- h2o.gbm(training_frame = train,
                learn_rate = 0.5,
                x=c(1,3:101),                  ## the predictor columns, by column index
                y=2,
                model_id = "gbm_MEM_1"         ## name the model in H2O
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

train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,2]) # just take days and MEM sale figures
test_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(test)[,2]) # just take days and MEM sale figures
pred_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(pred)) # GET PREDICTIONS

err <- pred/as.data.frame(test_df[,2])
summary(err)
sum(na.omit(abs(pred-as.data.frame(test_df[,2]))))

# # just for data transfer form server to local
# write.csv(train_df,"temp_train_df_gbm.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df_gbm.csv",row.names = FALSE)
# write.csv(pred_df,"temp_pred_df_gbm.csv",row.names = FALSE)

# just a transfer from server
train_df <- read.csv("temp_train_df_gbm.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_gbm.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_gbm.csv",row.names = NULL)


### VISUALIZE RESULTS ####
library("plotly")

grob <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Real (Train)") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
  # color = I("gray95"), name = "95% confidence") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
  # color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real (Test)") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Gradient Boost Machine<br>(First 5 months are traindata, sixth month is testdata)",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_GRADIENTBOOSTMACHINE.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Gradient Boost Machine<br>Ratio Graph",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_df$predict/test_df$MEM,name = "Ratio",type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_GBM.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)



### A TEST WITH KMEANS

## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  min_mem_size = "6G")    ## specify the memory size for the H2O cloud

train <- as.h2o(daily_stats[1:153,]) # take the first five months
# valid <- as.h2o(daily_stats[124:153,])
test <- as.h2o(daily_stats[154:184,]) # predict last month

train <- h2o.assign(train, "train.hex")
## assign the first result the R variable train
## and the H2O name train.hex
# valid <- h2o.assign(valid, "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(test, "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## IN ORDER TO PREDICT "MEM" CATEGORY SALES, column index is 2
## run our first predictive model

glm1 <- h2o.glm(training_frame = train,
                max_iterations = 1e3,
               x=c(1,3:101),                  ## the predictor columns, by column index
               y=2,
               model_id = "glm_MEM_1"         ## name the model in H2O
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

train_df <- data.frame(Days=1:nrow(train),MEM=as.data.frame(train)[,2]) # just take days and MEM sale figures
test_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(test)[,2]) # just take days and MEM sale figures
pred_df <- data.frame(Days=1:nrow(test)+nrow(train),MEM=as.data.frame(pred)) # GET PREDICTIONS

err <- pred/as.data.frame(test_df[,2])
summary(err)
sum(na.omit(abs(pred-as.data.frame(test_df[,2]))))

# # just for data transfer form server to local
# write.csv(train_df,"temp_train_df_glm.csv",row.names = FALSE)
# write.csv(test_df,"temp_test_df_glm.csv",row.names = FALSE)
# write.csv(pred_df,"temp_pred_df_glm.csv",row.names = FALSE)

# just a transfer from server
train_df <- read.csv("temp_train_df_glm.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_glm.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_glm.csv",row.names = NULL)


### VISUALIZE RESULTS ####
library("plotly")

grob <- plot_ly() %>%
  add_lines(x = train_df$Days, y = train_df$MEM,
            color = I("darkorchid"), name = "Real (Train)") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
  # color = I("gray95"), name = "95% confidence") %>%
  # add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
  # color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real (Test)") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Generalized Linear Model<br>(First 5 months are traindata, sixth month is testdata)",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

grob

htmlwidgets::saveWidget(as.widget(grob),"MEM_SALEPREDICTION_GENERALIZEDLINEARMODEL.html")

## ratio graph
p_main <- plot_ly() %>%
  add_lines(x = test_df$Days, y = test_df$MEM, color = I("gray"), name = "Real") %>%
  add_lines(x = test_df$Days, y = pred_df$predict, color = I("blue"), name = "Prediction") %>%
  layout(title = "MEM Category Sales Prediction with Generalized Linear Model<br>Ratio Graph",
         xaxis = list(title = "Days"),
         yaxis = list(title = "Number of Sales"))

p_main

p_sub <- plot_ly(x = test_df$Days,y = pred_df$predict/test_df$MEM,name = "Ratio",type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Days"),yaxis = list(title = "Ratio",type = "log"))

p_sub

p_merged <- subplot(p_main,p_sub,nrows = 2,shareX = TRUE,heights = c(0.75,0.25),margin = 0.01)

p_merged

htmlwidgets::saveWidget(as.widget(p_merged),"Ratio_MEM_GLM.html")

### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)





train_df <- read.csv("temp_train_df_gbm.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_gbm.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_gbm.csv",row.names = NULL)

train_df <- read.csv("temp_train_df_glm.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_glm.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_glm.csv",row.names = NULL)

train_df <- read.csv("temp_train_df_nn.csv",row.names = NULL)
test_df <- read.csv("temp_test_df_nn.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df_nn.csv",row.names = NULL)

train_df <- read.csv("temp_train_df.csv",row.names = NULL)
test_df <- read.csv("temp_test_df.csv",row.names = NULL)
pred_df <- read.csv("temp_pred_df.csv",row.names = NULL)