## hurriyet proj lib

# source("https://raw.githubusercontent.com/mcandar/Agents/master/clicklib.R")

# Required Libraries
if(!("plotly" %in% rownames(installed.packages()))) install.packages("plotly")
if(!("lubridate" %in% rownames(installed.packages()))) install.packages("lubridate")
if(!("htmlwidgets" %in% rownames(installed.packages()))) install.packages("htmlwidgets")
if(!("htmltools" %in% rownames(installed.packages()))) install.packages("htmltools")
if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools")
if(!("dygraphs" %in% rownames(installed.packages()))) install.packages("dygraphs")
if(!("nnet" %in% rownames(installed.packages()))) install.packages("nnet")
if(!("neuralnet" %in% rownames(installed.packages()))) install.packages("neuralnet")
if(!("geosphere" %in% rownames(installed.packages()))) install.packages("geosphere")
if(!("dtw" %in% rownames(installed.packages()))) install.packages("dtw")
if(!("highcharter" %in% rownames(installed.packages()))) install.packages("highcharter")
if(!("h2o" %in% rownames(installed.packages()))) install.packages("h2o") # requires source files, www.h2o.ai/download/
if(!("webshot" %in% rownames(installed.packages()))) install.packages("webshot")
if(!("RPostgreSQL" %in% rownames(installed.packages()))) install.packages("RPostgreSQL")

## data import with ease
clickstream.import <- function(filename)
  read.csv(filename,row.names = NULL,stringsAsFactors = FALSE,na.strings = "null")

## rearrange and clean the click data
clickstream.organize <- function(data, # click data to read
                                 exclude_null_columns = TRUE, # exclude the columns filled with null's
                                 convert_datetime = TRUE){ # convert datetime objects to POSIXlt or POSIXct
  
  
  library(lubridate)
  ## replace the nonsense characters with meaningful ones, update the source if found any new pair
  chars <- data.frame(find=as.character(c("Ä±","Ã¼","Ã§","Ã‡","ÅŸ","Ã¶","Ä°","ÄŸ","Ã–")),
                      replace=as.character(c("i","u","c","C","s","o","I","g","O")))
  for(i in 1:nrow(chars)){
    data$pg_titl <- gsub(pattern = chars[i,1],replacement = chars[i,2],data$pg_titl)
  }
  
  ## exclude null columns
  if(exclude_null_columns){
    valid_columns <- sapply(seq(ncol(data)),function(x){
      if(all(is.na(data[,x])))
        0 # do not take the columns filled with NA's
      else
        x # record otherwise, will be taken into considering later
    })
    
    valid_columns <- which(valid_columns != 0)
    # data <- data[,valid_columns]
  }
  print(valid_columns)
  data <- data[,valid_columns]
  
  tempind <- unique(c(which(lengths(strsplit(data$ev_tm,split = " ")) != 2), # indexes that time is not in a valid format
  which(lengths(strsplit(data$srvr_tm,split = " ")) != 2),
  which(lengths(strsplit(as.character(data$ssn_id),split = " ")) != 1),
  which(lengths(strsplit(data$country,split = " ")) != 1)))
  print("Excluded rows are:")
  print(tempind)
  if(length(tempind) != 0)
    data <- data[-tempind,]
  
  # convert datetime columns into more convenient forms like POSIXlt or POSIXct
  if(convert_datetime){
    data$ev_tm <- as_datetime(data$ev_tm)
    data$srvr_tm <- as_datetime(data$srvr_tm)
  }
  
  # data <- data[-which(year(data$ev_tm)>),]
  ## SHOULD BE EDITED FOR FOLLOWING YEARS, OR IN NEW YEAR'S EVE
  # freq <- table(year(data$srvr_tm))
  # freq <- freq[order(freq,decreasing = TRUE)]
  # data <- data[-which(year(data$srvr_tm)!=as.integer(names(freq[1]))),]
  
  return(data)
}

# merge columns of a row as a datetie object with format "yyyy-mm-dd HH:MM:SS"
merge.datetime.mins <- function(data)
  lubridate::as_datetime(paste(paste(data[,1],data[,2],data[,3],sep = "-"),paste(data[,4],data[,5],sep = ":")),tz = "GMT")

perc.error <- function(query,reference){
  if(length(query)==1){
    print("Error on single value.")
    if(reference == 0)
      return("Can't calculate due to zero divide.")
    return((abs(query-reference)/reference)*100)
  }
  else{
    print("Mean error on elementwise values.")
    temp <- data.frame(query=query,reference=reference)
    ind <- which(temp$reference == 0)
    if(length(ind)!=0)
      temp <- temp[-ind,] # exclude zero divisions
    return(mean(na.omit((abs(temp$query-temp$reference)/temp$reference)*100)))
  }
}

# split datetime into columns AND REDUCE INTO HOURLY INTERVALS (FOR PV)
clickstream.splitDatetime <- function(data,col.datetime,breaks = "hours",splitOnly=FALSE,...){
  if(!splitOnly){
    min <- data.frame(PV = table(cut(data[which(data$ev_nm == "PV"),"srvr_tm"], breaks="hours",...)))
    print("I do split.")
  }
  else{
    min <- data
    print("I skip splitting.")
  }
  
  library(lubridate)
  temp <- strsplit(as.character(as_datetime(min[,col.datetime])),split = " ")
  temp <- t(as.data.frame(temp))
  rownames(temp) <- seq(nrow(temp))
  temp <- cbind(t(as.data.frame(strsplit(temp[,1],split = "-"))),t(as.data.frame(strsplit(temp[,2],split = ":"))))
  temp <- as.data.frame(apply(temp,2,function(x) as.integer(as.character(x))))
  temp$V6 <- NULL
  colnames(temp) <- c("year","month","day","hour","min")
  min <- data.frame(temp,min[,-col.datetime])
  return(min)
}


# check this, is not working properly
holidays.2017 <- function(data,col.datetime){
  require(lubridate)
  holidays <- data.frame(year = rep(2017,16), 
                         month = c(1,4,5,5,6,6,6,6,8,8,9,9,9,9,10,10), 
                         day = c(1,23,1,19,24,25,26,27,30,31,1,2,3,4,28,29))
  
  Result <- apply(data.frame(year(data$datetime),month(data$datetime),day(data$datetime)),1,function(x){
    temp <- matrix(as.numeric(x),nrow = nrow(holidays),ncol = 3,byrow = T)
    any(((temp[,1] - holidays[,1]) == 0) & ((temp[,2] - holidays[,2]) == 0) & ((temp[,3] - holidays[,3]) == 0))
  })
  
  return(Result)
}

####THIS WORKS 
split.Datetime <- function(x){
  library(lubridate)
  temp <- strsplit(as.character(as_datetime(x)),split = " ")
  temp <- t(as.data.frame(temp))
  rownames(temp) <- seq(nrow(temp))
  temp <- cbind(t(as.data.frame(strsplit(temp[,1],split = "-"))),t(as.data.frame(strsplit(temp[,2],split = ":"))))
  temp <- as.data.frame(apply(temp,2,function(x) as.integer(as.character(x))))
  temp$V6 <- NULL
  colnames(temp) <- c("year","month","day","hour","min")
  return(temp)
}


## be careful about timezones, keep the tz arguments as "GMT"
merge.datetime.mins <- function(data)
  as.POSIXct(paste(paste(data[,1],data[,2],data[,3],sep = "-"),paste(data[,4],data[,5],sep = ":")),tz = "GMT")


## no treatment for NULL values, omit them
hidden.toStr <- function(hidden){
  len <- length(hidden)
  if(all(hidden == hidden[1]) & len > 1)
    return(paste(len,hidden[1],sep = "x"))
  if(all(hidden == hidden[1]) & len == 1)
    return(paste(len,hidden,sep = "x"))
  else
    return(hidden)
}


clickstream.importHourly <- function(filename,time_interval=NULL){
  result <- read.csv(filename,row.names = NULL,stringsAsFactors=FALSE)
  colnames(result) <- c("date","hour","pv")
  ### DONT FORGET TO ADD tz = "GMT" otherwise it will adapt to gmt and add or subtract some time
  result$datetime <- as.POSIXct(paste(result$date,paste(result$hour,"00",sep = ":")),tz = "GMT") # get the time as is
  if(is.null(time_interval))
    return(data.frame(datetime=result$datetime,PV=result$pv))
  else{
    result <- result[which(result$datetime %in% time_interval),]
    result <- result[order(result$datetime),]
    return(data.frame(datetime=result$datetime,PV=result$pv))
  }
}

# if a dataset is directly taken from redshift, convert it to more convenient form
# does not change timezones
clickstream.toHourly <- function(result,time_interval=NULL){
  colnames(result) <- c("date","hour","pv")
  result$datetime <- as.POSIXct(paste(result$date,paste(result$hour,"00",sep = ":")),tz = "GMT") # get the time as is
  if(is.null(time_interval))
    return(data.frame(datetime=result$datetime,PV=result$pv))
  else{
    result <- result[which(result$datetime %in% time_interval),]
    result <- result[order(result$datetime),]
    return(data.frame(datetime=result$datetime,PV=result$pv))
  }
}



## should be tested!
## TREAT WEEKENDS IN DIFFERENT WAYS
clickstream.importCurrencies <- function(filename, # filename to import
                                         time_interval = NULL, # specify a time sequence, exclude non-matching
                                         time_frame = "H1", # only works for H1 for now
                                         tz = "" # local time is default
                                         ){
  ohlc_colnames <- c("date","time","open","high","low","close","volume")
  if(time_frame == "H1"){
    result <- read.csv(filename,row.names = NULL,header = FALSE,stringsAsFactors = FALSE)
    colnames(result) <- ohlc_colnames
    result$datetime <- as.POSIXct(paste(result$date," ",
                                        result$time,":00",sep = ""),
                                  format = "%Y.%m.%d %H:%M",
                                  tz = tz)# subtracts 3 hours to adapt gmt
    
    if(is.null(time_interval))
      return(result[,c(8,3:7)])
    else{
      result <- result[which(result$datetime %in% time_interval),]
      result <- result[order(result$datetime),]
      return(result[,c(8,3:7)])
    }
  }
}


## input data frames into a list, and this will get the rows with common datetime info, specified as "time_interval"
clickstream.matchDatetime <- function(datalist, # bring all dataframes in a list
                                      time_interval, # a time sequence to take a basis
                                      column_datetime, # the column index of the dataframes to check datetime
                                      specify_columns_toinclude = NULL # which columns do you want to include?
){
  result <- data.frame(datetime=time_interval) # output as a dataframe
  n <- length(specify_columns_toinclude)
  for(i in 1:length(datalist)){
    if(is.null(specify_columns_toinclude))
      specify_columns_toinclude <- seq(ncol(datalist[[i]]))
    ## in current element of the list, just take the time-matching rows
    # print(head(datalist[[i]]))
    temp <- datalist[[i]][which(datalist[[i]][,column_datetime] %in% 
                                  time_interval),specify_columns_toinclude]
    
    # colnames(temp) <- paste(names(datalist)[i],colnames())
    result <- cbind(result,temp)
    index <- seq(n)+(n*(i))-(n-1)
    colnames(result)[index] <- paste(names(datalist)[i],colnames(datalist[[i]])[specify_columns_toinclude],sep = "_")
    print(index)
  }
  # colnames(result) <- c("datetime",as.character(paste("V",seq(ncol(result)-1)+1,sep = "")))
  return(result)
}


### needs further checks and improvements
## shifts time and response column
clickstream.align <- function(data,response.col,lag){
  index <- seq(nrow(data))+lag
  temp <- data[index,c(1,response.col)]
  result <- cbind(temp,data[,-c(1,response.col)])
  tm <- temp$datetime[nrow(temp)-lag]
  result$datetime <- c(na.omit(result$datetime),seq.POSIXt(from = tm + 3600, to = tm + lag*3600,by = "hours"))
  rownames(result) <- seq(nrow(result))
  return(result)
}


# # FURTHER CHECK THE FUNCTIONS !!
# ### needs further improvements and tests
# clickstream.h2o.buildandtest <- function(data, # the feed dataset
#          test.row, # number of DAYS to predict starting from the end
#          valid.row, # number of DAYS to use in validation starting from test
#          train.row="rest",
#          response.col=2,
#          beyond_data=FALSE, # compare
#          # max.lag = 31,
#          interpolation.step = NULL,
#          # use.onlyreal = TRUE,
#          nthreads = -1,
#          plot = FALSE,
#          errorinfo = FALSE,
#          ...
# ){
#   library(h2o)
#   h2o.init(nthreads = nthreads,min_mem_size = "6G")
#   h2o.removeAll()
#   
#   if(!is.null(interpolation.step)){ # apply interpolation
#     data <- interpolate(data,by = interpolation.step,months = 5)
#     interpolation.factor <- 1/interpolation.step
#   }
#   else # do not apply interpolation
#     interpolation.factor <- 1
#   
#   lag <- length(which(is.na(data[,response.col])))
#   
#   # indices to fractionate the data frame
#   if(beyond_data){ # cannot be comparable with real data, because this indexing is for predicting beyond the data
#     tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row
#     print(tes.ind)
#     val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)
#     print(val.ind)
#     if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
#       tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind))
#     print(tra.ind)
#   }
#   else{ # can be comparable with real data, prediction is inside the limits
#     tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row-lag
#     print(tes.ind)
#     val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)-lag
#     print(val.ind)
#     if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
#       tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind)-lag)
#     print(tra.ind)
#   }
#   
#   # prepare thedata frames
#   train <- h2o.assign(as.h2o(data[tra.ind,]),"train.hex")
#   valid <- h2o.assign(as.h2o(data[val.ind,]),"valid.hex")
#   # test <- h2o.assign(as.h2o(data[tes.ind,]),"test.hex")
#   print(train)
#   print(valid)
#   
#   model <- h2o.deeplearning(training_frame = train,
#                             validation_frame = valid,
#                             x=seq(ncol(data))[-c(response.col)],
#                             y=response.col,
#                             ...)
#   
#   if(errorinfo){
#     print(model@model$validation_metrics)
#     print(model@model$model_summary)
#     print(model@model$scoring_history)
#   }
#   
#   ## test data rearrangement, when the data is amplified, just use the real values to test
#   test <- data[tes.ind,] # form dataset
#   # if(use.onlyreal)
#   #   test <- test_init[which((test_init$Days %% 1)==0),] # take just integer days, rest is interpolation and not real
#   # else if(!use.onlyreal)
#   #   test <- test_init # take with interpolation values
#   
#   # print(test)
#   test <- h2o.assign(as.h2o(test),"test.hex") # convert to h2o
#   
#   print(test)
#   
#   prediction <- h2o.predict(object = model,newdata = test)
#   # print(as.data.frame(as.numeric(data[rel.ind[1:test.row],response.col])))
#   if(!beyond_data){
#     Result <- data.frame(Prediction=as.data.frame(as.numeric(prediction)),
#                          Real=as.data.frame(as.numeric(test))[,response.col]) # store predictions and real values in a data frame
#     colnames(Result) <- c("predict","Real")
#     print(Result)
#     # display error info
#     cat("\nRoot-Mean-Squared Error of Predictions :",rms(Result$predict-Result$Real))
#     cat("\nRoot-Mean-Squared Error of Predictions Per Day:",rms(Result$predict-Result$Real)/test.row,"or",
#         (rms(Result$predict-Result$Real)/sum(na.omit(Result$Real)))*100,"%")
#     cat("\nCorrelations of Prediction and Real Values :",cor(Result$predict,Result$Real))
#   }
#   else
#     Result <- data.frame(Prediction=as.data.frame(as.numeric(prediction)),
#                          Real=NA) # store predictions and real values in a data frame
#   
#   return(list(Result=Result,Model=model,Int=test))
# }

clickstream.h2o.buildandtest <- function(data, # the feed dataset
                                         test.row, # number of DAYS to predict starting from the end
                                         valid.row, # number of DAYS to use in validation starting from test
                                         train.row="rest",
                                         response.col=2,
                                         algorithm = "deeplearning",
                                         beyond_data=FALSE, # compare
                                         # max.lag = 31,
                                         interpolation.step = NULL,
                                         # use.onlyreal = TRUE,
                                         nthreads = -1,
                                         plot = FALSE,
                                         errorinfo = FALSE,
                                         ...
){
  library(h2o)
  h2o.init(nthreads = nthreads,min_mem_size = "6G")
  h2o.removeAll()
  
  if(!is.null(interpolation.step)){ # apply interpolation
    data <- interpolate(data,by = interpolation.step,months = 5)
    interpolation.factor <- 1/interpolation.step
  }
  else # do not apply interpolation
    interpolation.factor <- 1
  
  lag <- length(which(is.na(data[,response.col])))
  
  # indices to fractionate the data frame
  if(beyond_data){ # cannot be comparable with real data, because this indexing is for predicting beyond the data
    tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row
    print(tes.ind)
    val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)
    print(val.ind)
    if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
      tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind))
    print(tra.ind)
  }
  else{ # can be comparable with real data, prediction is inside the limits
    tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row-lag
    print(tes.ind)
    val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)-lag
    print(val.ind)
    if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
      tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind)-lag)
    print(tra.ind)
  }
  
  # prepare thedata frames
  train <- h2o.assign(as.h2o(data[tra.ind,]),"train.hex")
  valid <- h2o.assign(as.h2o(data[val.ind,]),"valid.hex")
  print(train)
  print(valid)
  
  switch(algorithm,
         "deeplearning" = {model <- h2o.deeplearning(training_frame = train,
                                                     validation_frame = valid,
                                                     x=seq(ncol(data))[-c(response.col)],
                                                     y=response.col,
                                                     ...)},
         "randomForest" = {model <- h2o.randomForest(training_frame = train,
                                                     validation_frame = valid,
                                                     x=seq(ncol(data))[-c(response.col)],
                                                     y=response.col,
                                                     ...)},
         "gbm"          = {model <- h2o.gbm(training_frame = train,
                                            validation_frame = valid,
                                            x=seq(ncol(data))[-c(response.col)],
                                            y=response.col,
                                            ...)})
  
  if(errorinfo){
    print(model@model$validation_metrics)
    print(model@model$model_summary)
    print(model@model$scoring_history)
  }
  
  test <- h2o.assign(as.h2o(data[tes.ind,]),"test.hex") # convert to h2o
  
  prediction <- h2o.predict(object = model,newdata = test)
  
  if(!beyond_data){
    Result <- data.frame(Prediction=as.data.frame(as.numeric(prediction)),
                         Real=as.data.frame(as.numeric(test))[,response.col]) # store predictions and real values in a data frame
    colnames(Result) <- c("predict","Real")
    print(Result)
    # display error info
    cat("\nRoot-Mean-Squared Error of Predictions :",rms(Result$predict-Result$Real))
    cat("\nRoot-Mean-Squared Error of Predictions Per Day:",rms(Result$predict-Result$Real)/test.row,"or",
        (rms(Result$predict-Result$Real)/sum(na.omit(Result$Real)))*100,"%")
    cat("\nCorrelations of Prediction and Real Values :",cor(Result$predict,Result$Real))
  }
  else
    Result <- data.frame(Prediction=as.data.frame(as.numeric(prediction)),
                         Real=NA) # store predictions and real values in a data frame
  
  return(list(Result=Result,Model=model,Int=test))
}


# FURTHER CHECK THE FUNCTIONS !!
# clickstream.h2o.plotresult <- function(data,
#                                        dates,
#                                        title = "Prediction",
#                                        subtitle = "", # to give model info
#                                        beyond_data = FALSE,
#                                        filename = NULL # name of the file if wanted to save, should be with ".html" extension
# ){
#   library(highcharter)
#   if(!beyond_data){
#     hc <- highchart() %>% 
#       hc_title(text = title) %>% 
#       hc_subtitle(text = subtitle) %>% 
#       hc_add_series_times_values(dates = dates,values = data$predict, id = "Prediction",name = "Prediction") %>% 
#       hc_add_series_times_values(dates = dates,values = data$Real, id = "Observed",name = "Observed")
#   }
#   else{
#     hc <- highchart() %>% 
#       hc_title(text = title) %>% 
#       hc_subtitle(text = subtitle) %>% 
#       hc_add_series_times_values(dates = dates,values = data$predict, id = "Prediction",name = "Prediction")
#   }
#   
#   if(!is.null(filename))
#     htmlwidgets::saveWidget(hc,filename)
#   return(hc)
# }
clickstream.h2o.plotresult <- function(data_aslist,
                                       # data,
                                       dates,
                                       title = "Prediction",
                                       subtitle = "", # to give model info
                                       beyond_data = FALSE,
                                       filename = NULL # name of the file if wanted to save, should be with ".html" extension
){
  library(highcharter)
  
  algorithm <- data_aslist$Model@algorithm
  switch(algorithm,
         "deeplearning" = {annot_text <- paste("Hidden:",hidden.toStr(data_aslist$Model@allparameters$hidden),"<br>",
                                               "Epoch:",data_aslist$Model@allparameters$epochs,"<br>",
                                               "Runtime:",data_aslist$Model@allparameters$max_runtime_secs,"secs <br>",
                                               "Activation:",data_aslist$Model@allparameters$activation,"<br>",
                                               "Stopping Rounds:",data_aslist$Model@allparameters$stopping_rounds,"<br>",
                                               "Stopping Tolerance:",data_aslist$Model@allparameters$stopping_tolerance,"<br>")},
         
         "drf" = {annot_text <- paste("Number of Trees:",data_aslist$Model@allparameters$ntrees,"<br>",
                                      "Maximum Depth:",data_aslist$Model@allparameters$max_depth,"<br>",
                                      "Minimum Rows:",data_aslist$Model@allparameters$min_rows,"<br>",
                                      "Number of Bins:",data_aslist$Model@allparameters$nbins,"<br>",
                                      "Max Depth:",data_aslist$Model@allparameters$stopping_rounds,"<br>",
                                      "Sample Rate:",data_aslist$Model@allparameters$sample_rate,"<br>")},
         
         "gbm" = {annot_text <- paste("Number of Trees:",data_aslist$Model@allparameters$ntrees,"<br>",
                                      "Maximum Depth:",data_aslist$Model@allparameters$max_depth,"<br>",
                                      "Minimum Rows:",data_aslist$Model@allparameters$min_rows,"<br>",
                                      "Max Depth:",data_aslist$Model@allparameters$stopping_rounds,"<br>",
                                      "Col Sample Rate:",data_aslist$Model@allparameters$col_sample_rate,"<br>",
                                      "Col Sample Rate Per Tree:",data_aslist$Model@allparameters$col_sample_rate_per_tree,"<br>",
                                      "Sample Rate:",data_aslist$Model@allparameters$sample_rate,"<br>")})
  
  if(!beyond_data){
    # hc <- highchart() %>% 
    #   hc_title(text = title) %>% 
    #   hc_subtitle(text = subtitle) %>% 
    #   hc_add_series_times_values(dates = dates,values = data_aslist$Result$predict, id = "Prediction",name = "Prediction") %>% 
    #   hc_add_series_times_values(dates = dates,values = data_aslist$Result$Real, id = "Observed",name = "Observed")
    
    hc <- highchart() %>% 
      hc_title(text = title) %>%  # rearrange the title and subtitle !!!!
      hc_subtitle(text = subtitle) %>% 
      hc_add_series_times_values(dates = dates,values = data_aslist$Result$predict, id = "Prediction",name = "Prediction") %>% 
      hc_add_series_times_values(dates = dates,values = data_aslist$Result[,2], id = "Observed",name = "Observed") %>%
      hc_add_annotation(title = list(text = annot_text,
                                     style = list(color = rgb(0.3,0.3,0.3), # dark gray
                                                  fontFamily = "Lucida Sans Unicode")), # same font with titles
                        x = 125,y = 170)
  }
  else{
    hc <- highchart() %>% 
      hc_title(text = title) %>% 
      hc_subtitle(text = subtitle) %>% 
      hc_add_series_times_values(dates = dates,values = data_aslist$Result$predict, id = "Prediction",name = "Prediction") %>%
      hc_add_annotation(title = list(text = annot_text,
                                     style = list(color = rgb(0.3,0.3,0.3), # dark gray
                                                  fontFamily = "Lucida Sans Unicode")), # same font with titles
                        x = 125,y = 170)
  }
  
  if(!is.null(filename))
    htmlwidgets::saveWidget(hc,filename)
  return(hc)
}



clickstream.simlist <- function(xdata,                # first data frame, considered as x
                                ydata,                # second data frame, considered as y
                                # cat1 = "Category1",   # category names, they will be listed in results
                                # cat2 = "Category2",
                                type = "correlation", # type, (cross) "correlation" or "covariance"
                                lag.max = 28,         # maximum lag value to calculate cross-correlation
                                # signif.lags = c(7,14,21,28), # these will be taken
                                digits = 6            # number of decimals for precision
){
  require(dtw);require(stats)
  deinit_x <- which(sapply(xdata,function(x) var(x)==0)) # get the columns with zero variance, for xdata
  deinit_y <- which(sapply(ydata,function(x) var(x)==0)) # get the columns with zero variance, for ydata
  
  Result <- lapply(seq.int(ncol(xdata)),function(x){ # outer loop, for x
    # note that transpose is taken for a better data shape and as.data.frame for use of function "bind_rows"
    as.data.frame(t(sapply(seq.int(ncol(ydata)),function(y){ # inner loop, for y
      # calculate cross-correlation and take only numerical values
      print("I came into core of sim.list") # to delete later
      cat("x =",x,"y =",y,"\n")
      if(any(deinit_x == x) || any(deinit_y == y)){
        temp_prsn <- as.numeric(rep(0,2*lag.max+1))
        temp_spmn <- temp_prsn
        pval.chisq <- -1 # in this case chi sq test is not calculated
        cat("Zero variance encountered at",x,y,"\nCategory 1:",cat1,"\nCategory 2:",cat2,"\n")
      }
      else{ # compute cross-correlations
        # Pearson's product moment coefficient for Cross-Correlation
        temp_prsn <- as.numeric(ccf(xdata[,x],
                                    ydata[,y],
                                    type = type,
                                    lag.max = lag.max,
                                    plot = F)$acf)
        
        # Spearman's rank correlation coefficient for Cross-Correlation
        temp_spmn <- as.numeric(ccf(rank(xdata[,x]),
                                    rank(ydata[,y]),
                                    type = type,
                                    lag.max = lag.max,
                                    plot = F)$acf)
        
        pval.chisq <- chisq.test(xdata[,x],ydata[,y])$p.value # chi squared (MC simulation takes too much time!)
      }
      print("I pass ccf")
      
      print("I combine them")
      # combine with other values such as max, lag at which the cor is max, root-mean-square, and CC values
      c(colnames(xdata)[x], # StateAbb1
        colnames(ydata)[y], # StateAbb2
        pval.chisq,
        dtw(xdata[,x],ydata[,y],distance.only = TRUE)$normalizedDistance, # dynamic time warp
        max(temp_prsn), # MaxCC - pearson
        max(temp_spmn), # MaxCC - spearman
        which.max(temp_prsn)-(lag.max+1), # MaxCCatLag - pearson
        which.max(temp_spmn)-(lag.max+1), # MaxCCatLag - spearman
        rms(temp_prsn), # RmsCC - pearson
        rms(temp_spmn), # RmsCC - spearman
        sum(xdata[,x])/sum(ydata[,y]), # NoSRatio
        temp_prsn, # pearson cross-correlation values
        temp_spmn) # spearman cross-correlation values
    })))
  })
  
  lagnms_prsn <- paste("P",as.character(seq(lag.max*2+1)-lag.max-1),sep = ".") # nomenclature for
  lagnms_spmn <- paste("S",as.character(seq(lag.max*2+1)-lag.max-1),sep = ".") # output data frame
  
  Result <- dplyr::bind_rows(Result) # give the final shape, from list to data frame
  
  print("I bound rows now I'm gonna name them.")
  print(head(Result))
  print(ncol(Result))
  colnames(Result) <- c("Category1","Category2","ChiSq.PVal","DTW.NormDist","Prsn.MaxCC",
                        "Spmn.MaxCC","Prsn.MaxCCatLag","Spmn.MaxCCatLag","Prsn.RmsCC","Spmn.RmsCC","NoSRatio",
                        lagnms_prsn,lagnms_spmn)
  
  print("In cclist: I'm gonna finally convert them to numeric before return.")
  
  # convert to numeric if type list with a function
  Result[,3:(11+(2*lag.max+1)*2)] <- sapply(Result[,3:(11+(2*lag.max+1)*2)],function(x)
    round(as.numeric(x),digits = digits)) # set the column types
  return(Result)
}


## test this function again!
## automatized grid and random search function for hyper parameter search and optimization
clickstream.h2o.gridBuildandtest <- function(data, # the feed dataset
                                             test.row, # number of DAYS to predict starting from the end
                                             valid.row, # number of DAYS to use in validation starting from test
                                             train.row="rest",
                                             response.col=2,
                                             algorithm = "deeplearning",
                                             beyond_data=FALSE, # compare
                                             interpolation.step = NULL,
                                             nthreads = -1,
                                             plot = FALSE,
                                             errorinfo = FALSE,
                                             sort_by = "mse",
                                             hyper_params = list(epochs = c(1,10,100),
                                                                 hidden = list(100,1000,c(200,200),c(1e3,1e3),rep(6,1e3))),
                                             search_criteria = list(strategy = "RandomDiscrete", 
                                                                    max_runtime_secs = 900, 
                                                                    max_models = 200, 
                                                                    stopping_metric = "AUTO", 
                                                                    stopping_tolerance = 0.00001, 
                                                                    stopping_rounds = 5, 
                                                                    seed = 123456),
                                             ...
){
  library(h2o)
  h2o.init(nthreads = nthreads,min_mem_size = "6G")
  h2o.removeAll()
  
  if(!is.null(interpolation.step)){ # apply interpolation
    data <- interpolate(data,by = interpolation.step,months = 5)
    interpolation.factor <- 1/interpolation.step
  }
  else # do not apply interpolation
    interpolation.factor <- 1
  
  lag <- length(which(is.na(data[,response.col])))
  
  # indices to fractionate the data frame
  if(beyond_data){ # cannot be comparable with real data, because this indexing is for predicting beyond the data
    tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row
    print(tes.ind)
    val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)
    print(val.ind)
    if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
      tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind))
    print(tra.ind)
  }
  else{ # can be comparable with real data, prediction is inside the limits
    tes.ind <-  seq(interpolation.factor*test.row)+nrow(data)-interpolation.factor*test.row-lag
    print(tes.ind)
    val.ind <-  seq(interpolation.factor*valid.row)+nrow(data)-interpolation.factor*valid.row-length(tes.ind)-lag
    print(val.ind)
    if(train.row == "rest" || train.row == "Rest" || is.null(train.row))
      tra.ind <- seq(nrow(data)-length(tes.ind)-length(val.ind)-lag)
    print(tra.ind)
  }
  
  # prepare thedata frames
  train <- h2o.assign(as.h2o(data[tra.ind,]),"train.hex")
  valid <- h2o.assign(as.h2o(data[val.ind,]),"valid.hex")
  print(train)
  print(valid)
  
  model <- h2o.grid(algorithm,
                    grid_id = "deep_models",
                    x = seq(ncol(data))[-c(response.col)],
                    y = response.col,
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 0,
                    
                    # distribution="gaussian", ## best for MSE loss, but can try other distributions ("laplace", "quantile")
                    
                    ## stop as soon as mse doesn't improve by more than 0.1% on the validation set,
                    ## for 2 consecutive scoring events
                    stopping_rounds = 2,
                    stopping_tolerance = 1e-3,
                    stopping_metric = "MSE",
                    
                    # score_tree_interval = 100, ## how often to score (affects early stopping)
                    seed = 123456, ## seed to control the sampling of the Cartesian hyper-parameter space
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)
  
  models.sorted <- h2o.getGrid(grid_id = "deep_models", sort_by = sort_by)
  
  test <- h2o.assign(as.h2o(data[tes.ind,]),"test.hex") # convert to h2o
  
  Result <- list()
  
  if(!beyond_data){
    for(i in 1:length(models.sorted@model_ids)){
      current_model <- h2o.getModel(models.sorted@model_ids[[i]]) # get current model
      pred <- h2o.predict(object = current_model,newdata = test) # make predictions
      res <- data.frame(predict = as.data.frame(pred),Real = as.data.frame(test[,response])) # store in a df
      print(res)
      Result[[i]] <- res
      # display error info
      cat("\nRoot-Mean-Squared Error of Predictions :",rms(res$predict-res[,2]))
      cat("\nRoot-Mean-Squared Error of Predictions Per Element:",rms(res$predict-res[,2])/test.row,"or",
          (rms(res$predict-res[,2])/sum(na.omit(res[,2])))*100,"%")
      cat("\nCorrelations of Prediction and Real Values :",cor(res$predict,res[,2]))
    }
  }
  else{
    for(i in 1:length(models.sorted)){
      current_model <- h2o.getModel(models.sorted@model_ids[[i]]) # get current model
      pred <- h2o.predict(object = current_model,newdata = test) # make predictions
      res <- data.frame(predict = as.data.frame(pred),Real = NA) # store in a df
      print(res)
      Result[[i]] <- res
    }
  }
  
  print(length(models.sorted@model_ids))
  return(list(Result=Result,Model=models.sorted,Int=test))
}



#### GET THEM ALL TOGETHER LIKE METHODS OF A CLASS
clickstream <- list(import = clickstream.import,
                    importCurrencies = clickstream.importCurrencies,
                    organize = clickstream.organize,
                    importHourly = clickstream.importHourly,
                    toHourly = clickstream.toHourly,
                    splitDatetime = clickstream.splitDatetime,
                    matchDatetime = clickstream.matchDatetime,
                    align = clickstream.align,
                    h2o.buildandtest = clickstream.h2o.buildandtest,
                    h2o.plotresult = clickstream.h2o.plotresult,
                    h2o.gridBuildandtest = clickstream.h2o.gridBuildandtest,
                    simlist = clickstream.simlist,
                    merge.datetime.mins = merge.datetime.mins,
                    perc.error = perc.error,
                    hidden.toStr = hidden.toStr,
                    split.Datetime = split.Datetime,
                    holidays.2017 = holidays.2017
                    )


