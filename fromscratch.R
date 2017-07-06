### END-TO-END PREDICTIVE MODELLING FROM SCRATCH USING DEEP LEARNING METHODS

## import the personal library
source("https://raw.githubusercontent.com/mcandar/Agents/master/clicklib.R")

library(highcharter)
library(RPostgreSQL)

### ------------------------------------------------------------------- ###
### - OBTAIN DATA FROM DB AND LOCAL MACHINE --------------------------- ###
### ------------------------------------------------------------------- ###

## CONNECT TO DB AND RECEIVE HOURLY PV DISTRIBUTION
drv <- dbDriver("PostgreSQL")
sess <- dbConnect(drv,
                  dbname = "segment",
                  host = "hur-clickstream.c2yosb7rggt4.eu-west-1.redshift.amazonaws.com",
                  port = 5439,
                  user = "mccandar",
                  password = "Mccandar123")

# get the data from db step by step, if any interruption is occured, some of the info could be saved.
## all platforms are included, pc, mobile, and mobile application
page_views <- list() # declare a list to fill in later

# THESE EVALUATIONS ARE MADE FOR bigpara.hurriyet.com.tr
# borsa category
page_views$borsa <- clickstream$toHourly(dbGetQuery(sess,"SELECT CAST(dateadd(h,3,srvr_tm) AS DATE) as [tm_date], -- date
    DATEPART(hour,dateadd(h,3,srvr_tm)) as [tm_hour], -- hour
                                      Count(1) as [PV]  -- counts
                                      FROM data_clickstream
                                      where ev_nm = 'PV'
                                      and path_nm like '%borsa%'
                                      and nws_cgy = 'borsa'
                                      and base_url LIKE '%bigpara.hurriyet.com.tr%' -- desktop, mobile website, application
                                      GROUP BY CAST(dateadd(h,3,srvr_tm) AS DATE), DATEPART(hour,dateadd(h,3,srvr_tm))
                                      ORDER BY 1,2"))

# doviz category
page_views$doviz <- clickstream$toHourly(dbGetQuery(sess,"SELECT CAST(dateadd(h,3,srvr_tm) AS DATE) as [tm_date], -- date
    DATEPART(hour,dateadd(h,3,srvr_tm)) as [tm_hour], -- hour
                                      Count(1) as [PV]  -- counts
                                      FROM data_clickstream
                                      where ev_nm = 'PV'
                                      and path_nm like '%doviz%'
                                      and nws_cgy = 'doviz'
                                      and base_url LIKE '%bigpara.hurriyet.com.tr%' -- desktop, mobile website, application
                                      GROUP BY CAST(dateadd(h,3,srvr_tm) AS DATE), DATEPART(hour,dateadd(h,3,srvr_tm))
                                      ORDER BY 1,2"))

# altin category
page_views$altin = clickstream$toHourly(dbGetQuery(sess,"SELECT CAST(dateadd(h,3,srvr_tm) AS DATE) as [tm_date], -- date
    DATEPART(hour,dateadd(h,3,srvr_tm)) as [tm_hour], -- hour
                                      Count(1) as [PV]  -- counts
                                      FROM data_clickstream
                                      where ev_nm = 'PV'
                                      and path_nm like '%altin%'
                                      and nws_cgy = 'altin'
                                      and base_url LIKE '%bigpara.hurriyet.com.tr%' -- desktop, mobile website, application
                                      GROUP BY CAST(dateadd(h,3,srvr_tm) AS DATE), DATEPART(hour,dateadd(h,3,srvr_tm))
                                      ORDER BY 1,2"))

# analiz category
page_views$analiz = clickstream$toHourly(dbGetQuery(sess,"SELECT CAST(dateadd(h,3,srvr_tm) AS DATE) as [tm_date], -- date
    DATEPART(hour,dateadd(h,3,srvr_tm)) as [tm_hour], -- hour
                                      Count(1) as [PV]  -- counts
                                      FROM data_clickstream
                                      where ev_nm = 'PV'
                                      and path_nm like '%analiz%'
                                      and nws_cgy = 'analiz'
                                      and base_url LIKE '%bigpara.hurriyet.com.tr%' -- desktop, mobile website, application
                                      GROUP BY CAST(dateadd(h,3,srvr_tm) AS DATE), DATEPART(hour,dateadd(h,3,srvr_tm))
                                      ORDER BY 1,2"))

# back up the content since the reproduction time is too long
backup_page_views <- page_views

# check for any unmatching number of rows
print(lapply(page_views,nrow))

## as default, currencies information is not adapted to local time, obtained as GMT
## in case for a local time currency data, try the input parameter "tz = GMT" to import datetime as is.
currencies <- list()
currencies$usdtry <- clickstream$importCurrencies("USDTRY_H1_UTC+0_00_GMT.csv",
                                                  time_interval = page_views$borsa$datetime) # maybe select a smaller time seq to avoid unmathches
currencies$eurusd <- clickstream$importCurrencies("EURUSD_H1_UTC+0_00_GMT.csv",
                                                  time_interval = page_views$borsa$datetime)
currencies$eurtry <- clickstream$importCurrencies("EURTRY_H1_UTC+0_00_GMT.csv",
                                                  time_interval = page_views$borsa$datetime)
currencies$gbpusd <- clickstream$importCurrencies("GBPUSD_H1_UTC+0_00_GMT.csv",
                                                  time_interval = page_views$borsa$datetime)

### ------------------------------------------------------------------- ###
### - REARRANGE AN ALIGN THE DATASET TO PREPARE FOR DEEP LEARNING ----- ###
### - APPLICATIONS ---------------------------------------------------- ###
### ------------------------------------------------------------------- ###

## the shortest and the most common time interval which all data frames have a value
time_seq <- currencies[[which.min(lapply(currencies,nrow))]]$datetime
print(time_seq)

## get PV's together in specified dates and times
page_views$matched <- clickstream$matchDatetime(datalist = page_views,
                                                time_interval = time_seq,
                                                column_datetime = 1,
                                                specify_columns_toinclude = 2)

## get major currencies' info together in specified dates and times
currencies$matched <- clickstream$matchDatetime(datalist = currencies,
                                                time_interval = time_seq,
                                                column_datetime = 1,
                                                specify_columns_toinclude = 6)


current_lag <- 24 # this is the number of units to predict! it also shifts the response column backwards with its value

# bind together 
df <- list()
df$original <- cbind(page_views$matched,currencies$matched[,-1])

# just split datetime in original
df$splitted <- data.frame(df$original[,-1],
                          clickstream$split.Datetime(df$original$datetime),
                          isHoliday=clickstream$holidays.2017(df$original,1))

# align and shift response column
df$aligned <- clickstream$align(data = df$original,response.col = 2,lag = current_lag)

# split POSIXct datetime and add holidays
df$alignedSplitted <- data.frame(df$aligned[,-1],
                                 clickstream$split.Datetime(df$aligned$datetime),
                                 isHoliday=clickstream$holidays.2017(df$aligned,1))


ID <- sample.int(1e5,1) # create an ID for current session
cat("The ID for the models generated in current session is",ID,"\n")

# create a directory to export and store files in a tidy way
initialwd <- getwd()
tempwd <- paste("modeloutputs",ID,today(),sep = "_")
dir.create(tempwd)
setwd(tempwd)



# export to csv
lapply(seq_along(df),function(i){
  temp_filename <- paste("sourcedata_",names(df)[i],"_",today(),".csv",sep = "")
  write.csv(df[[i]],temp_filename,row.names = FALSE)
  paste("File",temp_filename,"is saved to",getwd())
  })


# REARRANGE THIS! adjust this plot to current training data frame
vols_pv <- highchart(type = "stock") %>%
  hc_title(text = "Borsa Category Page View Counts with Major Currencies' Intraday Tick Volume") %>%
  hc_subtitle(text = "A basic comparison with Borsa Category PV counts of bigpara.hurriyet.com.tr
              with currency volumes, excluding weekends.") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=df$original$borsa_PV,
                             id = "Borsa Page Views",name = "Borsa Page Views",color = rgb(1,0,0)) %>%
  hc_add_series_times_values(dates = df$original$datetime,values=df$original$doviz_PV,
                             id = "Doviz Page Views",name = "Doviz Page Views") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=df$original$altin_PV,
                             id = "Altin Page Views",name = "Altin Page Views") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=df$original$analiz_PV,
                             id = "Analiz Page Views",name = "Analiz Page Views") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=date_limits$Vol_EURUSD,
                             id = "EURUSD Tick Volume",name = "EURUSD Tick Volume") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=date_limits$Vol_EURTRY,
                             id = "EURTRY Tick Volume",name = "EURTRY Tick Volume") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=date_limits$Vol_GBPUSD,
                             id = "GBPUSD Tick Volume",name = "GBPUSD Tick Volume") %>%
  hc_add_series_times_values(dates = df$original$datetime,values=date_limits$Vol_USDTRY,
                             id = "USDTRY Tick Volume",name = "USDTRY Tick Volume")

vols_pv



htmlwidgets::saveWidget(vols_pv,paste(colnames(df$alignedSplitted)[response],"vs_currency_volumes.html",sep = "_"))
webshot::webshot(url = paste(colnames(df$alignedSplitted)[response],"vs_currency_volumes.html",sep = "_"),
                 file = paste(colnames(df$alignedSplitted)[response],"vs_currency_volumes.png",sep = "_"),vwidth = 1200,delay = 3)




## COMPLETED UP TO HERE, REARRANGE AND IMPROVE BELOW
### ------------------------------------------------------------------- ###
### - BUILD PREDICTIVE MODELS WITH MACHINE LEARNING METHODS ----------- ###
### ------------------------------------------------------------------- ###

head(df$alignedSplitted) # the final shape of data
response <- 1

### DO NOT FORGET TO TUNE OTHER PARAMETERS, GIVEN BELOW ARE JUST A BASIC APPROACH ###
########### inside the data ############ ########### inside the data ############ ########### inside the data ############
## ANN
pred_grid_ann <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                  test.row = current_lag,
                                                  valid.row = 100,
                                                  response.col = response,
                                                  algorithm = "deeplearning",
                                                  hyper_params = list(epochs = c(1,10,20),
                                                                      hidden = list(100,1000,c(200,200)),
                                                                      activation = c("Rectifier","Tanh","Maxout")))


# pred_grid_ann$Specs
pred_canv_ann <- clickstream.h2o.plotGrid(pred_grid_ann,
                                          plot_output_prefix = "TEST_FUN_",
                                          title_prefix = "Test Prediction",
                                          subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                          datetime.cols = 9:13)


## run an accurate model among others
pred <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     epochs = 1,
                                     hidden = rep(10,2)
)

print(pred$Result)

p <- clickstream.h2o.plotresult(data_aslist = pred,title = "Lorem Ipsum")

p

htmlwidgets::saveWidget(p,"test_graph_dl.html")



## RANDOM FOREST
pred_grid_rf <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                 test.row = current_lag,
                                                 valid.row = 100,
                                                 response.col = response,
                                                 algorithm = "randomForest",
                                                 hyper_params = list( ntrees = c(1e2,5e2,1e3,5e3),
                                                                      # max_depth = c(1,5,10,15,20),
                                                                      # min_rows = c(1,5,10,20,50,100),
                                                                      # nbins = c(5,10,20,50,100),
                                                                      sample_rate = seq(0.3,1,0.05)))



pred_canv_rf <- clickstream.h2o.plotGrid(pred_grid_rf,
                                         plot_output_prefix = "TEST_FUN_",
                                         title_prefix = "Test Prediction",
                                         subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                         datetime.cols = 9:13)


## run an accurate model among others
pred <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     algorithm = "randomForest"
                                     # nthreads = 2,
                                     # epochs = 1e3,
                                     # hidden = rep(1e3,6)
)

print(pred$Result)

p <- clickstream.h2o.plotresult(data_aslist = pred,
                                title = "lorem ipsum drf predictions!!"
                                # dates = df$aligned$datetime[which(is.na(df$aligned$borsa_PV))]-(current_lag*3600)
                                )

p

htmlwidgets::saveWidget(p,"test_graph_rf.html")



## GRADIENT BOOST MACHINE
pred_grid_gbm <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                  test.row = current_lag,
                                                  valid.row = 100,
                                                  response.col = response,
                                                  algorithm = "gbm",
                                                  hyper_params = list(ntrees = c(100,1e3),
                                                                      # max_depth = max_depth_opts, ### check this
                                                                      min_rows = c(1,5,10,20,50,100),
                                                                      # learn_rate = seq(0.001,0.01,0.001),
                                                                      sample_rate = seq(0.3,1,0.05)
                                                                      # col_sample_rate = seq(0.3,1,0.05)
                                                                      # col_sample_rate_per_tree = seq(0.3,1,0.05)
                                                  )
)



pred_canv_gbm <- clickstream.h2o.plotGrid(pred_grid_gbm,
                                          plot_output_prefix = "TEST_FUN_",
                                          title_prefix = "Test Prediction",
                                          subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                          datetime.cols = 9:13)


## run an accurate model among others
pred <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     algorithm = "gbm"
                                     # nthreads = 2,
                                     # epochs = 1,
                                     # hidden = rep(10,2)
)

print(pred$Result)

p <- clickstream.h2o.plotresult(data_aslist = pred,title = "Lorem Ipsum"
                                # dates = df$aligned$datetime[which(is.na(df$aligned$borsa_PV))]-(current_lag*3600)
)

p

htmlwidgets::saveWidget(p,"test_graph_gbm.html")



#### get all together right here three columns
lapply(list(pred_canv_ann$plots,pred_canv_rf$plots,pred_canv_gbm$plots),length)
all_plots <- list()
n <- 1
for(i in seq.int(0,30,by = 3)+1){
  print(n)
  if(n > 10)
    break
  all_plots[[i]] <- pred_canv_ann$plots[[n]]
  all_plots[[i+1]] <- pred_canv_rf$plots[[n]]
  all_plots[[i+2]] <- pred_canv_gbm$plots[[n]]
  n <- n + 1
}

canv <- hw_grid(all_plots,ncol = 3,rowheight = 400) # make a canvas of plots

htmltools::save_html(canv,paste("TEST_hyper_params_top10_forex_COMBINED_",ID,".html",sep = ""))
webshot::webshot(url = paste("TEST_hyper_params_top10_forex_COMBINED_",ID,".html",sep = ""),
                 file = paste("TEST_hyper_params_top10_forex_COMBINED_",ID,".png",sep = ""),vwidth = 1200)
cat("The canvas ","TEST_hyper_params_top10_forex_COMBINED_",ID," is saved to ",getwd(),"\n",sep = "")




########### beyond the data ############ ########### beyond the data ############ ########### beyond the data ############ 

## ANN
pred_grid_ann_B <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                  test.row = current_lag,
                                                  valid.row = 100,
                                                  response.col = response,
                                                  algorithm = "deeplearning",
                                                  beyond_data = TRUE,
                                                  hyper_params = list(epochs = c(1,10,20),
                                                                      # epochs = c(100,1e3), # on linux remote server
                                                                      hidden = list(100,c(20,20),c(32,32,32,32)),
                                                                      # hidden = list(100,1000,c(200,200),rep(128,6),c(1e3,1e3),c(2e3,2e3),rep(6,1e3)),
                                                                      activation = c("Rectifier","Tanh","Maxout")))

length(pred_grid_ann_B$Model)
# pred_grid_ann$Specs
pred_canv_ann_B <- clickstream.h2o.plotGrid(pred_grid_ann_B,
                                            plot_output_prefix = "TEST_FUN_beyonddata_",
                                            title_prefix = "Test Prediction",
                                            subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                            datetime.cols = 9:13)



## run an accurate model among others
pred_ <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     beyond_data = TRUE,
                                     # nthreads = 2,
                                     epochs = 1,
                                     hidden = rep(10,2)
)

print(pred$Result)

p <- clickstream.h2o.plotresult(data_aslist = pred,title = "Lorem Ipsum"
                                # dates = df$aligned$datetime[which(is.na(df$aligned$borsa_PV))]-(current_lag*3600)
)

p

htmlwidgets::saveWidget(p,"test_graph_dl_beyond.html")


## RANDOM FOREST
pred_grid_rf_B <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                   test.row = current_lag,
                                                   valid.row = 100,
                                                   response.col = response,
                                                   beyond_data = TRUE,
                                                   algorithm = "randomForest",
                                                   hyper_params = list( ntrees = c(1e2,5e2,1e3,5e3),
                                                                        # max_depth = c(1,5,10,15,20),
                                                                        # min_rows = c(1,5,10,20,50,100),
                                                                        # nbins = c(5,10,20,50,100),
                                                                        sample_rate = seq(0.3,1,0.05)))



pred_canv_rf_B <- clickstream.h2o.plotGrid(pred_grid_rf_B,
                                           plot_output_prefix = "TEST_FUN_beyonddata_",
                                           title_prefix = "Test Prediction",
                                           subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                           datetime.cols = 9:13)


## run an accurate model among others
pred <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     beyond_data = TRUE,
                                     algorithm = "randomForest"
                                     # nthreads = 2,
)

p <- clickstream.h2o.plotresult(data_aslist = pred,
                                title = "lorem ipsum drf predictions!!"
)

p

htmlwidgets::saveWidget(p,"test_graph_rf_beyond.html")



## GRADIENT BOOST MACHINE
pred_grid_gbm_B <- clickstream.h2o.gridBuildandtest(data = df$alignedSplitted,
                                                    test.row = current_lag,
                                                    valid.row = 100,
                                                    response.col = response,
                                                    algorithm = "gbm",
                                                    beyond_data = TRUE,
                                                    hyper_params = list(ntrees = c(100,1e3),
                                                                        # max_depth = max_depth_opts, ### check this
                                                                        min_rows = c(1,5,10,20,50,100),
                                                                        # learn_rate = seq(0.001,0.01,0.001),
                                                                        sample_rate = seq(0.3,1,0.05)
                                                                        # col_sample_rate = seq(0.3,1,0.05)
                                                                        # col_sample_rate_per_tree = seq(0.3,1,0.05)
                                                    )
)



pred_canv_gbm_B <- clickstream.h2o.plotGrid(pred_grid_gbm_B,
                                            plot_output_prefix = "TEST_FUN_beyonddata_",
                                            title_prefix = "Test Prediction",
                                            subtitle_prefix = "Test of the clickstream.h2o.plotGrid function",
                                            datetime.cols = 9:13)


## run an accurate model among others
pred <- clickstream.h2o.buildandtest(df$alignedSplitted,
                                     test.row = current_lag,
                                     valid.row = 100,
                                     response.col = response,
                                     algorithm = "gbm",
                                     beyond_data = TRUE
                                     # nthreads = 2,
                                     # epochs = 1,
                                     # hidden = rep(10,2)
)

print(pred$Result)

p <- clickstream.h2o.plotresult(data_aslist = pred,title = "Lorem Ipsum"
                                # dates = df$aligned$datetime[which(is.na(df$aligned$borsa_PV))]-(current_lag*3600)
)

p

htmlwidgets::saveWidget(p,"test_graph_gbm_beyond.html")



#### get all together right here three columns
lapply(list(pred_canv_ann_B$plots,pred_canv_rf_B$plots,pred_canv_gbm_B$plots),length)
all_plots <- list()
n <- 1
for(i in seq.int(0,30,by = 3)+1){
  print(n)
  if(n > 10)
    break
  all_plots[[i]] <- pred_canv_ann_B$plots[[n]]
  all_plots[[i+1]] <- pred_canv_rf_B$plots[[n]]
  all_plots[[i+2]] <- pred_canv_gbm_B$plots[[n]]
  n <- n + 1
}

canv <- hw_grid(all_plots,ncol = 3,rowheight = 400) # make a canvas of plots

htmltools::save_html(canv,paste("TEST_beyonddata_hyper_params_top10_forex_COMBINED_",ID,".html",sep = ""))
webshot::webshot(url = paste("TEST_beyonddata_hyper_params_top10_forex_COMBINED_",ID,".html",sep = ""),
                 file = paste("TEST_beyonddata_hyper_params_top10_forex_COMBINED_",ID,".png",sep = ""),vwidth = 1200)
cat("The canvas ","TEST_beyonddata_hyper_params_top10_forex_COMBINED_",ID," is saved to ",getwd(),"\n",sep = "")


######## last detailed graphs, stack, confidence interval, elementwise averages, optional: loess smoother, spline interpolation


stack_plots <- list(ann=clickstream.h2o.plotGridstack(pred_grid_ann,"test_stack","test_stack","test_stack",datetime.cols = 9:13),
                    rf=clickstream.h2o.plotGridstack(pred_grid_rf,"test_stack","test_stack","test_stack",datetime.cols = 9:13),
                    gbm=clickstream.h2o.plotGridstack(pred_grid_gbm,"test_stack","test_stack","test_stack",datetime.cols = 9:13))

stack_plots <- list(stack_plots$ann$stack,stack_plots$ann$interval,stack_plots$rf$stack,stack_plots$rf$interval,stack_plots$gbm$stack,stack_plots$gbm$interval)

canv <- hw_grid(stack_plots,ncol = 2,rowheight = 400) # make a canvas of plots

htmltools::save_html(canv,paste("TEST_stack_top10_forex_COMBINED_",ID,".html",sep = ""))
webshot::webshot(url = paste("TEST_stack_top10_forex_COMBINED_",ID,".html",sep = ""),
                 file = paste("TEST_stack_top10_forex_COMBINED_",ID,".png",sep = ""),vwidth = 1200)
cat("The canvas ","TEST_stack_top10_forex_COMBINED_",ID," is saved to ",getwd(),"\n",sep = "")



### stack and area plots as a summary
stack_plots_B <- list(ann=clickstream.h2o.plotGridstack(pred_grid_ann_B,"test_stack","test_stack","test_stack",datetime.cols = 9:13),
                    rf=clickstream.h2o.plotGridstack(pred_grid_rf_B,"test_stack","test_stack","test_stack",datetime.cols = 9:13),
                    gbm=clickstream.h2o.plotGridstack(pred_grid_gbm_B,"test_stack","test_stack","test_stack",datetime.cols = 9:13))

stack_plots_B <- list(stack_plots_B$ann$stack,stack_plots_B$ann$interval,stack_plots_B$rf$stack,stack_plots_B$rf$interval,stack_plots_B$gbm$stack,stack_plots_B$gbm$interval)

canv <- hw_grid(stack_plots_B,ncol = 2,rowheight = 400) # make a canvas of plots

htmltools::save_html(canv,paste("TEST_beyonddata_stack_top10_forex_COMBINED_",ID,".html",sep = ""))
webshot::webshot(url = paste("TEST_beyonddata_stack_top10_forex_COMBINED_",ID,".html",sep = ""),
                 file = paste("TEST_beyonddata_stack_top10_forex_COMBINED_",ID,".png",sep = ""),vwidth = 1200)
cat("The canvas ","TEST_beyonddata_stack_top10_forex_COMBINED_",ID," is saved to ",getwd(),"\n",sep = "")

setwd(dir = initialwd) # set initial directory as working directory
h2o.shutdown() # ask the user to shutdown

