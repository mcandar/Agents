# # Following function is INCOMPLETE and NOT working. Should be reorganized.
# MultiPlotly3d <- function(data,
#                           x,
#                           y,
#                           z,
#                           color,
#                           fname = "3d",
#                           main = "3dPlot",
#                           type = "scatter3d",
#                           mode = "markers"){
# 
#   require(plotly)
#   if(is.null(colnames(x)))
#     x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
#   if(is.null(colnames(y)))
#     y <- as.data.frame(y) # convert to data frame if not already, (x maybe a multicolumn object too)
# 
#   for(i in 1:ncol(z)){
#   filename <- paste(fname,"_",i,"_",
#                     colnames(x[1]),"_",
#                     colnames(y[1]),"_",
#                     colnames(z[i]),".html",sep = "") # form a file name with an index
# 
#   plotname <- paste(main," #",i,sep = "")
# 
#   p <- plot_ly(data, x = x, y = y, z = z[,i],
#                color = t,
#                type = "scatter3d",
#                mode = "markers") %>%
#     layout(title = plotname,
#            scene = list(
#              xaxis = list(title = colnames(x[1])),
#              yaxis = list(title = colnames(y[1])),
#              zaxis = list(title = colnames(z[i]))))
#   htmlwidgets::saveWidget(as.widget(p), filename) # save as an html page to keep interactive tools
#   cat("Image",filename,"saved to",getwd(),"\n")
#   }
#   return(TRUE)
# }
# 
# # MultiPlotly3d(month10,x = SONumber,y = SenderZip,z = ReceipentZip,color = ShippingDate,fname = "3DIM")
# MultiPlotly3d(month10,x = month10[,1],y = month10[,3],z = month10[,c(4,6)],color = month10[,2],fname = "3DIM")


# color added, but beta version. does not work properly. color legend should display which variable it depends on
MultiGGPlot <- function(x,                    # data of x axis
                        data,                 # data set of y axis
                        main = "GGPlot",      # main title
                        fname = "GG_",        # prefix for filename
                        geom = "jitter",      # geometry of the plot
                        colour = "black",
                        alpha = I(1/10),      # transparecy of the points
                        smooth = FALSE,       # de/activate smoothing
                        smethod = "lm",       # smoothing method
                        formula=y ~ poly(x, 3, raw=TRUE), # formula for smoothing, linear, polynomial etc.
                        width = 6,            # width as inches
                        height = 3.375){      # height as inches
  
  require(ggplot2)
  if(is.null(colnames(x))) 
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(data)){
      if (identical(x[,j],data[,i])) next()
      filename <- paste(fname,"_",i+((j-1)*ncol(data)),"_",colnames(data[i]),"_vs_",colnames(x[j]),".png",sep = "") # form a file name with an index
      plotname <- paste(main," #",i,sep = "")
      ggplot(data, 
             aes(x = x[,j],
                 y = data[,i],
                 colour = colour)) +
        
        # if(!is.null(colour))
          scale_colour_gradientn(colours=rainbow(4)) +
      
        switch (geom,
                "jitter" = {geom_jitter(alpha = alpha)},
                "point" = {geom_point(alpha = alpha)},
                "path" = {geom_path(alpha = alpha)}
        ) +
        
        labs(title = main,
             x = colnames(x[j]),
             y = colnames(data[i])) +
        
        if(smooth)
          geom_smooth(method=smethod, se=TRUE, fill=NA,
                      formula=formula,colour="blue")
      
      ggsave(filename,device = "png",width = width,height = height)
      cat("Image",filename,"saved to",getwd(),"\n")
    }
  }
  return (TRUE)
}

MultiGGPlot(month10[,2],month10[,c(3,4)],fname = "trial",colour = month10$SONumber,alpha = I(1/5))




#######################################################################################
# Convert classes of objects without loss of information
Convert <- function(data,col,class="numeric",split=TRUE,na.rm=FALSE,limupper=0){ # unfactors and extracts the content out of it as numeric
  Result <- data
  for(i in col){ # split XXXXX-XXXX type of zip data, take the left side
    if(split){
      m <- strsplit(as.character(Result[,i]),"-") # separate them by "-"
      n <- as.data.frame(matrix(lapply(m, "[",1))) # take the left side
      Result[,i] <- as.data.frame(trimws(n[[1]])) # trim white spaces
    }
    switch (class,
            "numeric" = {
              Result[,i] <- as.numeric(as.character(Result[,i])) # originally denoted as Result$`Receipent Zip`
            },
            "character" = {
              Result[,i] <- as.character(Result[,i])
            }
    )
  }
  if(na.rm){ # remove NA rows and calculate the percentage
    oldr <- nrow(Result)
    Result <- na.omit(Result)
    if(class == "numeric"){
      for(i in col){ # remove the incorrect zips, zip codes cannot be equal or greater than 10^5
        if(limupper != 0 && any(Result[,i] > limupper))
          Result <- Result[-which(Result[,i] > limupper),]
      }
    }
    newr <- nrow(Result)
    cat(((oldr-newr)/oldr)*100,"% of the rows are removed.\n",sep = "")
  }
  return(Result)
}


#######################################################################################
#######################################################################################

# convert classes with minimal possible loss of information
Convert <- function(data,col,class="numeric",split=FALSE,na.rm=FALSE,limupper=0){ # unfactors and extracts the content out of it as numeric
  Result <- data
  for(i in col){ # split XXXXX-XXXX type of zip data, take the left side
    if(split){
      m <- strsplit(as.character(Result[,i]),"-") # separate them by "-"
      n <- as.data.frame(matrix(lapply(m, "[",1))) # take the left side
      Result[,i] <- as.data.frame(trimws(n[[1]])) # trim white spaces
    }
    else
      Result[,i] <- as.data.frame(trimws(Raw[,i])) # trim white spaces without splitting
    
    switch (class,
            "numeric" = {
              Result[,i] <- as.numeric(as.character(Result[,i])) # originally denoted as Result$`Receipent Zip`
            },
            "character" = {
              Result[,i] <- as.character(Result[,i])
            }
    )
  }
  if(na.rm){ # remove NA rows and calculate the percentage
    oldr <- nrow(Result)
    Result <- na.omit(Result)
    if(class == "numeric"){
      for(i in col){ # remove the incorrect zips, zip codes cannot be equal or greater than 10^5
        if(limupper != 0 && any(Result[,i] > limupper))
          Result <- Result[-which(Result[,i] > limupper),]
      }
    }
    newr <- nrow(Result)
    cat(((oldr-newr)/oldr)*100,"% of the rows are removed.\n",sep = "")
  }
  return(Result)
}

#######################################################################################
#######################################################################################

# Raw <- read.table("ShippingData_Months_10to12.txt",sep=",",colClasses = "character")
Raw <- read.table("Month10.txt",sep=",",colClasses = "character")
# Raw <- Convert(Raw,1,class="character",split = FALSE)
# Raw <- Format.SaleData(Raw)
Raw$Total <- Raw$UnitsShipped*Raw$AverageUnitPrice
Ship <- read.csv("ShippingSample1012.csv",colClasses = "character")
# Ship <- Format.ShippingData(Ship)
# Ship <- read.table("ShippingData_Months_10to12.txt",sep=",",colClasses = "character")

# match the rows of the same SONumber
n <- 1
# rows <- nrow(Raw)
rows <- 10^5
Unified <- as.data.frame(matrix(NA,1,ncol(Raw)+ncol(Ship)))
for(i in 1:rows){
  m <- match(Raw[i,1],Ship[,8])
  # print(m)
  if(!is.na(m)){
    Unified[n,] <- cbind(Raw[i,],Ship[m,])
    n <- n + 1
  }
}

colnames(Unified) <- c("SONumber","ShippingDate","SenderZip","ReceipentZip","ItemDescription","ItemWeight",
                       "UnitsShipped","AverageUnitPrice","TrackingNumber","Company","ShippingCode","SenderZip",
                       "ReceipentZip","Type","ShippingCost","SONumber","DateShipped","DateDelivered","Duration")

# write.csv(Unified,"Unified_M10_S1012.txt",row.names = FALSE)

### --- ###
solvl <- levels(factor(Raw[,1]))
system.time({
n <- 1
rows <- length(solvl)
MUnified <- as.data.frame(matrix(NA,1,ncol(Raw)+ncol(Ship)))
for(i in 1:rows){
  m <- which(solvl[i]==Ship[,8])
  temp <- length(m)
  if(temp > 0){
    for(j in 1:temp){
      MUnified[n,] <- cbind(Raw[i,],Ship[m[j],])
      n <- n + 1 
    }
  }
}
})

colnames(MUnified) <- c("SONumber","ShippingDate","SenderZip","ReceipentZip","ItemDescription","ItemWeight",
                       "UnitsShipped","AverageUnitPrice","TrackingNumber","Company","ShippingCode","SenderZip",
                       "ReceipentZip","Type","ShippingCost","SONumber","DateShipped","DateDelivered","Duration")
