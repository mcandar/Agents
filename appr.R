### ----------------------------------------------------------------------- ###
### - Contains function recipes for analyzing 6 months data of newegg.com - ###
### - Author     : Mert Candar -------------------------------------------- ###
### - Study      : Predictive Modelling with Machine Learning ------------- ###
### - Class      : Advanced Physics Project Lab --------------------------- ###
### - Supervisor : Altan Cakir -------------------------------------------- ###
### - Department of Physics Engineering, Istanbul Technical University ---- ###
### - Istanbul, Turkey ---------------------------------------------------- ###
### ----------------------------------------------------------------------- ###

Name <- function(data,type = "sale.data"){
  switch (type,
    "sale.data" = {
      colnames(data) <- c("SONumber","ShippingDate","SenderZip","ReceipentZip","ItemDescription","ItemWeight",
                          "UnitsShipped","AverageUnitPrice")
    },
    "shipping.data" = {
      colnames(data) <- c("TrackingNumber","Company","ShippingCode","SenderZip","ReceipentZip","Type",
                          "ItemWeight","SONumber","DateShipped","DateDelivered","Duration")
    }
  )
  return(data)
}

# Import time series into R
GetTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

# Convert classes of objects without loss of information
Convert <- function(data,col,class="numeric",na.rm=FALSE,limupper = 10^20){ # unfactors and extracts the content out of it as numeric
  Result <- data
  for(i in col){ # split XXXXX-XXXX type of zip data, take the right-hand side
    m <- strsplit(as.character(Result[,i]),"-")
    n <- as.data.frame(matrix(lapply(m, "[",1)))
    Result[,i] <- as.data.frame(trimws(n[[1]]))
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
    for(i in col){ # remove the incorrect zips, zip codes cannot be equal or greater than 10^5
      if(any(Result[,i] > limupper))
        Result <- Result[-which(Result[,i] > limupper),]
    }
    newr <- nrow(Result)
    cat(((oldr-newr)/oldr)*100,"% of the rows are removed.\n",sep = "")
  }
  return(Result)
}

# Draw multiple higher quality graphs at one time with ggplot2, 
MultiPlot <- function(x,                    # data of x axis
                      data,                 # data set of y axis
                      xlab = NULL,          # x label
                      main = "QuickPlot",   # main title
                      fname = "GG_",        # prefix for filename
                      geom = "jitter",      # geometry of the plot
                      alpha = I(1/10),      # transparecy of the points
                      width = 8,            # width as inches
                      height = 4.5){        # height as inches
  
  require(ggplot2)
  data <- as.data.frame(data)
  for(i in 1:ncol(data)){
    filename <- paste(fname,"_",i,"_",colnames(data[i]),"_vs_",xlab,".png",sep = "") # form a file name with an index
    plotname <- paste(main," #",i,sep = "")
    ggplot(data, 
           aes(x = x,
               y = data[,i])) +
      switch (geom,
              "jitter" = {geom_jitter(alpha = alpha)},
              "point" = {geom_point(alpha = alpha)},
              "path" = {geom_path(alpha = alpha)}
      ) +
      
      labs(title = main,
           x = xlab,
           y = colnames(data[i])) +
      
      ggsave(filename,device = "png",width = width,height = height)
    cat("Image",filename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}

# Zips <- read.table("https://raw.githubusercontent.com/mcandar/
#                    Agents/master/US_Postal_Codes_Merged.txt",colClasses = "character")
