### ----------------------------------------------------------------------- ###
### - Contains function recipes for analyzing 6 months data of newegg.com - ###
### - Author     : Mert Candar -------------------------------------------- ###
### - Study      : Predictive Modelling with Machine Learning ------------- ###
### - Class      : Advanced Physics Project Lab --------------------------- ###
### - Supervisor : Altan Cakir -------------------------------------------- ###
### - Department of Physics Engineering, Istanbul Technical University ---- ###
### - Istanbul, Turkey ---------------------------------------------------- ###
### ----------------------------------------------------------------------- ###

DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

Convert <- function(data,col,na.rm=FALSE){ # unfactors and extracts the content out of it as numeric
  Result <- data
  for(i in col){ # split XXXXX-XXXX type of zip data, take the right-hand side
    m <- strsplit(as.character(Result[,i]),"-")
    n <- as.data.frame(matrix(lapply(m, "[",1)))
    Result[,i] <- as.data.frame(n) # originally as.data.frame(trimws(n[[1]]))
    Result[,i] <- as.numeric(as.character(Result[,i])) # originally Result$`Receipent Zip` instead of Result[,5]
  }
  if(na.rm){ # remove NA rows and calculate the percentage
    oldr <- nrow(Result)
    Result <- na.omit(Result)
    for(i in col){ # remove the incorrect zips, zip codes cannot be equal or greater than 10^5
      if(any(Result[,i] > 10^5))
        Result <- Result[-which(Result[,i] > 10^5),]
    }
    newr <- nrow(Result)
    cat(((oldr-newr)/oldr)*100,"% of the rows are removed.\n",sep = "")
  }
  return(Result)
}

MultiQPlot <- function(x,                    # data of x axis
                       data,                 # data set of y axis
                       labelx = "Index",     # x label
                       main = "QuickPlot",   # main title
                       fname = "GG_",        # prefix for filename
                       geom = "jitter",      # geometry of the plot
                       alpha = I(1/10),      # transparecy of the points
                       width = 8,            # width as inches
                       height = 4.5){        # height as inches
  
  require(ggplot2)
  data <- as.data.frame(data)
  for(i in 1:ncol(data)){
    filename <- paste(fname,"_",i,"_",colnames(data[i]),"_vs_",labelx,".png",sep = "") # form a file name with an index
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
           x = labelx,
           y = colnames(data[i])) +
      
      ggsave(filename,device = "png",width = width,height = height)
    cat("Image",filename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}
