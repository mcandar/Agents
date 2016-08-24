### ----------------------------------------------------------------------- ###
### - Contains function recipes for analyzing 6 months data of newegg.com - ###
### - Author     : Mert Candar -------------------------------------------- ###
### - Study      : Predictive Modelling with Machine Learning ------------- ###
### - Class      : Advanced Physics Project Lab --------------------------- ###
### - Supervisor : Altan Cakir -------------------------------------------- ###
### - Department of Physics Engineering, Istanbul Technical University ---- ###
### - Istanbul, Turkey ---------------------------------------------------- ###
### ----------------------------------------------------------------------- ###

GetZips <- function(){
  Zips <- read.table("https://raw.githubusercontent.com/mcandar/
                     Agents/master/US_Postal_Codes_Merged.txt",colClasses = "character")
  return(Zips)
}

# Row-wise sorting, ascending order
Sort <- function(data,col){
  return(data[order(data[,col]),])
}

SetColNames <- function(data,type = "sale.data"){
  switch (type,
    "sale.data" = {
      colnames(data) <- c("SONumber","ShippingDate","SenderZip","ReceipentZip","ItemDescription","ItemWeight",
                          "UnitsShipped","AverageUnitPrice")
    },
    "shipping.data" = {
      colnames(data) <- c("TrackingNumber","Company","ShippingCode","SenderZip","ReceipentZip","Type",
                          "Item Weight","SONumber","DateShipped","DateDelivered","Duration")
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
    if(class == "numeric"){
      for(i in col){ # remove the incorrect zips, zip codes cannot be equal or greater than 10^5
        if(any(Result[,i] > limupper))
          Result <- Result[-which(Result[,i] > limupper),]
      }
    }
    newr <- nrow(Result)
    cat(((oldr-newr)/oldr)*100,"% of the rows are removed.\n",sep = "")
  }
  return(Result)
}

# Draw quick graphs with base plotting function
MultiPlot <- function(x,                    # data of x axis
                      data,                 # data set of y axis
                      xlab = "Index",       # x label
                      main = "Plot",        # main title
                      fname = "Plot_",      # prefix for filename
                      method = "plot",      # two options, for ono-to-one: "plot", one-to-many: "matplot"                      
                      width = 1280,         # width as pixels
                      height = 720){        # height as pixels
  
  for(i in 1:ncol(data)){
    filename <- paste(fname,"_",i,"_",colnames(data[i]),"_vs_",xlab,".png",sep = "") #paste(name,i,".png",sep = "")
    png(filename,width = width,height = height)
    switch (method, # switch for different plot options
            "plot" = { # plot one column versus another one
              plotname <- paste(main," #",i,sep = "")
              plot(x,
                   data[,i],
                   xlab = xlab,
                   ylab = colnames(data[i]),
                   main = plotname,
                   cex.lab = 1.5)
            },
            "matplot" = { # plot one column versus rest of the columns
              plotname <- paste(main,"#",i,sep = "")
              matplot(data[,i],
                      data[,-c(i)], # exclude x axis object
                      xlab = colnames(data[i]),
                      ylab = colnames(data[-c(i)]), 
                      main = plotname,col = 1:(ncol(data)-1),
                      pch=1:(ncol(data)-1))
              legend("topleft",legend = colnames(data[-c(i)]), 
                     col=1:(ncol(data)-1),pch=1:(ncol(data)-1))
            }
    )
    grid()
    dev.off()
    cat("Image",filename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}

# Draw multiple higher quality graphs at one time with ggplot2, 
MultiGGPlot <- function(x,                    # data of x axis
                        data,                 # data set of y axis
                        main = "GGPlot",      # main title
                        fname = "GG_",        # prefix for filename
                        geom = "jitter",      # geometry of the plot
                        alpha = I(1/10),      # transparecy of the points
                        smooth = FALSE,       # de/activate smoothing
                        smethod = "lm",       # smoothing method
                        formula=y ~ poly(x, 3, raw=TRUE), # formula for smoothing, linear, polynomial etc.
                        width = 8,            # width as inches
                        height = 4.5){        # height as inches
  
  require(ggplot2)
  if(is.null(colnames(x))) 
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(data)){
      filename <- paste(fname,"_",i,"_",colnames(data[i]),"_vs_",colnames(x[j]),".png",sep = "") # form a file name with an index
      plotname <- paste(main," #",i,sep = "")
      ggplot(data, 
             aes(x = x[,j],
                 y = data[,i])) +
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

# Calculate the correlations and plot them in a table (content of the function is properly 
# working outside the function, but function itself does not plot and save the graph.)
CorLevels <- function(data,filename="Correlations",main = "Title"){
  Result <- cor(data) # calculate correlation coefficients
  Original <- Result
  n <- 0 # initialize a variable for counting
  while(range(abs(Result))[1]<0.1){ # if absolute value of the smallest value if less than 0.1
    for(i in 1:nrow(Result)){         # take square roots of the data to rescale, until it can be
      for(j in 1:nrow(Result)){       # easily observable
        if(Result[i,j] < 0)
          Result[i,j] <- (-1)*sqrt(abs(Result[i,j])) # sqrt the abs value and then multiply by minus one
        else
          Result[i,j] <- sqrt(Result[i,j]) # normally sqrt the value itself
      }
    }
    n <- n + 1 # count the number of cycles
  }
  if(n>0) # warn the user about scaling
    cat(n," times square root is taken (elementwise) for better scaling.\n",sep="")
  require(lattice)
  pdf(filename) # save following plot to a file with pdf format 
  levelplot(Result,main = main) # plot levels with corresponding colors
  dev.off() # turn the device off
  return(Original) # return the original correlations matrix
}

# for quick arranging of Month10.txt and similars
Format.SaleData <- function(data,na.rm = FALSE,ziplim = 10^5,solim = 10^9){
  Result <- GetTime(data,2) # get time series
  Result <- Convert(Result,c(3,4),na.rm = na.rm,limupper = ziplim) # filter zips
  Result <- Convert(Result,1,na.rm = na.rm,limupper = solim) # filter SOnumbers
  Result <- SetColNames(Result) # set column names
  return(Result)
}

# for quick arranging of ShippingData_Months_10to12.txt and similars
Format.ShippingData <- function(data,na.rm = FALSE,ziplim = 10^5,solim = 2*(10^9),dulim = 1500){
  Result <- GetTime(GetTime(data,9),10) # get time series
  Result <- Convert(Result,c(4,5),na.rm = na.rm,limupper = ziplim) # filter zips
  Result <- Convert(Result,8,na.rm = na.rm,limupper = solim) # filter SOnumbers
  Result <- Convert(Result,c(7,11),na.rm = na.rm,limupper = dulim) # filter duration and one more
  Result <- Convert(Result,c(1,2,3,6),class = "character",na.rm = na.rm,limupper = solim) # set rest as char
  Result <- SetColNames(Result,type = "shipping.data") # set column names
  return(Result)
}

### ----------------------------------------------------------------------- ###
### - Text File and Data Editor Functions --------------------------------- ###
### ----------------------------------------------------------------------- ###

# Kindly note that following functions are not optimized, but left original as first written.
TxtFindFirst <- function(filename,value,delimiter){ # return the indexes of the value found at first
  data <- read.table(filename,sep=delimiter) # read the text file
  for (j in 1:ncol(data)){
    for (i in 1:nrow(data)){
      if(data[i,j] == value){# if found, store indexes
        Result <- c(i,j)
        names(Result) <- c("Row","Column")
        return(Result)
      } 
    }
  }
  cat(value,"is not found!\n")
  return(-1)
}

TxtFindAll <- function(filename,value,delimiter){ # return the indexes of the value found everywhere
  data <- read.table(filename,sep=delimiter) # read the text file
  n <- 1 # initialize a variable for indexing
  rows <- 0 # declare this in order to store row indexes
  cols <- 0 # declare this in order to store column indexes
  for (j in 1:ncol(data)){
    for (i in 1:nrow(data)){
      if(data[i,j] == value){ # if found, store indexes
        rows[n] <- i
        cols[n] <- j
        n <- n + 1 # increase by one to store possible incoming indexes, in "rows" and "cols"
      }
    }
  }
  if(n==1){
    cat(value,"is not found!\n")
    return(-1)
  }
  cat(value,"found at\n")
  Result <- c(rows,cols) # bound together as one dimensional vector
  dim(Result) <- c(n-1,2) # to give the final shape
  colnames(Result) <- c("Row","Column") # assign column names
  return(Result)
}

TxtEditRow <- function(filename,rowindex,newvalue,delimiter){ # edit a complete row, return edited data
  data <- read.table(filename,sep=delimiter) # read the text file
  if(ncol(data) != length(newvalue)){ # do nothing if incorrect dimensions
    cat("It should have",ncol(data),"columns.")
    return (-1)
  }
  else{
    Result <- data # back up the content
    temp1 <- c("was : ",Result[rowindex,]) # define a temporary variable for better display
    dim(temp1) <- c(1,ncol(Result)+1)
    print(temp1)
    Result[rowindex,] <- newvalue # set new values
    temp2 <- c("now : " ,Result[rowindex,])
    dim(temp2) <- c(1,ncol(Result)+1)
    print(temp2)
    return(Result)
  }
}

TxtSort <- function(filename,colindex,delimiter){ # descending order, with respect to chosen column
  data <- read.table(filename,sep=delimiter) # read the text file
  Result <- data # back up the content
  rows <- nrow(data) # store the number of elements in that column
  # Bubble sort algorithm starts
  for (i in 1:rows){
    for (n in 1:(rows-1)){ # inspect numbers by pairs
      if(!is.null(Result[n,colindex]) & !is.null(Result[n+1,colindex])){ # if not NULL
        if(Result[n,colindex] < Result[n+1,colindex]){ # compare each pair inside
          temp <- Result[n,]          #
          Result[n,] <- Result[n+1,]  # relocate entire row
          Result[n+1,] <- temp        #
        }
      }
      else # if NULL
        cat("NULL at :",n,colindex)
    }  
  }
  # Bubble sort algorithm ends
  return (Result)
}

TxtConcatenate <- function(filename1,delimiter1,filename2,delimiter2){
  data1 <- read.table(filename1,sep = delimiter1) # read the text file
  data2 <- read.table(filename2,sep = delimiter2)
  if (ncol(data1) != ncol(data2)){ # do nothing if incorrect dimensions
    print("Dimensions must agree!")
    return (-1)
  }
  Result <- matrix(0,nrow(data1)+nrow(data2),ncol(data1)) # initialize with zeros
  Result <- data1 # fill data1 from the beginning
  for (i in (nrow(data1)+1):(nrow(data1)+nrow(data2))) # fill data2 to the rest
    Result[i,] <- data2[i-nrow(data1),]
  return (Result)
}
