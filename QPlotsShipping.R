# Drawing graphs with qplot for ShippingData_Months_07to09.txt

library(ggplot2)

DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){
  Result <- data # back up the data
  for(i in col){
    Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  }
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

Arranged <- DetectTime(DetectTime(read.table("ShippingData_Months_07to09.txt",sep = ","),9),10)
colnames(Arranged) <- c("TrackingNumber","Company","ShippingCode","SenderZip","ReceipentZip","Type",
                      "Item Weight","SONumber","DateShipped","DateDelivered","Duration")
Arranged <- Convert(Arranged,col = c(4,5),na.rm = TRUE)

# for(i in 1:ncol(Arranged)){
#   
#   filename <- paste("QuickPlot",i,".png",sep = "") # form a file name with an index
#   plotname <- paste("Months July to Sept","#",i,sep = "")
#   y <- as.vector(Arranged[,i])
#   qplot(1:nrow(Arranged),y,geom = "jitter",alpha = I(1/10),xlab = "Index",ylab = colnames(Arranged[i]),main = plotname)
#   ggsave(filename,device = "pdf")
# }

backupArranged2 <- Arranged
Arranged <- B
Arranged <- DetectTime(Arranged,col = 9) # tried col = c(9,10) but error appeared
Arranged <- Convert(Arranged,c(4,5),na.rm = TRUE,limupper = 10^5)
# Arranged <- Convert(Arranged,8,na.rm = TRUE,limupper = 10^9)
Arranged <- Convert(Arranged,11,na.rm = TRUE,limupper = 45)

# Arranged <- Arranged[which(as.numeric(as.duration(
#  interval(Arranged[100,10],Arranged[101,10]))31536000))] # try date restrictions using lubridate

require(ggplot2)
require(lubridate)
n <- 0
for(j in 1:ncol(Arranged)){
  for(i in 1:ncol(Arranged)){
    if(j == i) next()
    filename <- paste(colnames(Arranged[i])," vs ",colnames(Arranged[j]),".png",sep = "") # form a file name with an index
    plotname <- paste("Months July to Sept","#",i,sep = "")
    y <- as.vector(Arranged[,i])
    x <- as.vector(Arranged[,j])
    # qplot(x,y,geom = "jitter",alpha = I(1/100),xlab = colnames(Arranged[j]),ylab = colnames(Arranged[i]),main = plotname)
    ggplot()
    ggsave(filename,device = "png",width = 8,height = 4.5)
    n <- n + 1
  }
  print(paste(n,"/",ncol(Arranged)*(ncol(Arranged)-1)," graphs are drawn.", sep="")) # use round()
}

##################### TO BE DELETED #########################
ggplot(Arranged, 
       aes(x = Arranged[,11],
           y = Arranged[,2],
           fill = Arranged[,4]),
       xlab(colnames(Arranged[11])),
       ylab(colnames(Arranged[2]))) +
  
  geom_jitter(alpha = I(1/100),colour = Arranged[,4]) + 
  
  labs(title = "GGPlot Sample",
       x = "Travel Duration",
       y = "Shipping Type") +
  
  # guide_legend(title="SenderZip")

  ggsave("ggplot2.4.png",device = "png")

# CORRECT BELOW CODE, IT IS PROPERLY WORKING!! DO NOT DELETE IT!!
# MultiPlot function helps visualing a given data by graphing all variables against a selected variable
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
    filename <- paste(fname,"_",i,"_",colnames(Arranged[i]),"_vs_",labelx,".png",sep = "") # form a file name with an index
    plotname <- paste(main," #",i,sep = "")
    ggplot(data, 
           aes(x = x,
               y = data[,i])) +
      switch (geom,
        "jitter" = {geom_jitter(alpha = alpha)},
        "point" = {geom_point(alpha = alpha)},
        "path" = {geom_path(alpha = alpha)}
      ) +
      # geom_jitter(alpha = I(1/100)) + 
      
      labs(title = "GGPlot Sample",
           x = labelx,
           y = colnames(data[i])) +
      
    ggsave(filename,device = "png",width = width,height = height)
    cat("Image",filename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}

MultiQPlot(1:nrow(Arranged),Arranged[,c(4,5)],fname = "Z",alpha = I(1/100))
##### source("...") gets the code and executes it !!
source("cleandata.R")
