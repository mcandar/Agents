# Drawing graphs with qplot for ShippingData_Months_07to09.txt

library(ggplot2)

DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

Convert <- function(data,col,class="numeric",na.rm=FALSE){ # unfactors and extracts the content out of it as numeric
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

for(i in 1:ncol(Arranged)){
  filename <- paste("QuickPlot",i,".pdf",sep = "") # form a file name with an index
  plotname <- paste("Months July to Sept","#",i,sep = "")
  y <- as.vector(Arranged[,i])
  qplot(1:nrow(Arranged),y,geom = "jitter",alpha = I(1/10),xlab = "Index",ylab = colnames(Arranged[i]),main = plotname)
  ggsave(filename,device = "pdf")
}
