# Following code graphs and sorts the file Month10.txt.

# DetecTime function is to convert and arrange time column from 
# characters to catch any exception and for a more reliable analysis
DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){ 
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

# MultiPlot function helps visualing a given data by graphing all variables against a selected variable
MultiPlot <- function(x,                    # data of x axis
                      data,                 # data set of y axis
                      excludecols = c(0),   # index of unwanted columns in graph  
                      labelx = "Index",     # x label
                      method = "plot",      # two options, for ono-to-one: "plot", one-to-many: "matplot"
                      name = "Plot_",       # name of the file which graph will be saved
                      width = 1920,         # width as pixels
                      height = 1080){       # height as pixels
  
  for(i in 1:ncol(data)){
    filename <- paste(name,"_",i,".png",sep = "")
    if (any(excludecols == i)) next() # pass to next step if excluded columns encountered
    png(filename,width = width,height = height)
    switch (method, # switch for different plot options
            "plot" = { # plot one column versus another one
              plotname <- paste("Simple Plot #",i,sep = "")
              plot(x,data[,i],xlab = labelx,ylab = colnames(data[i]),main = plotname)
            },
            "matplot" = { # plot one column versus rest of the columns
              labelx <- colnames(data[i])
              plotname <- paste("Multiple Plot #",i,sep = "")
              matplot(data[,i],data[,-c(i,excludecols)],xlab = labelx,ylab = colnames(data[-c(i,excludecols)]), 
                      main = plotname,col = 1:(ncol(data)-length(excludecols)-1),
                      pch=1:(ncol(data)-length(excludecols)-1))
              legend("topleft",legend = colnames(data[-c(i,excludecols)]), 
                     col=1:(ncol(data)-length(excludecols)-1),pch=1:(ncol(data)-length(excludecols)-1))
              grid()
            }
    )
    dev.off()
  }
  return (TRUE)
}

# main
# Arranged <- DetectTime(read.table("Month10.txt",sep = ","),2)
Arranged <- DetectTime(DetectTime(read.table("ShippingData_Months_07to09.txt",sep = ","),9),10)
colnames(Arranged) <- c("Column1","Column2","Column3","Column4","Column5","Column6","Column7","Column8","Column9",
                        "Column10","Column11")

## Visualize ##
MultiPlot(1:nrow(Arranged),Arranged,excludecols = c(1,2,6),name = "Shipping.MyPlot")
MultiPlot(Arranged[,9],Arranged,excludecols = c(1,2,6,9),name = "Shipping.MyPlotTime1",labelx = "Time (Column 9)")
MultiPlot(Arranged[,10],Arranged,excludecols = c(1,2,6,10),name = "Shipping.MyPlotTime2",labelx = "Time (Column 10)")
MultiPlot(1:nrow(Arranged),Arranged,excludecols = c(1,2,6,9,10),name = "Shipping.MyMatPlot",method = "matplot")

library(graphics)
png("Shipping.PairPlot_1.png",width = 2000,height = 2000) # Save the figure for a closer look
pairs(~Arranged[,3]+Arranged[,4]+Arranged[,5]+Arranged[,7]+Arranged[,8]+Arranged[,11],data=Arranged,
      labels = colnames(Arranged[-c(1,2,6,9,10)]))
dev.off()
