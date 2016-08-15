# Following code graphs and sorts the file Month11.txt.

# DetecTime function is to convert and arrange time column from 
# characters to catch any exception and for a more reliable analysis

# DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){ 
#   Result <- data # back up the data
#   Result$V2 <- strptime(data[,col],format=format) # convert date and time from characters
#   return(Result)
# }

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
    filename <- paste(name,i,".png",sep = "")
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
Arranged <- DetectTime(read.table("Month11.txt",sep = ","),2) #
colnames(Arranged) <- c("SONumber","Shipping Date","Sender Zip","Receipent Zip","Item Description","Item Weight",
                        "Units Shipped","Average Unit Price")
m <- strsplit(as.character(Arranged[,4]),"-")
n <- as.data.frame(matrix(lapply(m, "[",1)))
# Arranged[,4] <- as.data.frame(trimws(n[[1]]))
Arranged[,4] <- as.data.frame(n[[1]])
Arranged$`Receipent Zip` <- as.numeric(as.character(Arranged$`Receipent Zip`))
# unfactors and extracts the content out of it as numeric (integer)

m <- strsplit(as.character(Arranged[,3]),"-")
n <- as.data.frame(matrix(lapply(m, "[",1)))
# Arranged[,3] <- as.data.frame(trimws(n[[1]]))
Arranged[,3] <- as.data.frame(n[[1]])
Arranged$`Sender Zip` <- as.numeric(as.character(Arranged$`Sender Zip`))


## Visualize ##
MultiPlot(1:nrow(Arranged),Arranged,excludecols = c(4,5),name = "MyPlot")
MultiPlot(Arranged[,2],Arranged,excludecols = c(4,5),name = "MyPlotTime",labelx = "Time")
MultiPlot(1:nrow(Arranged),Arranged,excludecols = c(2,4,5),name = "MyMatPlot",method = "matplot")
