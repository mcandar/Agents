# Calculation of correlation and covariance inside a given data frame

DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){ 
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

CorList <- function(data,excludecols = c(0),filewrite = FALSE,filename = "Correlations.txt"){
  cols <- ncol(data)
  n <- 1
  m <- 1
  Result <- data.frame(matrix(NA,1,5))
  for(i in 1:cols){
    for(j in m:cols){
      if (any(excludecols==i) || any(excludecols==j) || i == j) next()
      Result[n,] <- c(cor(data[,i],data[,j]),i,j,colnames(data[i]),colnames(data[j]))
      # Note that when a non-numeric type variable is added into matrix, Result[,-c(1)] notation
      # does not work, yielding the error "invalid argument to unary operator".
      n <- n + 1
    }
    m <- m + 1
  }
  colnames(Result) <- c("Correlations","Index_1","Index_2", "Name_1","Name_2")
  if(filewrite) write.table(Result,filename,sep = " ")
  return(Result[order(Result[,1]),])
}

CovList <- function(data,excludecols = c(0),filewrite = FALSE,filename = "Covariances.txt"){
  cols <- ncol(data)
  n <- 1
  m <- 1
  Result <- data.frame(matrix(NA,1,5))
  for(i in 1:cols){
    for(j in m:cols){
      if (any(excludecols==i) || any(excludecols==j) || i == j) next()
      Result[n,] <- c(cov(data[,i],data[,j]),i,j,colnames(data[i]),colnames(data[j]))
      n <- n + 1
    }
    m <- m + 1
  }
  colnames(Result) <- c("Covariances","Index_1","Index_2","Name_1","Name_2")
  if(filewrite) write.table(Result,filename,sep = " ")
  return(Result[order(Result[,1]),])
}

Shipping1 <- DetectTime(DetectTime(read.table("ShippingData_Months_07to09.txt",sep = ","),9),10)
Shipping2 <- DetectTime(DetectTime(read.table("ShippingData_Months_10to12.txt",sep = ","),9),10)

CorList(Shipping1,excludecols = c(1,2,3,6,9,10),filewrite = TRUE,filename = "Correlations_07to09.txt")
CovList(Shipping1,excludecols = c(1,2,3,6,9,10),filewrite = TRUE,filename = "Corvariances_07to09.txt")
CorList(Shipping2,excludecols = c(1,2,3,6,9,10),filewrite = TRUE,filename = "Correlations_10to12.txt")
CovList(Shipping2,excludecols = c(1,2,3,6,9,10),filewrite = TRUE,filename = "Corvariances_10to12.txt")