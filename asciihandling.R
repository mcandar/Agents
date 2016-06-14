#text file handling

TxtFindFirst <- function(filename,value){
  data <- read.table(filename)
  for (i in 1:ncol(data)){
    output <- match(value,data[,i])#,nomatch = NA_integer_)
    if(!is.na(output)){
      return(output)
      break
    }
    else
      return(-1) # if not found
    }
}

# TxtFindAll <- function(filename,value){ # search by row (INCOMPLETE)
#   data <- read.table(filename)
#   n <- 1
#   rows <- c(0,0)
#   cols <- c(0,0)
#   for (j in 1:ncol(data)){
#     for (i in 1:nrow(data)){
#       if(data[i,j] == value){
#         rows[n]<-i
#         cols[n]<-j
#         n <- n + 1
#       }
#     }
#   }
#   print(rows)
#   print(cols)
#   output <- c(rows,cols)
#   dim(output) <- c(n,2)
#   return(output)
# }

TxtEditRow <- function(filename,rowindex,newvalue){
  data <- read.table(filename)
  if(ncol(data) != length(newvalue)){
    print("It should have " + toString(ncol(data)) + "columns.")
    return -1
  }
  else{
    edited <- data
    print(c("was : ",edited[rowindex,]))
    edited[rowindex,] <- newvalue
    print(c("now : " ,edited[rowindex,]))
  }
}

TxtSort <- function(filename,colindex){ # descending order by row
  data <- read.table(filename)
  sorted <- data
  rows <- nrow(data)
  print(sorted)
  print(sorted[1,colindex] < sorted[2,colindex])
  for (i in 1:rows){
    for (n in 1:(rows-1)){
      if(!is.null(sorted[n,colindex]) & !is.null(sorted[n+1,colindex])){ # if not NULL
        if(sorted[n,colindex] < sorted[n+1,colindex]){ # bubble sort algorithm
          temp <- sorted[n,]
          sorted[n,] <- sorted[n+1,]
          sorted[n+1,] <- temp
        }
      }
      else
        print(c("NULL at :",n,colindex))
    }  
  }
  return (sorted)
}

TxtConcatenate <- function(filename1,filename2){
  data1 <- read.table(filename1)
  data2 <- read.table(filename2)
  if (ncol(data1) != ncol(data2)){
    print("Dimensions must agree!")
  return (-1)
  }
  Result <- matrix(0,nrow(data1)+nrow(data2),ncol(data1)) # initialize with zeros
  Result <- data1
  for (i in (nrow(data1)+1):(nrow(data1)+nrow(data2)))
    Result[i,] <- data2[i-nrow(data1),]
  return (Result)
}

#main
# print(TxtFindFirst("mysample2.txt",2))
# print(TxtFindAll("mysample2.txt",2))
# print(TxtEditRow("mysample2.txt",24,c(123,123,123),n))
# print(TxtSort("mysample2.txt",1))
print(TxtConcatenate("mysample.txt","mysample2.txt"))