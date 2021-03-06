#text file handling

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

# main
# print(TxtFindFirst("mysample2.txt",2,"\t"))
# print(TxtFindAll("mysample2.txt",2,"\t")) # "\t" could be used instead of ""
# print(TxtEditRow("mysample2.txt",24,c(123,123,123),"\t"))
# print(TxtSort("mysample2.txt",1,"\t"))
# print(TxtConcatenate("mysample.txt","\t","mysample2.txt","\t"))
