# graph for a brownian motion

Brownian <- function(rows,cols,min,max){
  randomreal <- runif(rows*cols,-10,10) # form a a matrix with random real numbers
  dim(randomreal) <- c(rows,cols)
  binit <-100 # determine a starting point on y axis
  bvect <- matrix(NA,rows,cols) # form a matrix which each row corresponds to a different path
  for (j in 1:cols){
    for (i in 1:rows){
      bvect[i,j] <- binit # randomly increase or decrease initial point at each step
      binit <- binit + (randomreal[i,j])
    }
    binit <- 100 # reset the initial point to intersect all at beginning
  }
  return(bvect)
}

myresult <- Brownian(100,5,-10,10)
matplot(1:100,myresult,type = "l",col = 1:5,xlab = "Time",ylab = "Price") # plot them together
# legend("topleft", legend = 1:cols, col=1:cols, pch=1)
grid()
