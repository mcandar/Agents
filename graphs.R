# graph for a brownian motion

Brownian <- function(rows,cols,min,max){
  randomreal <- runif(rows*cols,-10,10)
  dim(randomreal) <- c(rows,cols)
  binit <-100 
  bvect <- matrix(NA,rows,cols)
  for (j in 1:cols){
    for (i in 1:rows){
      bvect[i,j] <- binit
      binit <- binit + (randomreal[i,j])
    }
    binit <- 100
  }
  return(bvect)
}

myresult <- Brownian(100,5,-10,10)
matplot(1:100,myresult,type = "l",col = 1:5,xlab = "Time",ylab = "Price")
# legend("topleft", legend = 1:cols, col=1:cols, pch=1)
grid()

