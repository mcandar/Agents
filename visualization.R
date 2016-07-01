RV <- sample(10,100,replace = TRUE) # form a matrix full of random integers
RM <- matrix(0,100,10) # declare and initialize a matrix to fill later, 100 rows, 10 columns

for (i in 1:10){
  RM[,i] <- RV*i*i # fill each column with random integers but increase by square at each step
}

init <- 100 # starting point on y axis
RM2 <- matrix(0,100,10) # declare and initialize a matrix to fill later, 100 rows, 10 columns
for (n in 1:10){
  RV <- RV*(n/5)
  for (m in 1:100){
    RM2[m,n] <- init # set the starting point
    init <- init + RV[m] # randomly increase at each step
  }
  init <- 100 # reset at the end of the column
}

png("mygraph1.png",width = 1366,height = 768) # turn on the image saver and create the image
persp(RM,theta = -60) # draw the graph
dev.off() # save it and turn the device off

png("mygraph2.png",width = 1366,height = 768)
persp(RM2,theta = -60)
dev.off()
