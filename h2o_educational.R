# h2o package, educational

library(h2o)
h2o.init(nthreads = -1) # start h2o
# train.hex <- h2o.importFile("https://h2o-public-testdata.s3.amazonaws.com/smalldata/iris/iris_wheader.csv")
train.hex <- h2o.importFile("iris_dataset.csv")
splits <- h2o.splitFrame(train.hex, 0.75, seed=1234)
dl <- h2o.deeplearning(x=1:3, y="Petal.Length",
                         training_frame=splits[[1]],
                         distribution="quantile", quantile_alpha=0.8)
h2o.predict(dl, splits[[2]])

####

library(h2o)
h2o.init(nthreads = -1) # start h2o, use all CPU's
# train.hex <- h2o.importFile("https://h2o-public-testdata.s3.amazonaws.com/smalldata/iris/iris_wheader.csv")
JT <- read.csv("JOULETHOMS_DATA.csv",row.names = NULL,sep = ";")
train.hex <- h2o.importFile("JOULETHOMS_DATA.csv")
splits <- h2o.splitFrame(train.hex, 0.75, seed=1234)
dl <- h2o.deeplearning(x=1, y=1,
                       training_frame=splits[[1]],
                       distribution="quantile", quantile_alpha=0.8)
pre <- h2o.predict(dl, splits[[2]])
pre
c(JT$deltaT_N,pre)
c(JT$deltaT_CO,pre)

pdf("pre_plot.pdf")
plot(1:13,test)
dev.off()

library(h2o)
h2o.init(nthreads = -1) # start h2o, use all CPU's
# train.hex <- h2o.importFile("https://h2o-public-testdata.s3.amazonaws.com/smalldata/iris/iris_wheader.csv")
init <- read.csv("month7_mostexp_item.csv",row.names = NULL)
train.hex <- h2o.importFile("month7_mostexp_item.csv")
splits <- h2o.splitFrame(train.hex, 0.75, seed=1234)
dl <- h2o.deeplearning(x=10, y=3,
                       training_frame=splits[[1]])
                       # distribution="quantile", quantile_alpha=0.8)
pred <- h2o.predict(dl, splits[[2]])
pred

####################

library(h2o)
h2o.init(nthreads = -1) # start h2o
# train_ship_h2o <- as.h2o(trainer_ship)
train_ship_h2o <- h2o.importFile("ShippingData_Months_10to12.txt")


model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train_ship_h2o,
                          validation_frame = train_ship_h2o[1:10^4,],
                          distribution = "multinomial",
                          activation = "RectifierWithDropout",
                          hidden = c(32,32,32),
                          input_dropout_ratio = 0.2,
                          sparse = TRUE,
                          l1 = 1e-5,
                          epochs = 10)
