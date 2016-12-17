source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# a function for easily extracting summarized data out of unified data, on monthly basis.
# this function lists the items and gets the corresponding information about them
Outcost.Summary <- function(ship_unified, # the big unified shipdata, (latest) on monthly basis (e.g. month_m10 etc.)
                            filter.stat = FALSE, # filter according to uses
                            limit.stat = 5, # take if used equal or more than 5, remove rest
                            col.products = 25, # column of product names in unified shipdata
                            col.cost = 7, # column of shipping costs in unified shipdata
                            col.dist = 20, # column of average distances in unified shipdata
                            col.units = 27, # column of amount of shipped units
                            col.price = 28, #  column of prices in unified shipdata
                            col.wei = 26, #  column of weight in unified shipdata
                            col.dur = 11, # column of durations in unified shipdata
                            col.slat = 12, # column of sender lat in unified shipdata
                            col.slon = 13, # column of sender lon in unified shipdata
                            col.rlat = 14, # column of receipent lat in unified shipdata
                            col.rlon = 15 # column of receipent lon in unified shipdata
){
  lvl <- levels(factor(ship_unified[,col.products])) # product name levels of the most expensive 100 deliveries in October
  output <- as.data.frame(matrix(NA,length(lvl),13))
  colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance","NumberofUses",
                        "TotalUnitsShipped","Price","Weight","AveDuration","S.AveLat","S.AveLon","R.AveLat","R.AveLon")
  for(i in 1:length(lvl)){
    ind <- which(ship_unified[,col.products]==lvl[i])
    output[i,] <- cbind(lvl[i], # product name #1
                        as.numeric(sum(na.omit(ship_unified[ind,col.cost]))), # total cost #2
                        as.numeric(mean(na.omit(ship_unified[ind,col.cost]))), # average cost #3
                        as.numeric(mean(na.omit(ship_unified[ind,col.dist]))), # # average distance #4
                        as.numeric(length(ind)), # number of uses #5 but not units
                        as.numeric(sum(na.omit(ship_unified[ind,col.units]))), # number of shipped units #6
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.price]), # price #7
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.wei]), # weight #8
                        as.numeric(mean(na.omit(ship_unified[ind,col.dur]))), # average duration #9
                        as.numeric(mean(na.omit(ship_unified[ind,col.slat]))), # sender lattitude #10
                        as.numeric(mean(na.omit(ship_unified[ind,col.slon]))), # sender longtitude #11
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlat]))), # receipent lattitude #12
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlon])))) # receipent longtitude #13
  }
  class(output$TotalShippingCost) <- "numeric"
  output <- Convert(Sort(output,2,decreasing = TRUE),col = c(3,4,5,6,7,8,9,10,11,12,13))
  if(filter.stat)
    output <- output[which(output$NumberofUses>=limit.stat),] # filter by number of uses, just take which is delivered more than 4
  return(output)
}

# for (i in 1:6){
#   raw <- read.csv(file=paste("final_matched_m",i+6,".csv",sep = ""),row.names = NULL)
#   temp <- ByState.Summary(raw)
#   ByState.Map(temp,main = paste("Outcost by State Month",i+6),filename = paste("USA_OUTCOST_BYSTATE_M",i+6,".html",sep = "") )
#   print(i)
# }

for (i in 1:6){
  raw <- read.csv(file=paste("final_matched_m",i+6,".csv",sep = ""),row.names = NULL)
  init <- Outcost.Summary(raw,filter.stat = TRUE)
  write.csv(init,paste("summary_final_matched_m",i+6,".csv",sep = ""))
  print(i)
}