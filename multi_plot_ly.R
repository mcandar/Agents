# medley cuts and visualization of outcost dataset

source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

MultiPlotly <- function(x,                    # data of x axis
                        y,                    # data set of y axis
                        main = "Plot_ly",      # main title
                        fname = "Plot_ly",        # prefix for filename
                        color = NULL,
                        colorname = NULL,
                        type = "scatter",
                        mode = "markers",
                        text = NULL, # hover text
                        size = NULL,
                        logy = NULL,
                        col.blank = NULL # column numbers which require a margin from bottom for easy observation of names
){      # height as inches
  require(plotly)
  m <- list(b = 200)
  
  dt_classes <- c("POSIXct","POSIXlt","POSIXt")
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  clr <- as.numeric(as.character(color))
  for(j in 1:ncol(x)){ # 1 through total column number, for x
    all_grobs <- NA
    # rearrange the order for factors and characters, prevent unwanted sorting
    if(class(x[,j])=="factor" || class(x[,j])=="character"){
      xlabel <- list(title = colnames(x[j]),categoryorder = "array",categoryarray = x[,j])
      print("Adjusting category order for x axis ...")
    }
    else
      xlabel <- list(title = colnames(x[j]))
    
    # predetermined bottom margins for better display, apply for selected columns
    if(any(j == col.blank)) margin = m
    else margin = NULL
    
    for(i in 1:ncol(y)){ # 1 through total column number, for y
      if (identical(x[,j],y[,i])) next() # pass through next step if identical vectors encountered
      
      plotname <- paste(main," #",i,sep = "") # initialize nomenclature
      
      # set the axis type to log scale according to column numbers
      if (!is.null(logy) && any(logy == i))
        current_type <- "log"
      else 
        current_type <- NULL
      
      # rearrange the order for factors and characters, prevent unwanted sorting
      # if(class(y[,i])=="factor" || class(y[,i])=="character"){
      if((class(y[,i])=="factor" || class(y[,i])=="character") && !any(class(y[,i]) %in% dt_classes)){
        ylabel <- list(title = colnames(y[i]),categoryorder = "array",categoryarray = y[,j],type = current_type)
        print("Adjusting category order for y axis ...")
      }
      else
        ylabel <- list(title = colnames(y[i]),type = current_type)
      
      # plot and save
      p <- plot_ly(x=x[,j],y=y[,i],type=type,mode=mode,size = size,color = clr,text = text) %>%
        layout(title = plotname,xaxis = xlabel,yaxis = ylabel,margin = margin) %>%
        colorbar(title = colorname)
      
      cat((j-1)*ncol(y)+i,colnames(x[j]),"vs",colnames(y[i]),"is plotted.\n")
      all_grobs <- list(all_grobs,p) # maybe try with tagList() from htmltools lib
    }
    # save all of the graphic objects as an html page list
    print("Saving in progress...")
    savename <- paste(fname,colnames(x[j]),"vs_all_LIST.html",sep = "_")
    # save_tags(all_grobs,savename)
    htmltools::save_html(all_grobs,savename)
    cat("Image",savename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}

# data filtering and cuts
MultiPlotly(x = my_samp,
            y = my_samp,
            # mode = "lines+markers",
            fname = "OutcostData_2D_Scatter",
            main = "Outcost Data 2-D Scatter Plot",
            # logy = c(1,2,3,4,5),
            text = my_samp$ItemDescription,
            color = my_samp$ShippingCost,
            colorname = "ShippingCost")

# final_matched <- read.csv("final_matched.csv",row.names = TRUE)
fm_month7 <- final_matched[which(),]
# monthly <- array(NA,dim = c(nrow(final_matched)/3,ncol(final_matched),6))
monthly7 <- final_matched[which(lubridate::month(final_matched$DateDelivered)==7),]
MultiPlotly(x = data.frame(Index=1:nrow(my_samp)),
            y = my_samp,
            # mode = "lines+markers",
            fname = "OutcostData_2D_Scatter_TEST",
            main = "Outcost Data 2-D Scatter Plot",
            # logy = c(1,2,3,4,5),
            text = my_samp$ItemDescription,
            color = my_samp$ShippingCost,
            colorname = "ShippingCost")

### test for alternative saving
p <- plot_ly(x = sample.int(10^5,10^4),type = "scatter",mode = "markers")
pl <- tagList(p, p) # combine them as a list
browsable(pl) # render them
save_html(pl,"taglist_plotly_test.html") # save as a list
htmlwidgets::saveWidget(as.widget(tagList(p,p,p)),"taglist_plotly_test.html")
###

library(reshape2)
library(plotly)

my_samp <- final_matched[sample(nrow(final_matched),10^4),-c(21,23,24)]
my_samp$DateShipped <- as.POSIXct(my_samp$DateShipped)
my_samp$DateDelivered <- as.POSIXct(my_samp$DateDelivered)
my_samp$ShippingDate <- as.POSIXct(my_samp$ShippingDate)
my_samp <- Convert(my_samp,c(1,2,3,6,16,17,18,19,22),class = "character")

# classifying months and adding
my_samp$Month <- lubridate::month(my_samp$DateDelivered)
my_samp <- my_samp[-which(my_samp$Month == 1),]
my_samp <- na.omit(my_samp)

library(ggplot2)
sp <- ggplot(my_samp, aes(x=seq(nrow(my_samp)),y=Distance)) + #,colour = seq(nrow(my_samp))) + 
  geom_point() +
  facet_grid(. ~ Month,scales = "free_x")
  
sp
#save
htmltools::save_html(plotly::ggplotly(sp),"test.html")

# sp + facet_grid(Month ~ .)
# sp <- sp +  facet_grid(. ~ Month)# this is a useful binning for months




########## VERY USEFUL KEEP FOLLOWING SAFE!!! ###########
# binned by month
# gg_fac_list <- lapply(my_samp, function(x){
#   crt <- ggplot(my_samp, aes(x=seq(nrow(my_samp)), y=x)) + geom_point()
#   crt <- crt + facet_grid(. ~ Month) # this is a useful binning for months
#   plotly::ggplotly(crt)
# })


library(ggplot2)
# binned by month
gg_fac_list <- lapply(my_samp, function(x){
  crt <- ggplot(my_samp, aes(x=seq(nrow(my_samp)), y=x)) + 
    geom_jitter(alpha = I(1/5)) +
    labs(title = "Outcost Data Binned by Month",x = "Count") +
    facet_grid(. ~ Month,scales = "free_x") # this is a useful binning for months
  plotly::ggplotly(crt)
})

library(htmltools)
# browsable(tagList(gg_fac_list)) # render locally
# library(htmlwidgets)
# saveWidget(as.widget(gg_fac_list),"ttttt.html")
# htmlwidgets::saveWidget(as.widget(gg_fac_list))

Multi_Facet <- function(x,
                        y,
                        facet,
                        main = "Facets",      # main title
                        filename = "Multi_Facet",        # prefix for filename, without file extension
                        directory = NULL,
                        geom = "point",
                        scales = "free_x",
                        alpha = 1,
                        libdir = "lib"
                        ){
  library(ggplot2)
  library(htmltools)
  
  main_path <- getwd() # main path before change
  if(is.null(directory))
    current_path <- paste(getwd(),paste("DataMatch",sample(1000,1),sep="_"),sep = "/") # folder names
  else if(!is.null(directory))
    current_path <- paste(getwd(),directory,sep = "/") # folder names
  
  dir.create(current_path) # create a new directory to store files
  setwd(current_path) # set new working directory
  cat("Working directory is set to",current_path,"\n")
  
  for(j in 1:ncol(x)){
    current <- NA
      for(i in 1:ncol(y)){
        if (identical(x[,j],y[,i])) next()
        init <- ggplot(my_samp, aes(x=x[,j], y=y[,i])) + 
          switch (geom,
                  "jitter" = {geom_jitter(alpha = alpha)},
                  "point" = {geom_point(alpha = alpha)},
                  "line" = {geom_line(alpha = alpha)}
          ) +
          labs(title = main,x = colnames(x)[j],y = colnames(y)[i]) +
          facet_grid(facet,scales = "free_x") # this is a useful binning for months
        current <- tagList(current,plotly::ggplotly(init))
      }
    savename <- paste(filename,"_all_vs_",colnames(x)[j],".html",sep = "")
    save_html(current,savename)#,libdir = libdir)
    cat("File",savename,"saved to",getwd(),"\n")
  }
  setwd(main_path) # reset working directory
  cat("Working directory is set to default,",main_path,"\n")
  return(TRUE)
}

Multi_Facet(my_samp,
            my_samp,
            facet = . ~ Month,
            filename = "TEST_FACET_",
            main = "Outcost Facet Plot Test",
            directory = "Facet_Dir")

# gg_fac_grid <- gridExtra::marrangeGrob(gg_fac_list,
# nrow = 26,ncol = 1) # for making grids and subplots, unnecessary for now, just for ggplot objects!
# ggsave("test_facet_grid.pdf",plot = gg_fac_grid)

