# # variance-covariance matrix for a multivariate normal distribution
# s <- matrix(c(1, .5, .5,
#               .5, 1, .5,
#               .5, .5, 1), ncol = 3)
# # use the mvtnorm package to sample 200 observations
# obs <- mvtnorm::rmvnorm(200, sigma = s)
# # collect everything in a data-frame
# df <- setNames(data.frame(obs), c("x", "y", "z"))
# df <- as.data.frame(df)

require(plotly)

m <- 10^4
n <- 10^4
df <- data.frame(x = sample(n,m),
                 y = sample(n,m),
                 z = sample(n,m),
                 t = 1:m)
df
class(a)
month10 <- plot_ly(a, x = SenderZip, y = ReceipentZip, z = ShippingDate,color = SONumber, type = "scatter3d", mode = "markers")%>%
  layout(title="3D Scatter for October 2012")
htmlwidgets::saveWidget(as.widget(month10), "3D_PLOT_MONTH10.html")
month10

MultiPlotly3d <- function(data,
                          x,
                          y,
                          z,
                          color,
                          fname = "3d",
                          main = "3dPlot",
                          type = "scatter3d", 
                          mode = "markers"){
  
  require(plotly)
  if(is.null(colnames(x)))
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  if(is.null(colnames(y)))
    y <- as.data.frame(y) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  # for(i in 1:ncol(z)){
    filename <- paste(fname,"_",i,"_",
                      colnames(x[1]),"_",
                      colnames(y[1]),"_",
                      colnames(z[i]),".html",sep = "") # form a file name with an index
    
    plotname <- paste(main," #",i,sep = "")
    
    p <- plot_ly(data, x = x, y = y, z = z[,i],
            color = t, 
            type = "scatter3d", 
            mode = "markers") %>% 
      layout(title = plotname,
             scene = list(
               xaxis = list(title = colnames(x[1])), 
               yaxis = list(title = colnames(y[1])), 
               zaxis = list(title = colnames(z[i]))))
    htmlwidgets::saveWidget(as.widget(p), filename) # save as an html page to keep interactive tools
    cat("Image",filename,"saved to",getwd(),"\n")
  # }
  return(p)
}

a <- Raw[sample(nrow(A),1000),]
MultiPlotly3d(a,a$SenderZip,a$ReceipentZip,a$ShippingDate,color = SONumber)

MultiGGPlot <- function(x,                    # data of x axis
                        data,                 # data set of y axis
                        main = "GGPlot",      # main title
                        fname = "GG_",        # prefix for filename
                        geom = "jitter",      # geometry of the plot
                        alpha = I(1/10),      # transparecy of the points
                        smooth = FALSE,       # de/activate smoothing
                        smethod = "lm",       # smoothing method
                        formula=y ~ poly(x, 3, raw=TRUE), # formula for smoothing, linear, polynomial etc.
                        width = 8,            # width as inches
                        height = 4.5){        # height as inches
  
  require(ggplot2)
  if(is.null(colnames(x))) 
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(data)){
      filename <- paste(fname,"_",i,"_",colnames(data[i]),"_vs_",colnames(x[j]),".png",sep = "") # form a file name with an index
      plotname <- paste(main," #",i,sep = "")
      ggplot(data, 
             aes(x = x[,j],
                 y = data[,i])) +
        switch (geom,
                "jitter" = {geom_jitter(alpha = alpha)},
                "point" = {geom_point(alpha = alpha)},
                "path" = {geom_path(alpha = alpha)}
        ) +
        
        labs(title = main,
             x = colnames(x[j]),
             y = colnames(data[i])) +
        
        if(smooth)
          geom_smooth(method=smethod, se=TRUE, fill=NA,
                      formula=formula,colour="blue")
      
      ggsave(filename,device = "png",width = width,height = height)
      cat("Image",filename,"saved to",getwd(),"\n")
    }
  }
  return (TRUE)
}
