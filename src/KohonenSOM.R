
# Muokkaa gridi siten, etta jokaiseen ruutuun jaa min 10-15 havaintoa

library(kohonen)

FitKohonenSOM <- function(df, xdim, ydim, rlen){
  df <-  na.omit(df) 
  df <- as.matrix((df))
  
  som_grid <- somgrid(xdim = xdim, ydim=ydim, topo="hexagonal")
  
  som_model <- supersom(df, 
                        grid=som_grid, 
                        rlen=rlen, 
                        alpha=c(0.05,0.01), 
                        keep.data = TRUE)
  
}

#Piirra kohosen paketin mukaan tehdylle mallille heatmapit muuttujittain

PrintKohonenSOM <- function(x, tiedostonNimi) {
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  pvm <- Sys.Date()
  tiedosto <-
    paste(tiedostonNimi, pvm,".pdf")
  pdf(file=tiedosto)
  par(mfrow = c(4, 3)) 
  for (col.name in 1:dim(som_model$codes[[1]])[2]){
    par(cex.main = 1)
    plot(
      x,
      type = "property",
      property = som_model$codes[[1]][,col.name],
      main = colnames((som_model$data[[1]]))[col.name],
      palette.name = coolBlueHotRed,
      heatkeywidth = .6,
      shape = c("straight")
    )
  }
  dev.off()
}

