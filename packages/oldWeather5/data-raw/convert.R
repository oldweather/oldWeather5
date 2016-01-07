library(png)
bg<-readPNG('data-raw/background.png')
background.img<-bg[,1:960,] # Truncate to 4:3 aspect
devtools::use_data(background.img)
