# Extract the time taken to do each page

library(oldWeather5)
library(lubridate)

classifications<-ReadClassifications('../../data-exports/classifications.csv')

  pd<-as.numeric(classifications$meta$finished_at)-
      as.numeric(classifications$meta$started_at)
  cat(sprintf("%g\n",pd),
                  file="time_per_page.txt",append=FALSE)
                                       

