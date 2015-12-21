# Count the number of pages transcribed each day

library(oldWeather5)
library(lubridate)

classifications<-ReadClassifications('../../data-exports/classifications.csv')

Days<-list()
for(i in seq_along(classifications$core$created_at)) {
  ct<-classifications$core$created_at[i]
  key<-sprintf("%04d-%02d-%02d",year(ct),month(ct),day(ct))
  if(is.null(Days[[key]])) Days[[key]]<-0
  Days[[key]]<-Days[[key]]+1
}
days<-sort(names(Days))

system('rm pages_per_day.txt')
for(i in seq_along(days)) {
  cat(sprintf("%s %d\n",days[i],Days[[days[i]]]),
                  file="pages_per_day.txt",append=TRUE)
}
                                       

