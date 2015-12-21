# Count the number of users transcribing each day

library(oldWeather5)
library(lubridate)

classifications<-ReadClassifications('../../data-exports/classifications.csv')

Days<-list()
for(i in seq_along(classifications$core$created_at)) {
  ct<-classifications$core$created_at[i]
  key<-sprintf("%04d-%02d-%02d",year(ct),month(ct),day(ct))
  if(is.null(Days[[key]])) Days[[key]]<-list()
  Days[[key]][[classifications$core$user_name[i]]]<-
      Days[[key]][[classifications$core$user_name[i]]]+1
}
days<-sort(names(Days))

system('rm users_per_day.txt')
for(i in seq_along(days)) {
  cat(sprintf("%s %d\n",days[i],length(Days[[days[i]]])),
                  file="users_per_day.txt",append=TRUE)
}
                                       

