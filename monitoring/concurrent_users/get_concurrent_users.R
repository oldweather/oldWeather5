# Count the number of simultanious users

library(oldWeather5)
library(lubridate)

# Time resolution for comparisons (seconds)
time.resolution<-60

classifications<-ReadClassifications('../../data-exports/classifications.csv')

start.time<-min(classifications$meta$started_at,na.rm=TRUE)
end.time<-max(classifications$meta$started_at,na.rm=TRUE)

system('rm concurrent.users.txt')
current.time<-start.time
while(current.time<end.time) {
  current.time<-current.time+seconds(time.resolution)
  w<-which(classifications$meta$started_at<=
                       (current.time+seconds(time.resolution/2)) &
           classifications$meta$finished_at>=
                       (current.time-seconds(time.resolution/2)))
  cat(sprintf("%04d-%02d-%02d:%02d:%02d:%02d %d\n",year(current.time),
                 month(current.time),day(current.time),hour(current.time),
                 minute(current.time),as.integer(second(current.time)),
                 length(w)),
                  file="concurrent.users.txt",append=TRUE)
}   


                                       

