# Count the number of pages transcribed by each user

library(oldWeather5)

classifications<-ReadClassifications('../../data-exports/classifications.csv')

Users<-list()
for(i in seq_along(classifications$core$created_at)) {
  user<-classifications$core$user_name[i]
  if(is.null(Users[[user]])) Users[[user]]<-0
  Users[[user]]<-Users[[user]]+1
}
o<-sort.list(unlist(Users))
Users<-Users[rev(o)]

system('rm pages_per_user.txt')
for(i in seq_along(Users)) {
  cat(sprintf("%s %d\n",names(Users)[i],Users[[i]]),
                  file="pages_per_user.txt",append=TRUE)
}
                                       

