# Beep every time a classification occurs

library(oldWeather5)
library(tuneR)

base<-readWave('sounds/beep-01a.wav')
base.length<-base@left/base@samp.rate

begin<-ymd_hms('2015-12-03 16:25:00')
end<-ymd_hms('2015-12-03 16:44:41')
speedup<-24 # times faster than real-time

# Make a blank sound file covering the whole period
s.length<-(as.numeric(end)-as.numeric(begin))*base@samp.rate/speedup

full<-base
full@left<-rep(0,s.length)

# Put in a beep for every classification in the period
classifications<-ReadClassifications('../data-exports/classifications.csv')
#subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)

w<-GetClassificationsByDate(classifications,begin,end)
begin<-as.numeric(begin)
end<-as.numeric(end)
for(i in w) {
  for(n in seq_along(classifications$annotations[[i]])) {
    c.time<-classifications$annotations[[i]][[n]]$timestamp/1000
    if(c.time<begin || c.time>end) next
    s.start<-jitter(c.time-begin,
                    amount=0.5)*base@samp.rate/speedup
    s.end<-s.start+length(base@left)-1
    if(s.end>length(full@left)) next
    full@left[s.start:s.end]<-full@left[s.start:s.end]+base@left
  }
}
full<-normalize(full,unit='16')
writeWave(full,file='sound.wav')
