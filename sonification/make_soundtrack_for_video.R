# Beep every time a classification occurs

library(oldWeather5)
library(tuneR)

# load some sounds
beep<-readWave('sounds/beep-01a.wav')
twang<-readWave('sounds/twang.wav')
cymbal<-readWave('sounds/cymbal.wav')
chime<-readWave('sounds/chime.wav')
keystroke<-readWave('sounds/keystroke.wav')

# Get the length of the video, and the re-layouts, from the png files.
files<-list.files(path=sprintf("%s/images/oW5.working/",Sys.getenv('SCRATCH')),
                  pattern='2015120316.*\\.png')
begin<-ymd_hms(substr(files[1],1,14))
end<-ymd_hms(substr(files[length(files)],1,14))
layout.files<-list.files(path=sprintf("%s/images/oW5.working/",
                             Sys.getenv('SCRATCH')),
                             pattern='2015120316.*_01\\.png')
layout.times<-ymd_hms(substr(layout.files,1,14))

framerate<-24 # Frames/s
layout.frames<-18 # No. of frames for a layout change.

film.length<-length(files)/framerate  # in s.

# Make a blank sound file covering the whole period
s.length<-film.length*beep@samp.rate
sound.track<-beep
sound.track@left<-rep(0,s.length)

# Put in a beep for every classification in the period covered
classifications<-ReadClassifications('../data-exports/classifications.csv')
#subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)

w<-GetClassificationsByDate(classifications,begin,end)
begin.n<-as.numeric(begin)
end.n<-as.numeric(end)
for(i in w) {
  for(n in seq_along(classifications$annotations[[i]])) {
    c.time<-classifications$annotations[[i]][[n]]$timestamp/1000
    if(c.time<begin.n || c.time>end.n) next
    s.start<-(c.time-begin.n)/framerate*sound.track@samp.rate
    # Adjust for the re-layouts
    ct<-which(layout.times<c.time)
    s.start<-s.start+length(ct)*(layout.frames/framerate)*sound.track@samp.rate
    # Different sounds for different sorts of data
    sample<-keystroke
    if(!is.null(classifications$annotations[[i]][[n]]$type)) {
    if(classifications$annotations[[i]][[n]]$type=='weather') sample<-cymbal
    if(classifications$annotations[[i]][[n]]$type=='sea-ice') sample<-cymbal
    if(classifications$annotations[[i]][[n]]$type=='date') sample<-chime
    if(classifications$annotations[[i]][[n]]$type=='location') sample<-chime
    if(classifications$annotations[[i]][[n]]$type=='events') sample<-twang
    if(classifications$annotations[[i]][[n]]$type=='mentions') sample<-twang
    if(classifications$annotations[[i]][[n]]$type=='refueling') sample<-twang
    if(classifications$annotations[[i]][[n]]$type=='animals') sample<-twang
    }
    s.end<-s.start+length(sample@left)-1
    if(s.end>length(sound.track@left)) {
        space<-length(sound.track@left)-s.start-1
        sample@left<-sample@left[1:space]
        s.end<-s.start+length(sample@left)-1
    }
    sound.track@left[s.start:s.end]<-sound.track@left[s.start:s.end]+sample@left
  }
}
sound.track<-normalize(sound.track,unit='16')
writeWave(sound.track,file='sound.wav')
system('lame -V2 sound.wav sound.mp3')
