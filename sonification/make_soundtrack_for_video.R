# Beep every time a classification occurs

library(oldWeather5)
library(tuneR)

# load some sounds - all sampled at 24,000 hz
celesta<-list()
for(i in 1:27) {
  fn<-sprintf("sounds/ltw/celesta/c%03d.mp3",i)
  celesta[[i]]<-readMP3(fn)
}
clav<-list()
for(i in 1:27) {
  fn<-sprintf("sounds/ltw/clav/c%03d.mp3",i)
  clav[[i]]<-readMP3(fn)
}
beep<-readWave('sounds/beep-01a.wav')
beep<-downsample(beep,24000)
twang<-readWave('sounds/twang.wav')
twang<-downsample(twang,24000)
cymbal<-readWave('sounds/cymbal.wav')
cymbal<-downsample(cymbal,24000)
chime<-readWave('sounds/chime.wav')
chime<-downsample(chime,24000)
keystroke<-readWave('sounds/keystroke.wav')
keystroke<-downsample(keystroke,24000)

# Get the length of the video, and the re-layouts, from the png files.
files<-list.files(path=sprintf("%s/images/oW5.working/",Sys.getenv('SCRATCH')),
                  pattern='*\\.png')
begin<-ymd_hms(substr(files[1],1,14))
end<-ymd_hms(substr(files[length(files)],1,14))
layout.files<-list.files(path=sprintf("%s/images/oW5.working/",
                             Sys.getenv('SCRATCH')),
                             pattern='*_01\\.png')
layout.times<-ymd_hms(substr(layout.files,1,14))

framerate<-24 # Frames/s
layout.frames<-18 # No. of frames for a layout change.

film.length<-length(files)/framerate  # in s.

# Make a blank sound file covering the whole period
sound.track<-beep
sound.track@samp.rate<-24000
sound.track@left<-rep(0,film.length*sound.track@samp.rate)

# Put in a sound for every classification in the period covered
classifications<-ReadClassifications('../data-exports/classifications.csv')
classifications<-InterpolateTimestamps(classifications)
classifications<-SetIsTranscription(classifications)

w<-GetClassificationsByDate(begin,end)
begin.n<-as.numeric(begin)
end.n<-as.numeric(end)
for(i in w) {
  for(n in seq_along(classifications$annotations[[i]])) {
    ann<-classifications$annotations[[i]][[n]]
    c.time<-ann$timestamp/1000
    if(c.time<begin.n || c.time>end.n) next
    s.start<-(c.time-begin.n)/framerate*sound.track@samp.rate
    # Adjust for the re-layouts
    ct<-which(layout.times<c.time)
    s.start<-s.start+length(ct)*(layout.frames/framerate)*sound.track@samp.rate
    # Different sounds for different sorts of data
    if(classifications$meta$is_transcription[i]) {     
      sample<-keystroke
      if(is.null(ann$contents) || nchar(ann$contents)<1) next
      for(k in 1:nchar(ann$contents)) {
        s.end<-s.start+length(sample@left)-1
        if(s.end>length(sound.track@left)) break
        sound.track@left[s.start:s.end]<-sound.track@left[s.start:s.end]+
                                         sample@left
        s.start<-s.start+jitter(0.1,amount=0.02)*sound.track@samp.rate
      }
    } else {  # Annotation
        if(is.null(ann$type) || ann$type=='cell') next # auto-generated boxes
        y<-1000
        if(!is.null(ann$y)) y<-ann$y
        pitch.class<-as.integer(27*(2300-y)/2300)+1
        pitch.class<-min(pitch.class,27)
        sample<-celesta[[pitch.class]]
        if(!is.null(ann$type)&& (ann$type=='date' ||
                                 ann$type=='location' ||
                                 ann$type=='events' ||
                                 ann$type=='mentions' ||
                                 ann$type=='refueling' ||
                                 ann$type=='animals')) {
          sample<-clav[[pitch.class]]
        }
        s.end<-s.start+length(sample@left)-1
        if(s.end>length(sound.track@left)) {
            space<-length(sound.track@left)-s.start-1
            if(space<1) next
            sample@left<-sample@left[1:space]
            s.end<-s.start+length(sample@left)-1
          }
       sound.track@left[s.start:s.end]<-sound.track@left[s.start:s.end]+sample@left
      }
  }
}
sound.track<-normalize(sound.track,unit='16')
writeWave(sound.track,file='sound.wav')
system('lame -V2 sound.wav sound.mp3')
