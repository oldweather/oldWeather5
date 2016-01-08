# R script to make a video

library(oldWeather5)
library(parallel)

#begin<-ymd_hms('2015-12-03 16:59:59')
begin<-ymd_hms('2015-12-03 16:25:59')
end<-ymd_hms('2015-12-03 20:59:41')

classifications<-ReadClassifications('../../data-exports/classifications.csv',
                                     start_date=begin-seconds(500),
                                     end_date=end+seconds(500))
subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)
classifications<-SetIsTranscription(classifications)

page.width<-1080*4/3
page.height<-1080
step<-seconds(1)
load('layouts.Rdata') # pre-calculated layouts

# Switch layouts without moving forward in time
SwitchLayout<-function(old.layout,new.layout,current,steps) {

  for(i in 1:steps) {
     fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_%02d.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current),i)
    if(file.exists(fn) && file.info(fn)$size>0) next
    png(filename=fn,width=page.width,height=page.height,pointsize=24)
     pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    grid.raster(background.img,width=unit(page.width,'native'),
                               height=unit(page.height,'native'))
     i.layout<-InterpolateLayout(old.layout,new.layout,plogis((i-0.5)/steps,
                                                location = 0.5, scale = 0.1, log = FALSE))
    DrawLayout(i.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
    dev.off()
 }
   
}

plot_current<-function(current) {
      fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_00.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current))
    if(file.exists(fn) && file.info(fn)$size>0) return()
    current.layout<-layouts[[as.character(current-step)]]
    new.layout<-layouts[[as.character(current)]]
    if(!is.null(current.layout) && length(new.layout$contents)!=length(current.layout$contents)) {
        SwitchLayout(current.layout,new.layout,current-step,18)
    }
    print(fn)
    png(filename=fn,width=page.width,height=page.height,pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    grid.raster(background.img,width=unit(page.width,'native'),
                               height=unit(page.height,'native'))
    DrawLayout(new.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
    dev.off()
    gc(verbose=FALSE)
}
    
# Dodge bizarre on-the-hour bug
increment.step<-function(current) {
  if(second(current)==59) current<-current+step*2-step
  else current<-current+step
  return(current)
}

# Make the time-points to render
steps<-list()
idx<-1
current<-increment.step(begin)
while(current<end) {

    steps[[idx]]<-current
    current<-increment.step(current)
      fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_00.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current))
    if(is.null(layouts[[as.character(current)]])) {
        w<-GetClassificationsByDate(current-seconds(60),current+seconds(60))
        layouts[[as.character(current)]] <- UpdateLayout(layouts[[as.character(current-step)]],
                                                         page.width,page.height,w)
      }
    if(file.exists(fn) && file.info(fn)$size>0) next
    idx<-idx+1
    if(idx>=60) {
      gc(verbose=FALSE)
      #mclapply(steps,plot_current,mc.cores=3,mc.preschedule=FALSE)
      lapply(steps,plot_current)
      steps<-list()
      idx<-1
      gc(verbose=FALSE)
    }
}
mclapply(steps,plot_current,mc.cores=6)

