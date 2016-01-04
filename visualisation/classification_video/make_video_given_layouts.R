# R script to make a video

library(oldWeather5)
library(parallel)

classifications<-ReadClassifications('../../data-exports/classifications.csv')
subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)

page.width<-1080*4/3
page.height<-1080
begin<-ymd_hms('2015-12-03 16:25:00')
end<-ymd_hms('2015-12-03 16:44:41')
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
    png(filename=fn,width=page.width,height=page.height,bg='white',,pointsize=24)
     pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
     i.layout<-InterpolateLayout(old.layout,new.layout,plogis((i-0.5)/steps,
                                                location = 0.5, scale = 0.1, log = FALSE))
    DrawLayout(classifications,subjects,i.layout,before=current)
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
    current.layout<-layouts[[as.character(current-step)]]
    new.layout<-layouts[[as.character(current)]]
    if(!is.null(current.layout) && length(new.layout$contents)!=length(current.layout$contents)) {
        SwitchLayout(current.layout,new.layout,current-step,18)
    }
    if(file.exists(fn) && file.info(fn)$size>0) return()
    png(filename=fn,width=page.width,height=page.height,bg='white',,pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    DrawLayout(classifications,subjects,new.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
    dev.off()
}
    
# Make the time-points to render

steps<-list()
idx<-1
current<-begin+step
while(current<end) {

    steps[[idx]]<-current
    current<-current+step
    idx<-idx+1
}

# Do the rendering
lapply(steps,plot_current)
#mclapply(steps,plot_current,mc.cores=6)