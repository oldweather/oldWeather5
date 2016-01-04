# R script to make a video

library(oldWeather5)

classifications<-ReadClassifications('../../data-exports/classifications.csv')
subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)

# Switch layouts without moving forward in time
SwitchLayout<-function(old.layout,new.layout,current,steps) {

  for(i in 1:steps) {
     fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_%02d.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current),i)
    png(filename=fn,width=page.width,height=page.height,bg=bg.colour,pointsize=24)
     pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
     i.layout<-InterpolateLayout(old.layout,new.layout,plogis((i-0.5)/steps,
                                                location = 0.5, scale = 0.1, log = FALSE))
    DrawLayout(classifications,subjects,i.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
     dev.off()
 }
   
}
    

# First test video
page.width<-1080*4/3
page.height<-1080
bg.colour<-rgb(243/255,236/255,226/255,1)

begin<-ymd_hms('2015-12-03 16:26:32')
end<-ymd_hms('2015-12-03 16:36:32')
step<-seconds(1)
current<-begin
current.layout<-NULL
while(current<end) {
    fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_00.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current))
    w<-GetClassificationsByDate(classifications,current-seconds(30),current+seconds(30))
    new.layout<-UpdateLayout(current.layout,page.width,page.height,w)
    if(!is.null(current.layout) && length(new.layout$contents)!=length(current.layout$contents)) {
        SwitchLayout(current.layout,new.layout,current-step,18)
    }
    current.layout<-new.layout
    
    png(filename=fn,width=page.width,height=page.height,bg=bg.colour,pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    DrawLayout(classifications,subjects,current.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
    dev.off()
                
    current<-current+step*2-step
}
                
