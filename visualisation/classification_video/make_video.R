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
    png(filename=fn,width=page.width,height=page.height,bg='white',,pointsize=24)
     pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
     i.layout<-InterpolateLayout(old.layout,new.layout,(i-0.5)/steps)
    DrawLayout(classifications,subjects,i.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
     dev.off()
 }
   
}
    

# First test video
page.width<-1080*4/3
page.height<-1080

begin<-ymd_hms('2015-12-04 02:30:00')
end<-ymd_hms('2015-12-04 02:32:00')
current<-begin
current.layout<-NULL
while(current<end) {
    fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_00.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current))
    w<-GetClassificationsByDate(classifications,current-seconds(120),current+seconds(120))
    new.layout<-UpdateLayout(current.layout,w)
    if(length(new.layout$contents)!=current.layout$contents) {
        SwitchLayout(old.layout,new.layout,current,12)
    }
    current.layout<-new.layout
    
    png(filename=fn,width=page.width,height=page.height,bg='white',,pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    DrawLayout(classifications,subjects,current.layout,before=current)
    popViewport()
    DrawLabel(as.character(current))
    dev.off()
                
    current<-current+seconds(1)
}
                
