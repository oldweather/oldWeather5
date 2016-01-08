# Just make the layouts for the video - non-parellelisable

library(oldWeather5)

classifications<-ReadClassifications('../../data-exports/classifications.csv')
subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)

# Dodge bizarre on-the-hour bug
increment.step<-function(current) {
  if(second(current)==59) current<-current+step*2-step
  else current<-current+step
  return(current)
}

layouts<-list()

page.width<-1080*4/3
page.height<-1080

begin<-ymd_hms('2015-12-03 16:25:59')
end<-ymd_hms('2015-12-03 20:25:00')
step<-seconds(1)
current<-begin
current.layout<-NULL
while(current<end) {
    fn<-sprintf("%s/images/oW5.working/%04d%02d%02d%02d%02d%02d_00.png",
                Sys.getenv('SCRATCH'),
                year(current),month(current),day(current),hour(current),
                minute(current),second(current))
    w<-GetClassificationsByDate(current-seconds(60),current+seconds(60))
    new.layout<-UpdateLayout(current.layout,page.width,page.height,w)
    layouts[[as.character(current)]]<-new.layout
    current.layout<-new.layout
                    
    current<-increment.step(current)
    print(current)
}
                
save(layouts,file='layouts.Rdata')
