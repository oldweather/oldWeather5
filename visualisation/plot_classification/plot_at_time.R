# R script to plot the set of classifications active at a particular time

library(oldWeather5)

p.time<-ymd_hms('2015-12-03:16:51:56')
#p.time<-ymd_hms('2015-12-04:02:00:00')
classifications<-ReadClassifications('../../data-exports/classifications.csv',
                                     p.time-seconds(500),
                                     p.time+seconds(500))
subjects<-ReadSubjects('../../data-exports/subjects.csv')
classifications<-InterpolateTimestamps(classifications)
classifications<-SetIsTranscription(classifications)

c.set<-GetClassificationsByDate(p.time-seconds(30),p.time+seconds(30))

page.width<-1080*4/3
page.height<-1080

l1<-UpdateLayout(NULL,page.width,page.height,c.set)

png(filename='tst_by_date.png',width=page.width,height=page.height,
    bg='white',pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    grid.raster(background.img,width=unit(page.width,'native'),
                               height=unit(page.height,'native'))
    DrawLayout(l1,before=p.time)
    popViewport()
    DrawLabel(as.character(p.time))
    dev.off()
                
