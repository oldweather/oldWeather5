# R script to plot a set of classifications

library(oldWeather5)

classifications<-ReadClassifications('../../data-exports/classifications.csv')
subjects<-ReadSubjects('../../data-exports/subjects.csv')

page.width<-1080*4/3
page.height<-1080

c.set<-c(2,4,8,16,32)

l1<-UpdateLayout(NULL,page.width,page.height,c.set)

png(filename='tst_multiple.png',width=page.width,height=page.height,
    bg='white',pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    DrawLayout(classifications,subjects,l1,before=NULL)
    popViewport()
    DrawLabel(paste(c.set,collapse=' '))
    dev.off()
                