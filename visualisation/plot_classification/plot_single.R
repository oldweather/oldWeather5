# R script to plot a single classification

library(oldWeather5)

classifications<-ReadClassifications('../../data-exports/classifications.csv')
subjects<-ReadSubjects('../../data-exports/subjects.csv')

page.width<-1080*4/3
page.height<-1080

png(filename='tst_single.png',width=page.width,height=page.height,pointsize=24)
    pushViewport(viewport(xscale=c(0,page.width),yscale=c(0,page.height)))
    grid.raster(background.img,width=unit(page.width,'native'),
                               height=unit(page.height,'native'))
    DrawClassification(10,page.width,page.height,
                        before=NULL)
    popViewport()
    DrawLabel('Test single classification (10)')
    dev.off()
                
