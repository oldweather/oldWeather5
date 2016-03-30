# Cut the Northland 1931 images into individual page files.

photos<-Sys.glob('/Volumes/My Passport/Shipment to UK 2015-06-23/The Arctic Frontier/Coast Guard/Northland (WPG-49)/Northland-WPG-49-1931-split/*')

uploads.dir<-'/Users/philip/LocalData/ow5.uploads/Northland-WPG-49-1931/'
if(!file.exists(uploads.dir)) dir.create(uploads.dir,recursive=TRUE)

p1.coords<-list(x=c(157,2541),y=c(103,3657))
p2.coords<-list(x=c(2542,4959),y=c(103,3657))

scale.factor<-1400/(3657-103)
quality.control<-'-strip -sampling-factor 4:2:0 -quality 50'

for(pic in photos) {
 p1.name<-sprintf("%s/%s_p1.jpg",uploads.dir,
      substr(basename(pic),1,nchar(basename(pic))-4))
 system(sprintf("convert -crop %dx%d+%d+%d -resize %dx%d %s '%s' %s",
      as.integer((p1.coords$x[2]-p1.coords$x[1])),
      as.integer((p1.coords$y[2]-p1.coords$y[1])),
      p1.coords$x[1],
      p1.coords$y[1],
      as.integer((p1.coords$x[2]-p1.coords$x[1])*scale.factor),
      as.integer((p1.coords$y[2]-p1.coords$y[1])*scale.factor),
      quality.control,
      pic,p1.name))
 p2.name<-sprintf("%s/%s_p2.jpg",uploads.dir,
      substr(basename(pic),1,nchar(basename(pic))-4))
 system(sprintf("convert -crop %dx%d+%d+%d -resize %dx%d %s '%s' %s",
      as.integer((p2.coords$x[2]-p2.coords$x[1])),
      as.integer((p2.coords$y[2]-p2.coords$y[1])),
      p2.coords$x[1],
      p2.coords$y[1],
      as.integer((p2.coords$x[2]-p2.coords$x[1])*scale.factor),
      as.integer((p2.coords$y[2]-p2.coords$y[1])*scale.factor),
      quality.control,
      pic,p2.name))
}

