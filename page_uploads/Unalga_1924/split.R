# Cut the Faragut 1942 images into individual page files.

photos<-Sys.glob('/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Unalga-WPG-53-1924/raw_images/*')

uploads.dir<-'/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Unalga-WPG-53-1924/for_upload'
if(!file.exists(uploads.dir)) dir.create(uploads.dir,recursive=TRUE)

scale.factor<-1400/3744
quality.control<-'-strip -sampling-factor 4:2:0 -quality 50'

for(pic in photos) {
 p1.name<-sprintf("%s/%s.jpg",uploads.dir,
      substr(basename(pic),1,nchar(basename(pic))-4))
 system(sprintf("convert -resize %dx%d %s '%s' %s",
      as.integer(3182*scale.factor),
      as.integer(3744*scale.factor),
      quality.control,
      pic,p1.name))
}

