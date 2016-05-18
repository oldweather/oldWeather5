# Cut the Faragut 1942 images into individual page files.

photos<-Sys.glob('/data/local/hadpb/oW5.uploads/War_in_the_Arctic/Aleutian_Campaign_and_WSF/Navy/Edwards_DD-619/Edwards-DD-619-1943/raw_images/*')

uploads.dir<-'/data/local/hadpb/oW5.uploads/War_in_the_Arctic/Aleutian_Campaign_and_WSF/Navy/Edwards_DD-619/Edwards-DD-619-1943/for_upload'
if(!file.exists(uploads.dir)) dir.create(uploads.dir,recursive=TRUE)

scale.factor<-1400/5616
quality.control<-'-strip -sampling-factor 4:2:0 -quality 50'

for(pic in photos) {
 p1.name<-sprintf("%s/%s.jpg",uploads.dir,
      substr(basename(pic),1,nchar(basename(pic))-4))
 system(sprintf("convert -resize %dx%d %s '%s' %s",
      as.integer(3744*scale.factor),
      as.integer(5616*scale.factor),
      quality.control,
      pic,p1.name))
}

