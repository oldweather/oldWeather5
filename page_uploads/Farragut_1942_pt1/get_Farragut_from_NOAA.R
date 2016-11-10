# Retrieve and Rename all vol3 high-resolution Farragut 1942 pt1 images from NARA

local.dir<-'/scratch/hadpb/oW5_logbooks/NARA/Farragut_1942_pt1'
for(month in seq(1,12)) {
    idx<-7795038+month
base.url<-sprintf("https://catalog.archives.gov/OpaAPI/media/%d/content/arcmedia/dc-metro/594258-navy-deck-logs/batch-y",idx)
    for(image in seq(1,100)) {
      image.url<-sprintf("%s/Farragut-DD-348-1942-%02d/Farragut-DD-348-1942-%02d-%04d.jpg?download=true",base.url,month,month,image)
      destination.file<-sprintf("%s/Farragut-DD-348-1942-%02d-%04d.jpg",local.dir,month,image)
      if(file.exists(destination.file) &&
         file.info(destination.file)$size > 0) next
      download.file(image.url,destination.file,'wget')
    }
  }
