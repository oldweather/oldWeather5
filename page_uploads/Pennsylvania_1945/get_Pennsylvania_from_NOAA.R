# Retrieve and Rename all vol3 high-resolution Farragut 1942 pt1 images from NARA

local.dir<-'/scratch/hadpb/oW5_logbooks/NARA/Pennsylvania_1945'
idx<-c(17306349,17306420,17306505,17306612,17306735,17306852,
       17306961,17307044,17307115,17307198,17307293,17307366)
for(month in seq(1,12)) {
base.url<-sprintf("https://catalog.archives.gov/OpaAPI/media/%d/content/dc-metro/rg-024/594258-ndl/594258-Batch00005/",idx[month])
    for(image in seq(1,100)) {
      image.url<-sprintf("%s/Pennsylvania-BB-38-1945-%02d/Pennsylvania-BB-38-1945-%02d-%04d.JPG?download=true",base.url,month,month,image)
      destination.file<-sprintf("%s/Pennsylvania-BB-38-1945-%02d-%04d.jpg",local.dir,month,image)
      if(file.exists(destination.file) &&
         file.info(destination.file)$size > 0) next
      download.file(image.url,destination.file,'wget')
    }
  }
