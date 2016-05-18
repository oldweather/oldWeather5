# Make the manifest CSV file for the Pennsylvania 1945 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/Distant_Seas/Navy/Pennsylvania_BB-38/Pennsylvania-BB-38-1945/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/Distant_Seas/Navy/Pennsylvania_BB-38/Pennsylvania-BB-38-1945/for_upload/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('Distant Seas,')
  cat('Navy,')
  cat('Pennsylvania BB-38,')
  cat('1945\n')
}
sink() # Output back to default

  
  

