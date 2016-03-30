# Make the manifest CSV file for the Farragut 1942 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/Distant_Seas/Navy/Farragut_DD-348/Farragut-DD-348-1942-pt1/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/Distant_Seas/Navy/Farragut_DD-348/Farragut-DD-348-1942-pt1/for_upload/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('Distant Seas,')
  cat('Navy,')
  cat('Farragut DD-348,')
  cat('1942 pt. 1\n')
}
sink() # Output back to default

  
  

