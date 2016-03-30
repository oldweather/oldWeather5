# Make the manifest CSV file for the Northwing 1931 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Northland_WPG-49/Northland-WPG-49-1931-split/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Northland_WPG-49/Northland-WPG-49-1931-split/for_upload/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('The Arctic Frontier,')
  cat('Coast Guard,')
  cat('Northland WPG-49,')
  cat('1931\n')
}
sink() # Output back to default

  
  

