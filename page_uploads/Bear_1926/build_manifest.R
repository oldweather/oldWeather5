# Make the manifest CSV file for the Bear 1926 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Bear-1926/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Bear-1926/for_upload/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('The Arctic Frontier,')
  cat('Coast Guard,')
  cat('Bear,')
  cat('1926\n')
}
sink() # Output back to default

  
  

