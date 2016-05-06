# Make the manifest CSV file for the Unalga 1924 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Unalga-WPG-53-1924/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/The_Arctic_Frontier/Coast_Guard/Unalga-WPG-53-1924/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('The Arctic Frontier,')
  cat('Coast Guard,')
  cat('Unalga WPG 53,')
  cat('1924\n')
}
sink() # Output back to default

  
  

