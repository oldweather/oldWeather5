# Make the manifest CSV file for the Edwards 1943 images

log.files<-Sys.glob('/data/local/hadpb/oW5.uploads/War_in_the_Arctic/Aleutian_Campaign_and_WSF/Navy/Edwards_DD-619/Edwards-DD-619-1943/for_upload/*.jpg')

# Redirect ouput to the manifest file
sink("/data/local/hadpb/oW5.uploads/War_in_the_Arctic/Aleutian_Campaign_and_WSF/Navy/Edwards_DD-619/Edwards-DD-619-1943/for_upload/manifest.csv")
cat('subject_id,image_name_1,origin,group,subgroup,ship,year\n')
for(i in seq_along(log.files)) {
  cat(i,',',sep="")
  cat(basename(log.files[i]),',',sep="")
  cat('NARA,')
  cat('War_in_the_Arctic/Aleutian_Campaign_and_WSF,')
  cat('Navy,')
  cat('Edwards DD-619,')
  cat('1943\n')
}
sink() # Output back to default

  
  

