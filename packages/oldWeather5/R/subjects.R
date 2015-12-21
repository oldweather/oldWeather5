#' Read in the oW5 subjects table
#'
#' Keeps the data internally in a data frame
#'
#' @export
#' @param file file to read data from.
#' @return list of data frames.
ReadSubjects<-function(file) {
    l<-read.csv(file=file,as.is=TRUE)
    # unpack the metadata into a separate frame
    meta<-UnpackSMeta(l$metadata)
    # same with locations
    locations<-UnpackSLocation(l$locations)
    # and classification counts
    classifications<-UnpackSLocation(l$classifications_by_workflow)
   return(list(core=l,meta=meta,locations=locations,classifications=classifications))
}
     
#' Make a data frame from the subject metadata strings.
#'
#' Uses the rjson library.
#'
#' Slow - explicit loops, not a problem with current data volumes.
#'
#' @param subject - Vector of JSON strings.
#' @return data frame, name::value pairs from the JSON
UnpackSMeta<-function(meta) {
  len<-length(meta)
  result<-list(id=character(len),
               year=character(len),
               group=character(len),
               theme=character(len),
               subgroup=character(len),
               image=character(len),
               vessel=character(len),
               pageNumber=character(len))
  for(n in names(result)) {
      is.na(result[[n]])<-TRUE
  }
  for(i in seq_along(meta)) {
      l<-rjson::fromJSON(meta[i])
      for(n in names(l)) {
          if(is.null(result[[n]])) {
              stop("Excess metadata element ",n," line ",i)
          }
          if(!is.null(l[[n]])) {
              result[[n]][i]<-l[[n]]
          }
      }
  }
  result$id <- as.integer(result$id)
  result$year <- as.integer(result$year)
  result$pageNumber <- as.integer(result$pageNumber)
  return(result)
}
     
#' Make a data frame from the annotations location strings.
#'
#' Uses the rjson library.
#'
#' Slow - explicit loops, not a problem with current data volumes.
#'
#' @param location - Vector of JSON strings.
#' @return data frame, name::value pairs from the JSON
UnpackSLocation<-function(location) {
  len<-length(location)
  result<-list()
  for(i in seq_along(location)) {
      result[[i]]<-rjson::fromJSON(location[i])
  }
  return(result)
}
