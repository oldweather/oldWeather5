#' Read in the oW5 classifications table
#'
#' Keeps the data internally in a data frame
#'
#'
#' @export
#' @param file file to read data from.
#' @return data frame - 1 row per record, column names as in the file.
ReadClassifications<-function(file) {
    l<-read.csv(file=file,as.is=TRUE)
    # string dates to POSIXt
    l$created_at <- ymd_hms(l$created_at)
    # unpack the metadata into a separate frame
    meta<-UnpackMeta(l$metadata)
    # same with subjects
    subject<-UnpackSubject(l$subject)
    # and annotations
    annotations<-UnpackAnnotation(l$annotations)
   return(list(core=l,meta=meta,subject=subject,annotations=annotations))
}
  
#' Make a data frame from the metadata classification strings.
#'
#' Uses the rjson library.
#'
#' Slow - explicit loops, not a problem with current data volumes.
#'
#' @param meta - Vector of JSON strings.
#' @return data frame, name::value pairs from the JSON
UnpackMeta<-function(meta) {
  len<-length(meta)
  result<-list(started_at=character(len),
               finished_at=character(len),
               user_agent=character(len),
               user_language=character(len),
               utc_offset=character(len),
               seen_before=logical(len),
               live_project=logical(len))
  for(n in names(result)) {
      is.na(result[[n]])<-TRUE
  }
  result$viewport<-list()
  result$subject_dimensions<-list()
  for(i in seq_along(meta)) {
      l<-fromJSON(meta[i])
      for(n in names(l)) {
          if(is.null(result[[n]])) {
              stop("Excess metadata element ",n," line ",i)
          }
          if(is.list(result[[n]])) {
              result[[n]][[i]]<-l[[n]]
          } else {
              result[[n]][i]<-l[[n]]
          }
      }
  }
  result$started_at <- ymd_hms(result$started_at)
  result$finished_at <- ymd_hms(result$finished_at)
  return(result)
}
    
#' Make a data frame from the subject classification strings.
#'
#' Uses the rjson library.
#'
#' Slow - explicit loops, not a problem with current data volumes.
#'
#' @param subject - Vector of JSON strings.
#' @return data frame, name::value pairs from the JSON
UnpackSubject<-function(subject) {
  len<-length(subject)
  result<-list(number=character(len),
               retired=logical(len),
               id=character(len),
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
  for(i in seq_along(subject)) {
      l<-fromJSON(subject[i])
      result$number[i]=names(l)
      for(n in names(l[[result$number[i]]])) {
          if(is.null(result[[n]])) {
              stop("Excess metadata element ",n," line ",i)
          }
          if(!is.null(l[[result$number[i]]][[n]])) {
              result[[n]][i]<-l[[result$number[i]]][[n]]
          }
      }
  }
  result$number <- as.integer(result$number)
  result$id <- as.integer(result$id)
  result$year <- as.integer(result$year)
  result$pageNumber <- as.integer(result$pageNumber)
  return(result)
}
     
#' Make a data frame from the annotations classification strings.
#'
#' Uses the rjson library.
#'
#' Slow - explicit loops, not a problem with current data volumes.
#'
#' @param annotation - Vector of JSON strings.
#' @return data frame, name::value pairs from the JSON
UnpackAnnotation<-function(annotation) {
  len<-length(annotation)
  result<-list()
  for(i in seq_along(annotation)) {
      result[[i]]<-fromJSON(annotation[i])
  }
  return(result)
}
