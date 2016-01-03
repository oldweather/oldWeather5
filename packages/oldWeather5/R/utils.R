# Utility functions for oW5 data.

#' Interpolate timestamps where needed
#'
#' Some annotations are missing a timestamp - provide them by interpolation
#'
#' Gap between timestamps is smaller of 2 seconds and interpolation between
#' previous and next.
#'
#' @export
#' @param classifications list from \code{\link{readClassifications}}.
#' @return modified list with timestamps for all annotations.
InterpolateTimestamps<-function(classifications) {
    for(i in seq_along(classifications$annotations)) {
         has.ts<-rep(NA,length(classifications$annotations[[i]]))
         for(n in seq_along(classifications$annotations[[i]])) {
             if(!is.null(classifications$annotations[[i]][[n]]$timestamp)) {
                 has.ts[n]<-classifications$annotations[[i]][[n]]$timestamp
             }
         }
         g<-which(!is.na(has.ts))
         if(length(g)==0) {
             classifications$annotations[[i]][[1]]$timestamp<-as.numeric(classifications$meta$started_at[i])*1000
             has.ts[1]<-classifications$annotations[[i]][[1]]$timestamp
             g<-which(!is.na(has.ts))
         }
         if(length(which(is.na(has.ts)))>0) {
             w<-which(is.na(has.ts))
             for(n in w) {
                 if(n<min(g)) {
                     m<-min(g)
                     classifications$annotations[[i]][[n]]$timestamp <-
                         has.ts[m]-(m-n)*2000
                 }
                 if(n>max(g)) {
                     m<-max(g)
                     classifications$annotations[[i]][[n]]$timestamp <-
                         has.ts[m]+(n-m)*2000
                 }
                 if(n<max(g) && n>min(g)) {
                     below<-max(g[g<n])
                     above<-min(g[g>n])
                     increment<-((n-below)/(above-below))*
                         (has.ts[above]-has.ts[below])
                     if(abs(increment)>(n-below)*2000) increment<-(n-below)*2000
                     classifications$annotations[[i]][[n]]$timestamp <-
                         has.ts[below]+increment
                 }
             }
         }
     }
    return(classifications)
}

#' Make some more plausible start and end dates
#'
#' Some classifications have a very long gap between
#' start and end dates - estimate better start and end dates
#' from the annotation timestamps.
#'
#' @export
#' @param classifications list from \code{\link{readClassifications}}.
#' @return modified list with added $meta$new_started_at and $meta$new_finished_at components.
FixStartAndFinish<-function(classifications) {
    classifications$meta$new_started_at<-rep(NA,length(classifications$meta$started_at))
    classifications$meta$new_finished_at<-rep(NA,length(classifications$meta$finished_at))
     for(i in seq_along(classifications$annotations)) {
         for(n in seq_along(classifications$annotations[[i]])) {
             if(!is.null(classifications$annotations[[i]][[n]]$timestamp)) {
                 nts<-classifications$annotations[[i]][[n]]$timestamp
                 if(is.na(classifications$meta$new_started_at[i]) ||
                    classifications$meta$new_started_at[i]>nts) classifications$meta$new_started_at[i]<-nts
                 if(is.na(classifications$meta$new_finished_at[i]) ||
                    classifications$meta$new_finished_at[i]<nts) classifications$meta$new_finished_at[i]<-nts
            }
         }
     }
     classifications$meta$new_started_at<-as.POSIXct(classifications$meta$new_started_at/1000,
                                 origin='1970-01-01:00:00:00')
     classifications$meta$new_finished_at<-as.POSIXct(classifications$meta$new_finished_at/1000,
                                 origin='1970-01-01:00:00:00')
     return(classifications)
 }

#' Get all the types of annotation
#'
#' Get the names of each type of classification in the database.
#'
#' @export
#' @param classifications list from \code{\link{readClassifications}}.
#' @return names for each classification type.
FindTypes<-function(classifications) {
    types<-list()
    for(i in seq_along(classifications$annotations)) {
        for(n in seq_along(classifications$annotations[[i]])) {
            if(!is.null(classifications$annotations[[i]][[n]]$type) &&
               is.null(types[[classifications$annotations[[i]][[n]]$type]])) {
                types[[classifications$annotations[[i]][[n]]$type]] <- 1
            }
        }
    }
    return(names(types))
}

#' Get classifications by date
#'
#' Extract a subset of the classifications active between two given dates.
#'
#' Optionally check that an annotation occurred between the two dates (slow).
#'
#' @export
#' @param classifications list from \code{\link{readClassifications}}.
#' @param start_date POSIXt date for selection period start
#' @param end_date POSIXt date for selection period end
#' @param strict Boolean, if TRUE (default) check an annotation occured in the range. 
#' @return vector of indices of classifications in the selected date range.
GetClassificationsByDate<-function(classifications,start_date,end_date,strict=TRUE) {
    w<-which(classifications$meta$started_at<=end_date &
             classifications$meta$finished_at>start_date)
    if(strict) {
       w2<-integer(0)
      for(i in w) {
          for(n in seq_along(classifications$annotations[[i]])) {
              if(!is.null(classifications$annotations[[i]][[n]]$timestamp)) {
                  ts <- as.POSIXct(classifications$annotations[[i]][[n]]$timestamp/1000,
                                   origin='1970-01-01:00:00:00')
                  if(ts>start_date && ts<=end_date) {
                      w2<-c(w2,i)
                      break
                  }
              }
          }
      }
      w<-w2
   }
    return(w)
}

#' Get a raster image of a selected page
#'
#' Download the image from the website (unless it's already cached).
#'
#' @export
#' @param subjects list from \code{\link{readSubjects}}.
#' @param i index of the selected subject
#' @return raster of the image.
GetPageImage<-function(subjects,i) {
  if(Sys.getenv('SCRATCH')=="") stop("Unspecified SCRATCH directory")
  cache.dir<-sprintf("%s/oW5.cache/",Sys.getenv('SCRATCH'))
  if(!file.exists(cache.dir)) dir.create(cache.dir,recursive=TRUE)
  local.name<-subjects$meta$image[[i]]
  url<-subjects$locations[[i]][['0']]
  local.filename<-sprintf("%s/%s",cache.dir,local.name)
  if(file.exists(local.filename) && file.info(local.filename)$size>0) {
    img<-readJPEG(local.filename,native=FALSE)
    return(img)
  }
  res<-download.file(url,local.filename,method='wget',quiet=TRUE)
  if(res!=0) stop(sprintf("Failed download of %s to $s",url,local.filename))
  img<-readJPEG(local.filename,native=FALSE)
  return(img)
}

#' Mock a raster image of a selected page
#'
#' Fore use when the actual image is unavailable.
#'
#' @export
#' @return mock raster.
MissingPageImage<-function() {
    ncol<-1600
    nrow<-2300
    colour<-rgb(0.79,0.82,0.72,1)
    img<-matrix(rep(colour,ncol*nrow), ncol=ncol, byrow=F)
    return(img)
}
