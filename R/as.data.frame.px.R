#################################################################
# 
# File:         as.data.frame.px.R 
# Purpose:      extracts a df from a px object
#
# Created:      20110801
# Authors:      cjgb, opl, fvf
#
# Modifications: 
#    20120323, cjgb: added error check: variables and codes should have the same length
#    20120402, cjgb: warnings can be either errors or warnings depending on paranoia level
#    20120402, cjgb: adapted to the new px object format (where DATA is already a df)
#    20141222. fvf:  bug in "wide" direction
#################################################################

as.data.frame.px <- function( x, ..., use.codes = FALSE, warnings.as.errors = TRUE, direction = "long", language=FALSE){

  dat <- x$DATA$value  # already a data frame
  
  if ((!is.logical(language) && is.character(language))){
    if(language %in% x$LANGUAGES$value){
      use.language = paste0('.',language,'.')
      default.language = x$LANGUAGE
      codes.ids = match(names(x$CODES), colnames(dat))

      for(code_id in codes.ids)
        dat[[codes.ids[code_id]]] <- mapvalues(dat[[codes.ids[code_id]]], 
                                     from = x$VALUES[[code_id]], 
                                     to   = x[[paste0('VALUES', use.language)]][[code_id]])
      translated_colnames = names(x[[paste0('CODES', use.language)]])[codes.ids]
      translated_colnames = c(translated_colnames, 'value')
      colnames(dat) = translated_colnames
    
    } else {
      stop(paste0('Can\'t find the proposed language. Please choose one of the available languages: ', x$LANGUAGES$value))
    }
  } else {
    language = ''
  }



  ## maybe we need to change values to codes
  if (is.logical(use.codes) && use.codes)
    use.codes <- names(x$CODES)
  
  if (! is.logical(use.codes))
    for( var.name in intersect( use.codes, intersect(colnames(dat), names(x$CODES) ) ) )
        dat[[var.name]] <- mapvalues(dat[[var.name]], 
                                     from = x$VALUES[[var.name]], 
                                     to   = x$CODES[[var.name]])

     
  ## do we need to reshape?
  if (direction == "wide")
    # fvf.20121222: The order of variables rows and pivots-columns was inverted
    # dcast(dat, list(x$HEADING$value, x$STUB$value))
    dcast(dat, list(x$STUB$value,x$HEADING$value))
  else
    dat
}

