chain_substituter2 <- function(x, 
                              subbed_from = vector("logical", length(x[[1]]))){
  sub_from_capmatch <- lapply(x$rhss, 
                              function(y) 
                                grepl(y, x$lhss))
  id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
  
  sub_from_capflip <- lapply(case_flipper(x$rhss), 
                             function(y) 
                               grepl(y, x$lhss))
  id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  while(any(c(id_sub_capflip, id_sub_capmatch)) & !same){
    for(i in seq_along(sub_from_capmatch)){
      if(id_sub_capmatch[i]){
        x$lhss[sub_from_capmatch[[i]]] <- gsub(x$rhss[i], 
                                               paste0("(", x$lhss[i], ")"), 
                                               x$lhss[sub_from_capmatch[[i]]])
      } 
    }
    for(i in seq_along(sub_from_capflip)){
      if(id_sub_capflip[i]){
        x$lhss[sub_from_capflip[[i]]] <- gsub(tolower(x$rhss[i]), 
                                              paste0("!(", x$lhss[i], ")"), 
                                              x$lhss[sub_from_capflip[[i]]])
      } 
    }
    
    wh_subbed_from <- which(subbed_from)
    wh_capmatch <- which(id_sub_capmatch)
    wh_capflip <- which(id_sub_capflip)
    w_subbed <- unique(c(wh_subbed_from, wh_capmatch, wh_capflip))
    subbed_from[w_subbed] <- TRUE
    sub_from_capmatch <- lapply(x$rhss, 
                                function(y) 
                                  grepl(y, x$lhss))
    id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
    
    sub_from_capflip <- lapply(case_flipper(x$rhss), 
                               function(y) 
                                 grepl(y, x$lhss))
    id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  }
  
  # chain_substituter(x, subbed_from = subbed_from)
  
  x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
  return(x)
}

