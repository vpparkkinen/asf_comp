library(cna)


case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}

check_ccomp <- function(x,y){
  c_asfs <- unlist(extract_asf(x))
  if(length(c_asfs) == 1){
    out <- check_comp_asf(y,x)
    return(out)
  } 
  asfs_checked <- lapply(c_asfs, function(x) check_comp_asf(x,y))
  out <- all(unlist(asfs_checked))
  return(out)
}


check_comp_asf <- function(x,y){
  if(!is.inus(x, selectCases(y))){
   return(FALSE)
   }
  if(length(y)==0 | is.submodel(x,y)){
    return(TRUE)
  }
  tar_asfs <- unlist(extract_asf(y))
  tar_outs <- rhs(tar_asfs)
  tar_outs_flipped <- sapply(tar_outs, case_flipper)
  tar_lhss <- lhs(tar_asfs)
  cand_lhs <- lhs(x)
  cand_disjs <- unlist(strsplit(cand_lhs, "\\+"))
  cand_conjs <- unlist(strsplit(cand_disjs, "\\*"))
  
  cand_out <- rhs(x)
  outcome_match <- which(tar_outs==cand_out)
  
  ultimate_lhs <- tar_lhss[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))
  not_subbable <- vector("logical", length(tar_outs))
  if(!any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
                                                                "")))){
    return(FALSE)
  }
  while(any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
                                                     "")))){
    not_sub_ccon <- sapply(cand_conjs,
                           function(y) grepl(toupper(y), 
                                             toupper(ultimate_lhs)))
    conj_in_ultasf <- paste0(names(which(not_sub_ccon)))
    not_subbable <- lapply(tar_outs,
                           function(y)
                             grepl(toupper(y), toupper(conj_in_ultasf)))
    not_subbable <- unlist(lapply(not_subbable, any))
    sub_from_capmatch <- sapply(tar_outs, 
                                function(y) 
                                  grepl(y, ultimate_lhs))
    sub_from_capmatch[which(not_subbable)] <- FALSE
    sub_from_capflip <- sapply(tar_outs_flipped, 
                               function(y) 
                                 grepl(y, ultimate_lhs))
    sub_from_capflip[which(not_subbable)] <- FALSE
    # if(!any(c(sub_from_capmatch, sub_from_capflip))){
    #   return(FALSE)
    # }
    subbing_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip))) 
    idx_sub_from[subbing_from] <- TRUE
    #sub_alts_capmatch <- vector("character", length(which(sub_from_capmatch)))
    if(any(sub_from_capmatch)){
      for(i in which(sub_from_capmatch)){
        ultimate_lhs <- gsub(tar_outs[i],
                             paste0("(", tar_lhss[i], ")"),
                             ultimate_lhs)
      }  
    }
    
    #sub_alts_capflip <- vector("character", length(which(sub_from_capflip)))
    if(any(sub_from_capflip)){
      for(i in which(sub_from_capflip)){
        ultimate_lhs <- gsub(tar_outs_flipped[i], 
                            paste0("!(", tar_lhss[i], ")"),
                            ultimate_lhs)
      }  
    }
    #all_subbed <- paste0("(", c(sub_alts_capmatch, sub_alts_capflip), ")")
    #all_subbed <- paste0(all_subbed, collapse = "+")
    #ultimate_lhs <- all_subbed  
  }

  
  subbed_lhs <- rreduce(getCond(selectCases(ultimate_lhs)))
  subbed_asf <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  #idx_sub_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip)))
  new_tar_asfs <- tar_asfs
  new_tar_asfs[outcome_match] <- subbed_asf
  new_tar_asfs <- new_tar_asfs[!idx_sub_from]
  if(length(new_tar_asfs) > 1){
    new_tar_asfs <- paste0("(", new_tar_asfs, ")")
    rec_target <- paste0(new_tar_asfs, collapse = "*")
  } else {
    rec_target <- new_tar_asfs
  }
  out <- is.submodel(x, rec_target)
  attributes(out) <- NULL
  return(out)
}
























