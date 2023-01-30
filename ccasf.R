library(cna)


case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}


check_comp_asf <- function(x,b){
  if(length(x)==0 | is.submodel(b,x)){
    return(TRUE)
  }
  tar_asfs <- unlist(extract_asf(x))
  tar_outs <- rhs(tar_asfs)
  tar_outs_flipped <- sapply(tar_outs, case_flipper)
  tar_lhss <- lhs(tar_asfs)
  cand_lhs <- lhs(b)
  cand_disjs <- unlist(strsplit(cand_lhs, "+"))
  cand_disjs <- cand_disjs[-which(cand_disjs %in% c("+","*"))]
  cand_conjs <- unlist(strsplit(cand_disjs, ""))
  cand_out <- rhs(b)
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
                           function(x) grepl(toupper(x), 
                                             toupper(ultimate_lhs)))
    conj_in_ultasf <- paste0(names(which(not_sub_ccon)))
    not_subbable <- lapply(tar_outs,
                           function(x)
                             grepl(toupper(x), toupper(conj_in_ultasf)))
    not_subbable <- unlist(lapply(not_subbable, any))
    sub_from_capmatch <- sapply(tar_outs, 
                                function(x) 
                                  grepl(x, ultimate_lhs))
    sub_from_capmatch[which(not_subbable)] <- FALSE
    sub_from_capflip <- sapply(tar_outs_flipped, 
                               function(x) 
                                 grepl(x, ultimate_lhs))
    sub_from_capflip[which(not_subbable)] <- FALSE
    if(!any(c(sub_from_capmatch, sub_from_capflip))){
      return(FALSE)
    }
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
  check_comp_asf(rec_target, b)
}
























