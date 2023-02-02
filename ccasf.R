library(cna)


case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}

ccheck_prep <- function(x,y){
  tar_asfs <- unlist(extract_asf(y))
  tar_outs <- rhs(tar_asfs)
  tar_lhss <- lhs(tar_asfs)
  cand_asfs <- unlist(extract_asf(x))
  cand_outs <- rhs(cand_asfs)
  cand_lhss <- lhs(cand_asfs)
  names(cand_lhss) <- cand_outs
  cand_facs <- lit_extract(cand_lhss)
  not_subbable <- toupper(tar_outs) %in% toupper(cand_facs)
  test_tar_lhss <- tar_lhss
  names(test_tar_lhss) <- tar_outs
  out <- list(target_lhss = test_tar_lhss, 
              candidate_lhss = cand_lhss,
              candidate_asfs = cand_asfs,
              no_sub = not_subbable)
  return(out)
}


check_ccomp <- function(x,y){
  x <- noblanks(x)
  y <- noblanks(y)
  #cand_asfs <- unlist(extract_asf(x))
  out <- vector("logical", 1)
  attributes(out) <- list(why = "", 
                          x = x, 
                          y = y,
                          ultimate_asfs = NULL,
                          cand_asfs_checked = NULL)
  if(!is.inus(x, selectCases(y))){
    out[1] <- FALSE
    attr(out, "why") <- "x is not inus wrt selectCases(y)"
    return(out)
  }
  is_sm <- is.submodel(x,y)
  c_asfcount <- unlist(strsplit(x, "<->"))
  is_x_csf <- length(c_asfcount) > 2
  if(!is_x_csf & is_sm){
     out[1] <- TRUE
     attr(out, "why") <- "x is asf and a submodel of y"
     return(out)
  }
  if(is_x_csf & is_sm){
    out[1] <- NA
    attr(out, "why") <- "x is a csf and a submodel of y, requires further processing that I haven't coded yet"
    return(out)
  }
  prepared <- ccheck_prep(x,y)
  
  # if(length(prepared$candidate_lhss) == 1){
  #   out <- check_comp_asf(prepared$candidate_lhss, prepared$target_lhss)
  #   return(out)
  # }
  prep_target <- prepared$target_lhss
  asf_subms <- is.submodel(prepared$candidate_asfs, y)
  
  # subbed_tar_asfs <- sapply(prepared$candidate_lhss[!asf_subms], 
  #                        function(f) check_comp_asf(f, prep_target))
  
  subbed_tar_asfs <- vector("character", length(prepared$candidate_lhss[!asf_subms]))
  correct <- vector("logical", length(prepared$candidate_lhss[!asf_subms]))
  for(i in seq_along(prepared$candidate_lhss[!asf_subms])){
    subbed_tar_asfs[i] <- check_comp_asf(prepared$candidate_lhss[!asf_subms][i], 
                                         prepared$target_lhss,
                                         prepared$no_sub)
    correct[i] <- is.submodel(prepared$candidate_asfs[i], subbed_tar_asfs[i])
  }
  
  parts_correct <- c(correct, asf_subms[asf_subms])
  names(parts_correct) <- prepared$candidate_asfs
  out[1] <- all(parts_correct)
  if(out[1]){
    attr(out, "why") <- "all x asfs are submodels of expanded y asfs"
  } else {
    attr(out, "why") <- "some x asfs are not submodels of expanded y asfs"
  }
  attr(out, "ultimate_asfs") <- subbed_tar_asfs
  attr(out, "cand_asfs_checked") <- parts_correct
  return(out)
}


lit_extract <- function(lhs){
  d <- unlist(strsplit(lhs, "\\+"))
  out <- unlist(strsplit(d, "\\*"))
  return(out)
}

check_comp_asf <- function(x, y, not_subbable){
  # if(!is.inus(x, selectCases(y))){
  #  return(FALSE)
  #  }
  # if(is.submodel(names(x), ogy)){
  #   return(TRUE)
  # }
  # tar_asfs <- unlist(extract_asf(y))
  # tar_outs <- rhs(tar_asfs)
  # tar_outs_flipped <- sapply(tar_outs, case_flipper)
  # tar_lhss <- lhs(tar_asfs)
  # cand_lhs <- lhs(x)
  # cand_disjs <- unlist(strsplit(cand_lhs, "\\+"))
  # cand_conjs <- unlist(strsplit(cand_disjs, "\\*"))
  # 
  # cand_out <- rhs(x)
  # outcome_match <- which(tar_outs==cand_out)
  tar_lhss <- y
  tar_outs <- names(y)
  tar_outs_flipped <- sapply(tar_outs, case_flipper)
  cand_out <- names(x)
  outc_matches <- tar_outs %in% cand_out
  if(!any(outc_matches)){return}
  outcome_match <- which(tar_outs %in% cand_out)
  
  ultimate_lhs <- y[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))
  # if(is.null(not_subbable)){
  #   not_subbable <- vector("logical", length(tar_outs))
  # }
  #    
  # if(!any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
  #                                                               "")))){
  #   return(FALSE)
  # }
  
  # if(!any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
  #                                                               "")))){
  #   return(FALSE)
  # }
  
  while(any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
                                                     "")))){
    # not_sub_ccon <- sapply(cand_conjs,
    #                        function(y) grepl(toupper(y), 
    #                                          toupper(ultimate_lhs)))
    # conj_in_ultasf <- paste0(names(which(not_sub_ccon)))
    # not_subbable <- lapply(tar_outs,
    #                        function(y)
    #                          grepl(toupper(y), toupper(conj_in_ultasf)))
    # not_subbable <- unlist(lapply(not_subbable, any))
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
  #subbed_asf <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  out <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  #idx_sub_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip)))
  
  # new_tar_asfs <- tar_asfs
  # new_tar_asfs[outcome_match] <- subbed_asf
  # new_tar_asfs <- new_tar_asfs[!idx_sub_from]
  # if(length(new_tar_asfs) > 1){
  #   new_tar_asfs <- paste0("(", new_tar_asfs, ")")
  #   rec_target <- paste0(new_tar_asfs, collapse = "*")
  # } else {
  #   rec_target <- new_tar_asfs
  # }
  # out <- rec_target
  # out <- is.submodel(x, rec_target)
  # attributes(out) <- NULL
  return(out)
}
























