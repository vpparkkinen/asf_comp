library(cna)
library(stringr)

case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}


decompose_model <- function(x){
  x <- noblanks(x)
  asfs <- unlist(extract_asf(x))
  rhss <- rhs(asfs)
  lhss <- lhs(asfs)
  #lhs_lits <- lit_extract(lhss)
  out <- list(asfs = asfs, 
              rhss = rhss, 
              lhss = lhss)
  return(out)
}


ccheck_prep <- function(x,y){
  tar_decomp <- decompose_model(y)
  cand_decomp <- decompose_model(x)
  tar_asfs <- tar_decomp$asfs
  tar_outs <- tar_decomp$rhss
  tar_lhss <- tar_decomp$lhss
  cand_asfs <- cand_decomp$asfs
  cand_outs <- cand_decomp$rhss
  cand_lhss <- cand_decomp$lhss
  names(cand_lhss) <- cand_outs
  cand_facs <- lit_extract(cand_lhss)
  #not_subbable <- toupper(tar_outs) %in% toupper(cand_facs)
  not_subbable <- toupper(tar_outs) %in% c(toupper(cand_facs), toupper(cand_outs))
  test_tar_lhss <- tar_lhss
  names(test_tar_lhss) <- tar_outs
  out <- list(target = y,
              target_lhss = test_tar_lhss,
              target_asfs = tar_asfs,
              candidate = x,
              candidate_lhss = cand_lhss,
              candidate_asfs = cand_asfs,
              no_sub = not_subbable)
  return(out)
}


is_compatible <- function(x,y){
  x <- noblanks(x)
  y <- noblanks(y)
  out <- vector("logical", 1)
  attributes(out) <- list(why = "", 
                          x = x, 
                          y = y,
                          ultimate_asfs = NULL,
                          cand_asfs_checked = NULL,
                          og_y = y,
                          og_x = x)
  # if(!is.inus(x, selectCases(y))){
  #   out[1] <- FALSE
  #   attr(out, "why") <- "x is not inus wrt selectCases(y)"
  #   return(out)
  # }
  is_sm <- is.submodel(x,y)
  c_asfcount <- unlist(strsplit(x, "<->"))
  is_x_csf <- length(c_asfcount) > 2
  if(!is_x_csf & is_sm){
     out[1] <- TRUE
     attr(out, "why") <- "x is asf and a submodel of y"
     return(out)
  }
   if(is_x_csf & is_sm){
     x <- is_comp_subtar(x)
     #out <- is_comp_subtar(x, y, out = out)
     # if(out[1]) {
     #   attr(out, "why") <- "x is a csf and a submodel of y, and x is causally compatible w/ y"
     # } else {
     #   attr(out, "why") <- "x is a csf and a submodel of y, but x is not causally compatible w/ y"
     # }
     attr(out, "why") <- "x is a csf and a submodel of y, substitute in x before checking compatibility"
     #return(out)
   }
  # prepared <- ccheck_prep(x,y)
  # prep_target <- prepared$target_lhss
  # asf_subms <- is.submodel(prepared$candidate_asfs, y)
  # subbed_tar_asfs <- vector("character", length(prepared$candidate_lhss[!asf_subms]))
  # correct <- vector("logical", length(prepared$candidate_lhss[!asf_subms]))
  # for(i in seq_along(prepared$candidate_lhss[!asf_subms])){
  #   subbed_tar_asfs[i] <- check_comp_asf(prepared$candidate_lhss[!asf_subms][i], 
  #                                        prepared$target_lhss,
  #                                        prepared$no_sub)
  #   correct[i] <- is.submodel(prepared$candidate_asfs[i], subbed_tar_asfs[i])
  # }
  # parts_correct <- c(correct, asf_subms[asf_subms])
  # names(parts_correct) <- prepared$candidate_asfs
  # out[1] <- all(parts_correct)
  # if(out[1]){
  #   attr(out, "why") <- "all x asfs are submodels of expanded y asfs"
  # } else {
  #   attr(out, "why") <- "some x asfs are not submodels of expanded y asfs"
  # }
  # attr(out, "ultimate_asfs") <- subbed_tar_asfs
  # attr(out, "cand_asfs_checked") <- parts_correct
  # return(out)
  out <- subin_target_ccomp(x,y,out)
  return(out)
}

subin_target_ccomp <- function(x, y, out){
  prepared <- ccheck_prep(x,y)
  prep_target <- prepared$target_lhss
  asf_subms <- is.submodel(prepared$candidate_asfs, y)
  cand_need_checking <- prepared$candidate_lhss[!asf_subms]
  subbed_tar_asfs <- vector("character", length(prepared$target_lhss))
  # sapply(names(prepared$target_lhss), 
  #        function(x) x %in% names(prepared$candidate_lhss))
  # #which_to_sub <- 
  #correct <- vector("logical", length(prepared$candidate_lhss[!asf_subms]))
  correct <- asf_subms
  # for(i in seq_along(prepared$candidate_lhss[!asf_subms])){
  #   subbed_tar_asfs[i] <- check_comp_asf(prepared$candidate_lhss[!asf_subms][i], 
  #                                        prepared$target_lhss,
  #                                        prepared$no_sub)
  #   # correct[i] <- is.submodel(prepared$candidate_asfs[!asf_subms][i], 
  #   #                           subbed_tar_asfs[i])
  #   asf_cor <- is.submodel(prepared$candidate_asfs[!asf_subms][i],
  #                          subbed_tar_asfs[i])
  #   correct[names(correct) == prepared$candidate_asfs[!asf_subms][i][i]] <- asf_cor
  # }
  for(i in seq_along(cand_need_checking)){
    subbed_tar_asfs[i] <- check_comp_asf(cand_need_checking[i], 
                                         prepared$target_lhss,
                                         prepared$no_sub,
                                         y)
    # correct[i] <- is.submodel(prepared$candidate_asfs[!asf_subms][i], 
    #                           subbed_tar_asfs[i])
    idx <- which(names(prepared$candidate_lhss) == names(cand_need_checking[i]))
    asf_cor <- is.submodel(prepared$candidate_asfs[idx],
                           subbed_tar_asfs[i])
    correct[names(correct) == prepared$candidate_asfs[idx]] <- asf_cor
  }
  attr(correct, "target") <- NULL
  #parts_correct <- c(asf_subms[asf_subms], correct)
  #names(parts_correct) <- prepared$candidate_asfs
  
  #out[1] <- all(parts_correct)
  out[1] <- all(correct)
  if(out[1]){
    attr(out, "why") <- "all x asfs are submodels of expanded y asfs"
  } else {
    attr(out, "why") <- "some x asfs are not submodels of expanded y asfs"
  }
  attr(out, "expanded_tar_asfs") <- subbed_tar_asfs
  attr(out, "cand_asfs_checked") <- correct
  return(out) 
}


mvdatgen <- function(x){
  fct <- full.ct(x)
  fct_max <- apply(fct, 2, max)
  mv_values <- lapply(fct_max, function(x) `:`(0L, x))
  out <- full.ct(cond = x, x = mv_values)
  return(out)
}

lit_extract <- function(lhs){
  d <- unlist(strsplit(lhs, "\\+"))
  out <- unlist(strsplit(d, "\\*"))
  return(out)
}


is_mv <- function(x){
  grepl("=", x)
}

check_comp_asf <- function(x, y, not_subbable, ogy, dat = full.ct(ogy)){
  if(grepl("=", ogy)){
    dat <- mvdatgen(ogy)
  }
  tar_lhss <- y
  tar_outs <- names(y)
  tar_outs_flipped <- sapply(tar_outs, case_flipper)
  cand_out <- names(x)
  outc_matches <- tar_outs %in% cand_out
  if(!any(outc_matches)){
    return(ogy)
  }
  outcome_match <- which(outc_matches)
  
  ultimate_lhs <- ultimate_lhs1 <- y[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))
  
  # while(any(toupper(tar_outs[!not_subbable]) %in% unlist(strsplit(toupper(ultimate_lhs), 
  #                                                    "")))){
  while(any(sapply(toupper(tar_outs[!not_subbable]), 
            function(x) grepl(x, toupper(ultimate_lhs))))){
    sub_from_capmatch <- sapply(tar_outs, 
                                function(y) 
                                  grepl(y, ultimate_lhs))
    sub_from_capmatch[which(not_subbable)] <- FALSE
    sub_from_capflip <- sapply(tar_outs_flipped, 
                               function(y) 
                                 grepl(y, ultimate_lhs))
    sub_from_capflip[which(not_subbable)] <- FALSE
    subbing_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip))) 
    idx_sub_from[subbing_from] <- TRUE
    if(any(sub_from_capmatch)){
      for(i in which(sub_from_capmatch)){
        ultimate_lhs <- gsub(tar_outs[i],
                             paste0("(", tar_lhss[i], ")"),
                             ultimate_lhs)
      }  
    }
    if(any(sub_from_capflip)){
      for(i in which(sub_from_capflip)){
        ultimate_lhs <- gsub(tar_outs_flipped[i], 
                            paste0("!(", tar_lhss[i], ")"),
                            ultimate_lhs)
      }  
    }
  }
  #ogy <- paste0(tar_lhss, "<->", names(tar_lhss)) #stupid hack must go
  #ogy <- paste0("(", ogy, ")")
  #ogy <- paste0(ogy, collapse = "*")
  if(identical(ultimate_lhs, ultimate_lhs1)){
    subbed_lhs <- ultimate_lhs1
  } else {
    u_lhsdat <- selectCases(cond = ultimate_lhs,
                            x = dat)
    cond <- getCond(u_lhsdat)
    subbed_lhs <- rreduce(cond, selectCases(ogy, x = dat), full = FALSE)  
  }
  # subbed_lhs <- rreduce(getCond(selectCases(ultimate_lhs)), #drop this if ultimate_lhs
  #                       selectCases(ogy), full = FALSE) # is identical to og ultimate_lhs
  out <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  return(out)
}


substitute_all <- function(x){
  x <- decompose_model(x)
  subbed <- chain_substituter(x)
  lhss <- subbed$lhss
  #dnf_lhss <- unlist(lapply(lhss, function(z) getCond(selectCases(z))))
  subbed$lhss <- unlist(lapply(lhss, function(z) getCond(selectCases(z))))
  #d <- data.frame(dnf_lhss, subbed$rhss)
  #new_asfs <- do.call("paste", c(d, sep = "<->"))
  #new_asfs <- paste0("(", new_asfs, ")")
  #new_csf <- paste0(new_asfs, collapse = "*")
  #return(new_csf)
  return(subbed)
}


chain_substituter <- function(x, 
                              subbed_from = vector("logical", length(x[[1]]))){
  sub_from_capmatch <- lapply(x$rhss, 
                              function(y) 
                                grepl(y, x$lhss))
  id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
  
  sub_from_capflip <- lapply(case_flipper(x$rhss), 
                             function(y) 
                               grepl(y, x$lhss))
  id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  if(!any(c(id_sub_capflip, id_sub_capmatch))){
    x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
    return(x)
  }
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
  chain_substituter(x, subbed_from = subbed_from)
}

# this one for the case where x is csf and submodel of y
# is_comp_subtar <- function(x, y, out){
is_comp_subtar <- function(x, y, out){  
  x_expanded <- substitute_all(x)
  # x_expanded$lhss <- unlist(lapply(x_expanded$lhss, 
  #               function(x) rreduce(getCond(selectCases(x)), 
  #                                   selectCases(y), full = FALSE)))
  
  x_expanded$lhss <- unlist(lapply(x_expanded$lhss, 
                                   function(x) rreduce(getCond(selectCases(x)))))
  x_expanded <- x_expanded[-1]
  # new_asfs <- vector("character", length(x_expanded[[1]]))
  # for(i in seq_along(x_expanded[[1]])){
  #   new_asfs[i] <- paste0("(", 
  #                         x_expanded$lhss[i], 
  #                         "<->",
  #                         x_expanded$rhss[i],
  #                         ")")
  #}
  d <- data.frame(x_expanded$lhss, x_expanded$rhss)
  new_asfs <- do.call("paste", c(d, sep = "<->"))
  if(length(new_asfs) > 1){
    new_asfs <- paste0("(", new_asfs, ")")
  } 
  new_csf <- paste0(new_asfs, collapse = "*")
  #out <- subin_target_ccomp(new_csf, y, out = out)
  #attr(out, "og_y") <- y
  #attr(out, "og_x") <- x 
  return(new_csf)
} 


















