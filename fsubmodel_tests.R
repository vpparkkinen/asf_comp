library(testthat)

shuffle <- function(x){
  l <- length(x)
  o <- sample(1:l, l, replace = FALSE)
  x[o]
}

recon_model <- function(x, add.error = FALSE){
  d <- decompose_model(x)
  o <- sample(1:length(d[[1]]), length(d[[1]]), replace = FALSE)
  l <- sample(1:length(d[[1]]), 1)
  o2 <- o[1:l]
  d2 <- lapply(d, function(x) x[o2])
  dis_l <- lit_extract(d2$lhss)
  dis_l_s <- lapply(dis_l, function(x) lapply(x, shuffle))     
  rec_dis <- lapply(dis_l_s,
                    function(x) lapply(x,
                                       function(z) paste0(z, collapse = "*")))
  rec_dis <- lapply(rec_dis, function(x) unlist(x, use.names = FALSE))
  #
  
  if(add.error){
    if(replace <- sample(c(TRUE, FALSE), 1)){
      lits <- lapply(dis_l_s, 
                     function(x) unique(unlist(x, use.names = FALSE, 
                                               recursive = FALSE)))
      wa <- sample(1:length(lits), 1)
      litr <- sample(lits[[wa]], 1)
      rec_dis[[wa]] <- sub(litr, "ERR", rec_dis[[wa]])
    } else if(add_dis <- sample(c(TRUE, FALSE), 1)){
      wa <- sample(1:length(rec_dis), 1)
      rec_dis[[wa]] <- shuffle(c("ERR", rec_dis[[wa]]))
    } else {
      wa <- sample(1:length(rec_dis), 1)
      rec_dis[[wa]] <- shuffle(c("ERR", rec_dis[[wa]]))
    }
  }
  rec_lhss <- lapply(rec_dis, function(x) paste0(x, collapse = "+"))
  if(length(rec_lhss) > 1){
    fd <- data.frame("(", unlist(rec_lhss), "<->", d2$rhss, ")")
    asfs <- do.call(paste0, fd)
    out <- paste0(asfs, collapse = "*")
  } else {
    out <- paste(unlist(rec_lhss), d2$rhss[[1]], sep = "<->")
  }
  return(out)  
}

set.seed(42)
targets <- replicate(1000, randomCsf(6))
cor_subs <- sapply(targets, recon_model, USE.NAMES = FALSE)
err_subs <- sapply(targets, 
                   function(x) recon_model(x, add.error = TRUE), 
                   USE.NAMES = FALSE)

res_is_submodel_cor <- vector(length = length(targets))
for(i in seq_along(targets)){
  res_is_submodel_cor[i] <- is.submodel(cor_subs[i], targets[i])
}
all(res_is_submodel_cor)  

res_fsubmodel_csf_cor <- vector(length = length(targets))
for(i in seq_along(targets)){
  res_fsubmodel_csf_cor[i] <- fsubmodel_csf(cor_subs[i], targets[i])
}
all(res_fsubmodel_csf_cor)

res_fsubmodel_csf_err <- vector(length = length(targets))
for(i in seq_along(targets)){
  res_fsubmodel_csf_err[i] <- fsubmodel_csf(err_subs[i], targets[i])
}
all(!res_fsubmodel_csf_err)

asf_targets <- replicate(5000, randomAsf(7))
cor_subs_asf <- sapply(asf_targets, recon_model, USE.NAMES = FALSE)
err_subs_asf <- sapply(asf_targets, 
                       function(x) recon_model(x, add.error = TRUE), 
                       USE.NAMES = FALSE)

res_is_submodel_cor_asf <- vector(length = length(targets))
for(i in seq_along(asf_targets)){
  res_is_submodel_cor_asf[i] <- is.submodel(cor_subs_asf[i], asf_targets[i])
}
all(res_is_submodel_cor_asf) 

res_fsubmodel_asf_cor <- vector(length = length(targets))
for(i in seq_along(asf_targets)){
  res_fsubmodel_asf_cor[i] <- fsubmodel_asf(cor_subs_asf[i], asf_targets[i])
}
all(res_fsubmodel_asf_cor)

res_fsubmodel_asf_cor <- vector(length = length(targets))
for(i in seq_along(asf_targets)){
  res_fsubmodel_asf_cor[i] <- fsubmodel_asf(cor_subs_asf[i], asf_targets[i])
}
all(res_fsubmodel_asf_cor)

res_fsubmodel_asf_err <- vector(length = length(targets))
for(i in seq_along(asf_targets)){
  res_fsubmodel_asf_err[i] <- fsubmodel_asf(err_subs_asf[i], asf_targets[i])
}
all(!res_fsubmodel_asf_err)


