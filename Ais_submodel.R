Ais_submodel <- function(x, y){
  #xy <- unique(c(x, y))
  #px <- lapply(xy, tryparse)
  #px <- lapply(xy, cna:::tryparse)
  # ok <- !vapply(px, is.null, logical(1))
  # ct_type <- if (any(grepl("=", xy, fixed = T))) 
  #   "mv"
  # else "cs"
  # if (ct_type == "cs") {
  #   vals <- unique.default(unlist(lapply(px, all.vars)))
  # } else {
    # vals <- rapply(px, .call2list, how = "unlist", stopOps = c("==",
    #                                                            "<", ">", "<=", ">="), validOps = c("<-", "<<-",
    #                                                                                                "=", "&", "|", "(", "-"))
    # vals <- unique.default(vals[!vapply(vals, is.symbol,
    #                                     FUN.VALUE = TRUE)])
    # vals <- sub(" == ", "=", vapply(vals, deparse, character(1)))
  # }
  # cond_type <- .qcondType(xy, values = vals, ct_type = ct_type, 
  #                         stdComplex.multiple.only = FALSE)
  # ok <- ok & cond_type %in% c("stdAtomic", "stdComplex")
  # if (any(!ok)) 
  #   stop("Invalid input to is.submodel:\n", paste0("  ", 
  #                                                  xy[!ok], collapse = "\n"), call. = FALSE)
  # yy <- extract_asf(y)[[1]]
  # xx <- extract_asf(x)
  yy <- y
  xx <- x
  lhsy <- lhs(yy)
  rhsy <- rhs(yy)
  # if (!all(nzchar(c(lhsy, rhsy))) || length(lhsy) != length(rhsy) || 
  #     length(lhsy) <= 0) 
  #   stop("Check the structure of y: should be a asf or csf.")
  lhsy <- setNames(lhs(yy), rhs(yy))
  # if (length(x) == 0) 
  #   return(logical(0))
  ux <- unlist(xx)
  llx <- lengths(xx)
  lux <- lhs(ux)
  rux <- rhs(ux)
  # if (!all(nzchar(c(lux, rux))) || length(lux) != length(rux) || 
  #     length(lux) <= 0) 
  #   stop("Check the structure of x: should contain asf or csf.")
  clx <- hstrsplit(lux, c("+", "*"), relist = FALSE)
  cly <- hstrsplit(lhsy, c("+", "*"), relist = FALSE)
  nms <- unique.default(c(clx, cly))
  ilx <- match(clx, table = nms)
  lx2 <- attr(clx, "lengths")[[2]]
  ilx <- ilx[order(rep(seq_along(lx2), lx2), ilx)]
  #rilx <- hrelist(ilx, attr(clx, "lengths"), relist = FALSE)
  rilx <- cna:::hrelist(ilx, attr(clx, "lengths"), relist = FALSE)
  ily <- match(cly, table = nms)
  ly2 <- attr(cly, "lengths")[[2]]
  ily <- ily[order(rep(seq_along(ly2), ly2), ily)]
  #rily <- hrelist(ily, attr(cly, "lengths"), relist = FALSE)
  rily <- cna:::hrelist(ily, attr(cly, "lengths"), relist = FALSE)
  names(rily) <- names(lhsy)
  ok <- rep(NA, sum(lengths(xx)))
  # for (r in unique(rux)) {
  #   ok[rux == r] <- C_is_submodel(rilx[rux == r], rily[[r]], 
  #                                 strict = FALSE)
  # }
  for (r in unique(rux)) {
    ok[rux == r] <- cna:::C_is_submodel(rilx[rux == r], rily[[r]], 
                                  strict = FALSE)
  }
  #out <- m_all(C_relist_Log(ok, llx))
  out <- vapply(cna:::C_relist_Log(ok, llx), all, logical(1), na.rm = FALSE, USE.NAMES = FALSE)
  # if (strict && any(out)) {
  #   ok <- rep(NA, sum(lengths(xx)))
  #   for (r in unique(rux)) {
  #     ok[rux == r] <- vapply(rilx[rux == r], intList_equal, 
  #                            rily[[r]], FUN.VALUE = logical(1))
  #   }
  #   out <- out & !m_all(C_relist_Log(ok, llx))
  # }
  names(out) <- x
  attr(out, "target") <- y
  out
}

x <- "A*B+C*d*E+X*y*A+R<->Z"
y <- "A*B*g*H+C*d*E*U+X*y*A*U2+R+U3<->Z"
Ais_submodel(x,y)
x <- "A=1+B=2<->C=2"
y <- "A=1*Y=2+B=1+Z=3<->C=2"
Ais_submodel(x,y)
