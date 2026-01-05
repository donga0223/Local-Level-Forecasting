termplot <- function (model, data = NULL, envir = environment(formula(model)), 
                      partial.resid = FALSE, rug = FALSE, terms = NULL, se = FALSE, 
                      xlabs = NULL, ylabs = NULL, main = NULL, col.term = 2, lwd.term = 2, 
                      col.se = "orange", lty.se = 2, lwd.se = 1.5, col.res = adjustcolor("black", alpha.f = 0.7), 
                      cex = 1, pch = 16, col.smth = "darkred", lty.smth = 2, 
                      span.smth = 1, ask = dev.interactive() && nb.fig < n.tms, 
                      use.factor.levels = TRUE, smooth = NULL, ylim = "common", 
                      plot = TRUE, transform.x = FALSE, ...) 
{
  which.terms <- terms
  terms <- if (is.null(terms)) 
    predict(model, type = "terms", se.fit = se)
  else predict(model, type = "terms", se.fit = se, terms = terms)
  n.tms <- ncol(tms <- as.matrix(if (se) 
    terms$fit
    else terms))
  transform.x <- rep_len(transform.x, n.tms)
  mf <- model.frame(model)
  if (is.null(data)) 
    data <- eval(model$call$data, envir)
  if (is.null(data)) 
    data <- mf
  use.rows <- if (NROW(tms) < NROW(data)) 
    match(rownames(tms), rownames(data))
  nmt <- colnames(tms)
  if (any(grepl(":", nmt, fixed = TRUE))) 
    warning("'model' appears to involve interactions: see the help page", 
            domain = NA, immediate. = TRUE)
  cn <- str2expression(nmt)
  if (!is.null(smooth)) 
    smooth <- match.fun(smooth)
  if (is.null(ylabs)) 
    ylabs <- paste("Partial for", nmt)
  if (is.null(main)) 
    main <- ""
  else if (is.logical(main)) 
    main <- if (main) 
      deparse(model$call, 500)
  else ""
  else if (!is.character(main)) 
    stop("'main' must be TRUE, FALSE, NULL or character (vector).")
  main <- rep_len(main, n.tms)
  pf <- envir
  carrier <- function(term, transform) {
    if (length(term) > 1L) {
      if (transform) 
        tms[, i]
      else carrier(term[[2L]], transform)
    }
    else eval(term, data, enclos = pf)
  }
  carrier.name <- function(term) {
    if (length(term) > 1L) 
      carrier.name(term[[2L]])
    else as.character(term)
  }
  in.mf <- nmt %in% names(mf)
  is.fac <- sapply(nmt, function(i) i %in% names(mf) && is.factor(mf[, 
                                                                     i]))
  if (!plot) {
    outlist <- vector("list", sum(in.mf))
    for (i in 1L:n.tms) {
      if (!in.mf[i]) 
        next
      if (is.fac[i]) {
        xx <- mf[, nmt[i]]
        if (!is.null(use.rows)) 
          xx <- xx[use.rows]
        ww <- match(levels(xx), xx, nomatch = 0L)
      }
      else {
        xx <- carrier(cn[[i]], transform.x[i])
        if (!is.null(use.rows)) 
          xx <- xx[use.rows]
        ww <- match(sort(unique(xx)), xx)
      }
      outlist[[i]] <- if (se) 
        data.frame(x = xx[ww], y = tms[ww, i], se = terms$se.fit[ww, 
                                                                 i], row.names = NULL)
      else data.frame(x = xx[ww], y = tms[ww, i], row.names = NULL)
    }
    attr(outlist, "constant") <- attr(terms, "constant")
    if (se && is.null(attr(outlist, "constant"))) 
      attr(outlist, "constant") <- attr(terms$fit, "constant")
    names(outlist) <- sapply(cn, carrier.name)[in.mf]
    return(outlist)
  }
  if (!is.null(smooth)) 
    smooth <- match.fun(smooth)
  if (is.null(ylabs)) 
    ylabs <- paste("Partial for", nmt)
  if (is.null(main)) 
    main <- ""
  else if (is.logical(main)) 
    main <- if (main) 
      deparse(model$call, 500)
  else ""
  else if (!is.character(main)) 
    stop("'main' must be TRUE, FALSE, NULL or character (vector).")
  main <- rep_len(main, n.tms)
  if (is.null(xlabs)) {
    xlabs <- unlist(lapply(cn, carrier.name))
    if (any(transform.x)) 
      xlabs <- ifelse(transform.x, lapply(cn, deparse), 
                      xlabs)
  }
  if (partial.resid || !is.null(smooth)) {
    pres <- residuals(model, "partial")
    if (!is.null(which.terms)) 
      pres <- pres[, which.terms, drop = FALSE]
  }
  se.lines <- function(x, iy, i, ff = 2) {
    tt <- ff * terms$se.fit[iy, i]
    lines(x, tms[iy, i] + tt, lty = lty.se, lwd = lwd.se, 
          col = col.se)
    lines(x, tms[iy, i] - tt, lty = lty.se, lwd = lwd.se, 
          col = col.se)
  }
  nb.fig <- prod(par("mfcol"))
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  ylims <- ylim
  if (identical(ylims, "common")) {
    ylims <- if (!se) 
      range(tms, na.rm = TRUE)
    else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 * 
                 2 * terms$se.fit, na.rm = TRUE)
    if (partial.resid) 
      ylims <- range(ylims, pres, na.rm = TRUE)
    if (rug) 
      ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
  }
  for (i in 1L:n.tms) {
    if (identical(ylim, "free")) {
      ylims <- range(tms[, i], na.rm = TRUE)
      if (se) 
        ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[, 
                                                                 i], tms[, i] - 1.05 * 2 * terms$se.fit[, i], 
                       na.rm = TRUE)
      if (partial.resid) 
        ylims <- range(ylims, pres[, i], na.rm = TRUE)
      if (rug) 
        ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    }
    if (!in.mf[i]) 
      next
    if (is.fac[i]) {
      ff <- mf[, nmt[i]]
      if (!is.null(model$na.action)) 
        ff <- naresid(model$na.action, ff)
      ll <- levels(ff)
      xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
      xx <- as.numeric(ff)
      if (rug) {
        xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
        xlims[2L] <- xlims[2L] + 0.03 * diff(xlims)
      }
      plot(1, 0, type = "n", xlab = xlabs[i], ylab = ylabs[i], 
           xlim = xlims, ylim = ylims, main = main[i], xaxt = "n", 
           ...)
      if (use.factor.levels) 
        axis(1, at = seq_along(ll), labels = ll, ...)
      else axis(1)
      for (j in seq_along(ll)) {
        ww <- which(ff == ll[j])[c(1, 1)]
        jf <- j + c(-0.4, 0.4)
        lines(jf, tms[ww, i], col = col.term, lwd = lwd.term, 
              ...)
        if (se) 
          se.lines(jf, iy = ww, i = i)
      }
    }
    else {
      xx <- carrier(cn[[i]], transform.x[i])
      if (!is.null(use.rows)) 
        xx <- xx[use.rows]
      xlims <- range(xx, na.rm = TRUE)
      if (rug) 
        xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
      oo <- order(xx)
      plot(xx[oo], tms[oo, i], type = "l", xlab = xlabs[i], 
           ylab = ylabs[i], xlim = xlims, ylim = ylims, 
           main = main[i], col = col.term, lwd = lwd.term, 
           ...)
      if (se) 
        se.lines(xx[oo], iy = oo, i = i)
    }
    if (partial.resid) {
      if (!is.fac[i] && !is.null(smooth)) {
        smooth(xx, pres[, i], lty = lty.smth, cex = cex, 
               pch = pch, col = col.res, col.smooth = col.smth, 
               span = span.smth)
      }
      else points(xx, pres[, i], cex = cex, pch = pch, 
                  col = col.res)
    }
    if (rug) {
      n <- length(xx)
      lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1L] + 
                                                          c(0, 0.05, NA) * diff(ylims), n))
      if (partial.resid) 
        lines(rep.int(xlims[1L] + c(0, 0.05, NA) * diff(xlims), 
                      n), rep.int(pres[, i], rep.int(3, n)))
    }
  }
  invisible(n.tms)
}

diag_4_fig <- function(y_pred, y){
  plot(y_pred,y, xlab = "Predicted", ylab = "Observed", 
       xlim = c(min(y), max(y)), ylim = c(min(y), max(y)))
  abline(a=0,b=1, lty = 2)
  
  brks <- pretty(range(c(y, y_pred), na.rm = TRUE), n = 30)
  
  hist(y, breaks = brks, freq = FALSE,
       col = adjustcolor("steelblue", 0.4), border = NA,
       xlab = "", main = "")
  
  hist(y_pred, breaks = brks, freq = FALSE,
       col = adjustcolor("tomato", 0.4), border = NA, add = TRUE)
  
  legend("topright", fill = c(adjustcolor("steelblue",0.4), adjustcolor("tomato",0.4)),
         border = NA, legend = c("Actual", "Pred"))
  resid_mean <- y - y_pred
  plot(y_pred, resid_mean, xlab="Fitted (mean)", ylab="Residual", main="Residuals vs Fitted")
  abline(h=0,lty=2)
  qqnorm(resid_mean); qqline(resid_mean)
  
}

lm_diag_figs <- function(lm_res, ylabs, xlabs, target_var = "diff_wis_overall"){
  y_pred <- lm_res$fitted.values
  y <- lm_res$model[[target_var]]
  
  diag_4_fig(y_pred, y)
  
  termplot(lm_res, partial = TRUE, se = TRUE,
           ylab = ylabs,
           xlab = xlabs,
           bty="n")
  
}


###### GAM

gam_diag_figs <- function(gam_res, xlabs, ylabs){
  y_pred <- gam_res$fitted.values
  y <- gam_res$y
  
  diag_4_fig(y_pred, y)
  for(i in 1:length(xlabs)){
    plot(gam_res, shade = TRUE, select = i, 
         xlab = xlabs[i], 
         ylab = ylabs[i],
         residuals = TRUE, cex = 3)
  }
}



bart_pd_imp_plot <- function(bart_res, predictor_multiline_names, X, predictors, y_pred, y){
  vc <- try(bart_res$varcount, silent=TRUE)  # 일부 빌드에서 제공
  if (!inherits(vc, "try-error") && !is.null(vc)) {
    incl <- colSums(vc) / sum(vc)
    barplot(incl, names.arg=predictor_multiline_names, las=1, cex.names=0.7,
            main="Variable inclusion proportion")
  }  
  
  pd_bart <- function(var, grid = NULL, n = 25,
                      probs = c(0.025, 0.975),
                      type = c("ppd", "mean")) {
    type <- match.arg(type)
    X0 <- X
    
    # 👉 전체 x 범위 쓰고 싶으면: min~max
    if (is.null(grid)) {
      grid <- seq(
        min(X0[[var]], na.rm = TRUE),
        max(X0[[var]], na.rm = TRUE),
        length.out = n
      )
    }
    
    out_list <- lapply(grid, function(g) {
      Xnew <- X0
      Xnew[[var]] <- g
      
      # ndpost x n
      D <- predict(bart_res, newdata = Xnew,
                   type = type, combineChains = TRUE)
      
      # iteration별 PD 값: 각 row(=iteration)에서 평균
      pd_draws <- rowMeans(D)   # 길이 ndpost
      
      c(
        mean  = mean(pd_draws),
        lower = quantile(pd_draws, probs[1]),
        upper = quantile(pd_draws, probs[2])
      )
    })
    
    out <- do.call(rbind, out_list)
    out <- as.data.frame(out)
    out$x <- grid
    
    out
  }
  
  for (i in seq_along(predictors)) {
    var   <- predictors[i]
    pd_df <- pd_bart(var)   # 위에서 만든 함수
    if(is.null(xlabs)){
      var_name <- var[i]
    }else{
      var_name <- xlabs[i]
    }
    
    
    # PD를 중심화해서 그릴지 여부 (선택)
    center <- mean(pd_df$mean)
    m  <- pd_df$mean  - center
    lo <- pd_df$lower - center
    hi <- pd_df$upper - center
    
    # 1) 먼저 빈 플롯 세팅 (축 범위 맞추기)
    
    resid_mean <- y - y_pred
    plot(
      X[[var]], resid_mean,
      xlab = var_name,
      ylab = ylabs[i],
      type = "n"   # 일단 점은 안 그림
    )
    
    # 2) shade (credible band) 추가
    polygon(
      x = c(pd_df$x, rev(pd_df$x)),
      y = c(lo, rev(hi)),
      col = adjustcolor("gray", alpha.f = 0.4),
      border = NA
    )
    
    # 3) PD mean 선
    lines(
      pd_df$x, m,
      lwd = 2
    )
    
    # 4) residual point들 위에 찍기 (shade 위에 올라오게)
    points(
      X[[var]], resid_mean,
      pch  = 16,
      cex  = 1,
      col  = adjustcolor("black")
      #col  = adjustcolor("gray50", alpha.f = 0.4)
    )
    
    abline(h = 0, lty = 2)
    #title(main = "BART partial dependence with residuals")
  }
  
}

bart_diag_figs <- function(bart_res, xlabs, ylabs, predictor_multiline_names, predictors){
  y  <- bart_res$y
  y_pred <- bart_res$yhat.train.mean
  diag_4_fig(y_pred, y)
  bart_pd_imp_plot(bart_res, predictor_multiline_names, X, predictors, y_pred, y)
}

