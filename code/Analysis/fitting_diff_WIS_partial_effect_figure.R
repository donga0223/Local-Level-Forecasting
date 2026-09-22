library(ggplot2)
library(dplyr)

plot_term_gg <- function(
    model,
    term,
    data,                 # 모델을 fit할 때 사용한 원 데이터
    xlab = term,
    ylab = expression("Partial effect on " * Delta * " MWIS"),
    n = 200,
    ci_mult = 1.96,
    partial_resid = TRUE
) {
  # 1) term의 x grid 만들기
  x <- data[[term]]
  if (is.factor(x) || is.character(x)) {
    # 범주형이면 level 그대로
    x_grid <- sort(unique(x))
  } else {
    x_grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)
  }
  
  # 2) newdata 구성: 다른 변수는 대표값(수치=median, 범주=최빈값)
  nd <- data[rep(1, length(x_grid)), , drop = FALSE]
  for (v in names(nd)) {
    if (v == term) next
    if (is.numeric(data[[v]]) || is.integer(data[[v]])) {
      nd[[v]] <- median(data[[v]], na.rm = TRUE)
    } else {
      # 최빈값
      tab <- table(data[[v]])
      nd[[v]] <- names(tab)[which.max(tab)]
      nd[[v]] <- as.factor(nd[[v]])
      # factor level 맞추기
      if (is.factor(data[[v]])) nd[[v]] <- factor(nd[[v]], levels = levels(data[[v]]))
    }
  }
  nd[[term]] <- x_grid
  if (is.factor(data[[term]])) nd[[term]] <- factor(nd[[term]], levels = levels(data[[term]]))
  
  # 3) term contribution + SE (type="terms"가 핵심)
  pr_terms <- predict(model, newdata = nd, type = "terms", se.fit = TRUE)
  term_idx <- which(colnames(pr_terms$fit) == term)
  if (length(term_idx) != 1) {
    stop(sprintf("term '%s' not found in predict(type='terms') output.", term))
  }
  
  df_term <- tibble(
    x = x_grid,
    fit = pr_terms$fit[, term_idx],
    se  = pr_terms$se.fit[, term_idx]
  ) %>%
    mutate(
      lo = fit - ci_mult * se,
      hi = fit + ci_mult * se
    )
  
  # 4) partial residuals (원하면)
  # partial residual = residual + term_effect_at_observed_x
  # term_effect_at_observed_x는 model matrix 기반으로 얻기 어려우니
  # predict(type="terms")를 원데이터에 바로 적용해서 가져옴
  df_res <- NULL
  if (partial_resid) {
    pr_obs <- predict(model, newdata = data, type = "terms", se.fit = FALSE)
    if (!term %in% colnames(pr_obs)) {
      stop(sprintf("term '%s' not found in observed terms.", term))
    }
    df_res <- tibble(
      x = data[[term]],
      y = resid(model) + pr_obs[, term]
    )
  }
  
  # 5) ggplot (위 그림과 최대한 같은 스타일)
  g <- ggplot(df_term, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
    geom_line(linewidth = 1.1) +
    labs(x = xlab, y = ylab) +
    theme_classic()
  
  if (partial_resid && !is.null(df_res)) {
    g <- g + geom_point(data = df_res, aes(x = x, y = y),
                        inherit.aes = FALSE, alpha = 0.8, size = 2) + 
      theme_classic(base_size = 15) +
      theme(
        axis.title = element_text(size = 15)
      )
  }
  
  g
}

var_names <- c("log_pop_ratio", "log_urban_pop_share", "n_msa")
var_xlabs <- c("HSA-to-state population ratio (Log)",
                "Proportion of urban population (log)",
               # "HSA-to-state area ratio (log)",
               "Number of MSAs per state")

i <- 3
p_term <- plot_term_gg(
  model = lm_res1,
  term  = var_names[i],
  data  = lm_res1$model, # df_all3,                 # lm_res1 fit에 사용한 데이터
  xlab  = var_xlabs[i]
)

png(paste0("paper_figures/LM_fitting3_log_wis_var3_interaction_nmsa_pe_",i,".png"), 
    width = 4.5, height = 3.5, units = "in", res = 600)
print(p_term)
dev.off()


lm_res2 <- lm(y_h1 ~ log_pop_ratio + log_urban_pop_share + n_msa, data = df_all3)

