  library(tidyverse)
  library(plm)
  library(lme4)
  library(lmtest)
  library(lmerTest)
  library(knitr)
  library(car)
  library(kableExtra)

  df = read_csv2("C:/.../data1.csv")
  
  ## Panel data creation
  
  df = pdata.frame(df, index = c("id", "t"))

  ## Data transformations
  df$First_time_applicants_log1p = log1p(df$First_time_applicants)
  df$Price_maize_log = log(df$Price_maize)
  df$Price_rice_log = log1p(df$Price_rice)
  df$GDPpc_log = log(df$GDPpc)
  df$Distance_km_1000 = df$Distance_km / 1000
  df$Population_size_million = df$Population_size / 1e6
  
  df$Price_maize_log <- df$Price_maize_log - mean(df$Price_maize_log, na.rm = TRUE)
  df$Price_rice_log <- df$Price_rice_log - mean(df$Price_rice_log, na.rm = TRUE)
  df$GDPpc_log <- df$GDPpc_log - mean(df$GDPpc_log, na.rm = TRUE)
  df$Distance_km_1000 <- df$Distance_km_1000 - mean(df$Distance_km_1000, na.rm = TRUE)
  df$Population_size_million <- df$Population_size_million - mean(df$Population_size_million, na.rm = TRUE)
  
  str(df)
  class(c(20))
  
    ## ============================================================================
  ## Pooling
  ## ============================================================================
  
  fit_pooling = plm(First_time_applicants_log1p ~
                      Population_size_million +
                      Age_group +
                      Price_maize_log +
                      Price_rice_log +
                      GDPpc_log +
                      Unemployment +
                      Conflict +
                      Disaster_people_affected +
                      Political_restriction  +
                      Distance_km_1000 +
                      id:Distance_km_1000,
                    data = df,
                    model = "pooling")
  
  kable(summary(fit_pooling)$coef, digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
    bptest(fit_pooling)
  
  par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
  qqPlot(resid(fit_pooling))
  
  coefci(fit_pooling)
  
  
  ## ============================================================================
  ## Within
  ## ============================================================================
  
  fit_within = plm(First_time_applicants_log1p ~
                     Population_size_million +
                     Age_group +
                     Price_maize_log +
                     Price_rice_log +
                     GDPpc_log +
                     Unemployment +
                     Conflict +
                     Disaster_people_affected +
                     Political_restriction  +
                     Distance_km_1000 +
                     id:Distance_km_100,
                   data = df,
                   model = "within",
                   effect = "individual")
  
  
  kable(summary(fit_within)$coef, digits = 5) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  bptest(fit_within)
  
  par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
  qqPlot(resid(fit_within))
  
  coefci(fit_within)
  
  
  ## ============================================================================
  ## Compare Pooling vs. Within
  ## ============================================================================
  
  ## First variant: Using plm's pFtest:
  ## F-Test.
  ## Hypothesis: 
  ## H0: factor(id) in fit_within is _not_ relevant
  ## vs.
  ## H1: factor(id) in fit_within is relevant
  
  pFtest(fit_within, fit_pooling)
  
  ## p < 0.001 ==> Reject H0, factor(id) seems to be relevant
  ## ==> Within model wins
  
  ## Second variant: Using anova function: Refit models:
  
  fit_pooling_lm = lm(First_time_applicants_log1p ~
                        Population_size_million +
                        Age_group +
                        Price_maize_log +
                        Price_rice_log +
                        GDPpc_log +
                        Unemployment +
                        Conflict +
                        Disaster_people_affected +
                        Political_restriction +
                        Distance_km_1000,
                      data = df)
  
  fit_within_lm = lm(First_time_applicants_log1p ~
                       Population_size_million +
                       Age_group +
                       Price_maize_log +
                       Price_rice_log +
                       GDPpc_log +
                       Unemployment +
                       Conflict +
                       Disaster_people_affected +
                       Political_restriction  +
                       Distance_km_1000 +
                       factor(id),
                     data = df)
  
  
  R2 = lapply(fit_within_lm, function(x) summary(x)$r.squared) %>%
    unlist
  
  R2_adj = lapply(fit_within_lm, function(x) summary(x)$adj.r.squared) %>%
    unlist
  
  F_test = lapply(fit_within_lm, function(x) summary(x)$fstatistic) %>%
    Reduce("rbind", .)
  
  ##H0: Price_maize_log = 0 und Price_rice_log = 0
  
  linearHypothesis(fit_within_lm, c("Price_maize_log", "Price_rice_log"), c(0, 0))
  
  ## --> H0 cannot be rejected.
  
  ## F-Test
  anova(fit_pooling_lm, fit_within_lm)
  
  ## ==> Within model wins
  
  ## ============================================================================
  ## Random
  ## ============================================================================
  
  fit_random = plm(First_time_applicants_log1p ~
                     Population_size_million +
                     Age_group +
                     Price_maize_log +
                     Price_rice_log +
                     GDPpc_log +
                     Unemployment +
                     Conflict + 
                     Disaster_people_affected +
                     Political_restriction +
                     Distance_km_1000,
                   data = df,
                   model = "random",
                   effect = "individual")

  
  dev.off()
  kable(summary(fit_random)$coef, digits = 5) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  R2 = lapply(fit_random, function(x) summary(x)$r.squared) %>%
    unlist
  
  R2_adj = lapply(fit_random, function(x) summary(x)$adj.r.squared) %>%
    unlist
  
  F_test = lapply(fit_random, function(x) summary(x)$fstatistic) %>%
    Reduce("rbind", .)
  
  

  bptest(fit_random)
  
  par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
  qqPlot(resid(fit_random))
  
  coefci(fit_random)
  
  ## Hausman-Test 
  phtest(fit_within_lm, fit_random) ## Produces an error!
  
  library(pracma) ## Using this package for the Moore Penrose pseudoinverse. 
  
  ## Hausman-test for panel models.
  ## Hypothesis: 
  ## H0: Within model Consistent Inefficient and random model Consistent Efficient
  ## vs.
  ## H1: Within model Consistent and Random model Inconsistent
  
  ## Coefficients
  coef_within = coef(fit_within_lm)[1:11]
  coef_random = coef(fit_random)
  
  ## Pseudo inverse
  pseudoinv = pinv(vcov(fit_within_lm)[1:11,1:11] - vcov(fit_random))
  
  ## Chisq test statistic
  chsq = t(coef_random - coef_within) %*% pseudoinv %*% (coef_random - coef_within)
  
  ## degrees of freedom
  rk = qr(vcov(fit_within_lm)[1:11,1:11] - vcov(fit_random))$rank
  
  ## p-value
  pv = pchisq(chsq, df = rk, lower.tail = FALSE)
  
  ## test result:
  cat(paste0("Hausman-Test for comparison of random and within panel models.\nchisq = ", 
             round(chsq, 2), " df = ", rk, " p-value = ", round(pv, 3)))
  
  ## p-value < 0.05 ==> reject H0
  ## ==> within model wins (consistent)
  
  
  ## ============================================================================
  ## Variable selection for winning model
  ## ============================================================================
  
  ## All possible influencing factors (apart from the food prices)
  vars = c("Population_size_million","Age_group", "GDPpc_log", "Unemployment",
           "Distance_km_1000", "Disaster_people_affected", "Conflict",
           "Political_restriction")
  
  ## All possible 1er, 2er, 3er, 4er, 5er, 6er, 7er und 8er combinations of these factors
  combinations = lapply(1:8, function(i) combn(x = vars, m = i))
  
  ## Convert combinations in formulas
  forms = lapply(combinations, function(x){
    apply(x, 2,
          function(y) paste("First_time_applicants_log1p ~ Price_rice_log + Price_maize_log + GDPpc_log + Unemployment + Conflict + factor(id) +",
                            paste(y, collapse = " + ")))
  }) %>%
    unlist
  
  
  fit_within_VS = lapply(forms, function(form) lm(form,
                                                  data = na.omit(df)))
  
  ## Checking on significance
  check = lapply(fit_within_VS, 
                 function(x) all(summary(x)$coef[-c(1:3, which(grepl(pattern = "factor", x = row.names(summary(x)$coef)))),4] < 0.05)) %>%
    unlist
  
  ## Extract models where all additional effects are significant
  w = which(check)
  
  ## Compute AIC for these models
  aics = lapply(fit_within_VS[w], AIC) %>% unlist
  
  ## Model with minimum AIC wins
  opt = w[which.min(aics)]
  fit_within_aic = fit_within_VS[[opt]]
  summary(fit_within_aic)
  
  
  kable(summary(fit_within_aic)$coef, digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  coefci(fit_within_aic)
  
  R2 = lapply(fit_within_aic, function(x) summary(x)$r.squared) %>%
    unlist
  
  R2_adj = lapply(fit_within_aic, function(x) summary(x)$adj.r.squared) %>%
    unlist
  
  F_test = lapply(fit_within_aic, function(x) summary(x)$fstatistic) %>%
    Reduce("rbind", .)
  
  
  ## Step wise variable selection using AIC
  library(MASS)
  fit_within_aic_2 = stepAIC(update(fit_within_lm, data = na.omit(df)))
  summary(fit_within_aic_2)
  
  kable(summary(fit_within_aic_2)$coef, digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  
  ## ============================================================================
  ## Comparison reduced and full model
  ## ============================================================================
  
  ## Extract results from reduced model
  ci_reduced = confint(fit_within_aic)
  res_reduced = tibble(
    Predictor = names(coef(fit_within_aic)),
    Effect = coef(fit_within_aic),
    Lower = ci_reduced[,1],
    Upper = ci_reduced[,2],
    pvalue = summary(fit_within_aic)$coef[,4],
    Type = "reduced"
  )
  
  ## Extract results from full model
  ci_full = confint(fit_within_lm)
  res_full = tibble(
    Predictor = names(coef(fit_within_lm)),
    Effect = coef(fit_within_lm),
    Lower = ci_full[,1],
    Upper = ci_full[,2],
    pvalue = summary(fit_within_lm)$coef[,4],
    Type = "full"
  )
  
  ## Plot without distance
  
  p = ggplot(res %>%
               filter(Predictor != "(Intercept)",
                      Predictor != "Distance_km_1000"), 
             aes(x = Predictor, y = Effect, ymin = Lower, ymax = Upper, colour = Type))
  p + geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
    geom_pointrange(position = position_dodge(width = .8)) +
    facet_wrap(~ effect_type, scales = "free") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "top") +
    labs(y = "Point estimate and 95% uncertainty interval", x = NULL)
  
  
  ## Combine

  res = bind_rows(res_full,
                  res_reduced)
  res$effect_type = "Predictor"
  res$effect_type[grepl(pattern = "factor", x = res$Predictor)] = "individual"
  
  
  ## Display
  p = ggplot(res %>%
               filter(Predictor != "(Intercept)"), 
             aes(x = Predictor, y = Effect, ymin = Lower, ymax = Upper, colour = Type), 
               
             )
  
  p + geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
    geom_pointrange(position = position_dodge(width = .8)) +
    facet_wrap(~ effect_type, scales = "free") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "top") +
    labs(y = "Point estimate and 95% uncertainty interval", x = NULL)
  
  ## Nearly no differences between reduced and full model visible.
  
  ## Table
  res %>%
    filter(effect_type == "Predictor") %>%
    dplyr::select(Type, Predictor, Effect, Lower, Upper, pvalue) %>% 
    mutate(Sign = ifelse(pvalue < 0.05, "*", "")) %>%
    kable(digits = 3)
  
  
  ## ============================================================================
  ## Check model assumptions for reduced model
  ## ============================================================================
  
  ## First, refit reduced within model using plm
  fit_within_aic_plm = plm(First_time_applicants_log1p ~
                             Price_maize_log +
                             Price_rice_log +
                             Political_restriction  +
                             Distance_km_1000,
                           data = df,
                           model = "within",
                           effect = "individual")
  
  
  ## Using the Studentized residuals as these values show no correltaion.
  
  r = rstudent(fit_within_aic)
  
 
  
  
  ## ===================================
  ## Normal distributed error components
  ## ===================================
  
  qqPlot(r)
  
  ## ===================================
  ## Outlier analysis
  ## ===================================
  
  ## First: Cook's distance. 
  plot(cooks.distance(fit_within_aic))
  
  ## ==> All values smaller 1. The largest value belongs to:
  na.omit(df)[which.max(cooks.distance(fit_within_aic)),]
  
  
  ## Conducting DFBETAS
  
  dfbetas(fit_within_aic) %>%
    as_tibble %>%
    dplyr::select(-starts_with("factor"), -contains("Intercept")) %>%
    mutate(indx = 1:length(r)) %>%
    gather(-indx, key = "Predictor", value = "value") %>%
    ggplot(aes(x = indx, y = value)) +
    geom_point() +
    facet_wrap(~ Predictor)
  
  ## ==> Effect on standardized regression coefficients is ok.
  
  
  ## ====================================
  ## Plot of residuals against predictors
  ## ====================================
  
  ## First: Graphs to check if there is a trend in the error components
  ## along the predictors.
  
  na.omit(df) %>%
    as_tibble() %>%
    mutate(r = r) %>%
    dplyr::select(r, Price_rice_log, Price_maize_log, Distance_km_1000, Political_restriction) %>%
    gather(-r, key = "Predictor", value = "value") %>%
    ggplot(aes(x = value, y = r)) +
    geom_point() +
    stat_smooth() +
    facet_wrap(~ Predictor, scales = "free_x")
  
  ## No trend visible.
  
  ## ===============================================
  ## Checks for heterogenity and serial correlations
  ## ===============================================
  
  ## Test for heteroskedasticity
  ## Breusch-Pagan test against heteroskedasticity.
  bptest(fit_within_aic_plm)
  ## ==> p < 0.001, strong indication of heteroskedasticity.
  
  ## ==> heteroskedasticity seems to be an issue
  
  ## Test for serial correlation
  ## Breusch-Godfrey of serial correlation:
  pbgtest(fit_within_aic_plm)
  ## ==> p < 0.001, strong indication of serial correlation in idiosyncratic errors
  
  ## Wooldridge Test for AR(1) Errors
  pwartest(fit_within_aic_plm)
  ## ==> p = 0.002, strong indication of AR(1) correlated errors
  
  ## ==> Serial correlation seems to be an issue.
  
  #Arellano estimator
  
  ## Regression coefficients
  coeftest(fit_within_aic_plm, vcov. = vcovHC(fit_within_aic_plm, method = "arellano"))
  
  kable(summary(fit_within_aic_plm)$coef, digits = 5) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  ## Confidence intervals
  coefci(fit_within_aic_plm, vcov. = vcovHC(fit_within_aic_plm, method = "arellano"))
  
  kable(summary(fit_within_aic_plm)$coef, digits = 5) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  
  ## ============================================================================
  ## Figures for individual and temporal effects
  ## ============================================================================
  
  ## Individual effect (country)
  fit = lm(First_time_applicants_log1p ~ scale(Price_rice_log) + scale(Price_maize_log) + 
             scale(Distance_km_1000) + Political_restriction + factor(id) - 1, 
           data = df)
  
  ci = confint(fit)
  cc = df %>%
    group_by(id) %>%
    summarize(Country = unique(Country)[1])
  tibble(
    eff = coef(fit)[-(1:4)],
    Lower = ci[-(1:4),1],
    Upper = ci[-(1:4),2],
    Country = cc[[2]]
  ) %>%
    ggplot(aes(x = Country, y = eff, ymin = Lower, ymax = Upper)) +
    geom_pointrange() +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
    coord_flip() +
    labs(y = "Effect")
  ggsave("20191028_country_effect.pdf", width = 8, height = 6)
  
  ## Temporal effect (year)
  ## Note: Political_restriction correlates highly with Year, as it can be seen here:
  table(df$Political_restriction, df$t)
  ## Thus, in order to estimate a temporal effect Political_restriction cannot be included
  ## in the model formula!
  
  fit = lm(First_time_applicants_log1p ~ scale(Distance_km_1000) + scale(Price_rice_log) + 
             scale(Price_maize_log) + factor(t) - 1, 
           data = df)
  
  ci = confint(fit)
  cc = df %>%
    group_by(t) %>%
    summarize(Year = unique(t)[1])
  tibble(
    eff = coef(fit)[-(1:3)],
    Lower = ci[-(1:3),1],
    Upper = ci[-(1:3),2],
    Year = cc[[2]]
  ) %>%
    ggplot(aes(x = Year, y = eff, ymin = Lower, ymax = Upper)) +
    geom_pointrange() +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
    coord_flip() +
    labs(y = "Effect")
  ggsave("20191028_year_effect.pdf", width = 8, height = 6)
  
  
  
  ## Regression coefficients
  coeftest(fit_within_aic_plm, vcov. = vcovHC(fit_within_aic_plm, method = "arellano"))
  
  ## Confidence intervals
  coefci(fit_within_aic_plm, vcov. = vcovHC(fit_within_aic_plm, method = "arellano"))

  