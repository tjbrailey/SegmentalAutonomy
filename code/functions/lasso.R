#' Covariate selection using LASSO

lasso_select <- function(var, ...){
  
  y <- lasso_dat[[var]][which(!is.na(lasso_dat[[var]]))]
  
  # Subset x such that the outcome variable is not missing (glmnet d.n. handle missing values)
  sub.x <- subset(x, !is.na(lasso_dat[[var]]))
  
  # Run lasso model
  lasso <- glmnet::cv.glmnet(sub.x,y,nfolds=10)
  
  # Plot
  plot(lasso)
  
  # Save results
  coef(lasso, s = lasso$lambda.min)
  lam = lasso$lambda.min
  
  # Output selected vars
  lasso_vars <- colnames(sub.x)[which(as.matrix(glmnet::coef.glmnet(lasso, s = "lambda.min") != 0))-1]
  sub.x <- subset(sub.x, select = lasso_vars)
  
  # Store selected vars for output
  lasso_vars <- colnames(sub.x)
  
  # If no additional controls selected, save as placeholder (var==1 for all observations)
  if (is.null(lasso_vars)) {
    lasso_vars <- c("placeholder")
  }
  
  return(lasso_vars)
}

lm_with_lasso_vars <- function(var, dv = "epr_reg_aut_dum", ...){
  
  lasso_vars <- lasso_select(var)
  
  form <- formula(paste0(var, " ~ (", dv, "* tb_other_provis) + as.factor(country) + as.factor(year) + ", paste0(lasso_vars, collapse=" + ")))
  reg <-
    estimatr::lm_robust(
      form,
      data = psp, 
      se_type = "CR2", 
      clusters = country
    )
  
  summary(reg)
  
  return(reg)
}

binom_with_lasso_vars <- function(var, dv = "epr_reg_aut_dum", ...){
  
  lasso_vars <- lasso_select(var)
  
  form <- formula(paste0(var, " ~ (", dv, "* tb_other_provis) + as.factor(country) + as.factor(year) + ", paste0(lasso_vars, collapse=" + ")))
  reg <- glm(form, data = psp, family = binomial(link = "logit"))
  
  reg_robust <- lmtest::coeftest(reg, vcov. = sandwich::vcovCL(reg, cluster = psp$country, type = "HC0"))
  summary(reg_robust)
  reg_robust
  reg
  
  my_list <- list()
  my_list[["a"]] <- reg
  my_list[["b"]] <- reg_robust
    
  return(my_list)
}

ologit_with_lasso_vars <- function(var, dv = "epr_reg_aut_dum", ...){
  
  lasso_vars <- lasso_select(var)
  
  form <- formula(paste0(as.factor(var), " ~ (", dv, "* tb_other_provis) + as.factor(country) + as.factor(year) + ", paste0(lasso_vars, collapse=" + ")))
  reg <- MASS::polr(form, data = psp, Hess=TRUE)
  
  reg_robust <- lmtest::coeftest(reg, vcov. = sandwich::vcovCL(reg, cluster = psp$country, type = "HC0"))
  summary(reg_robust)
  reg_robust
  reg
  
  my_list <- list()
  my_list[["a"]] <- reg
  my_list[["b"]] <- reg_robust
  
  return(my_list)
}
  
cox_with_lasso_vars <- function(var, ...){
  
  lasso_vars <- lasso_select(var)

  psp %<>% dplyr::mutate(dplyr::across(c(lasso_vars, "epr_reg_aut_dum", "tb_other_provis"), .fn = ~ dplyr::lag(.x, n = 1), .names = "lag1_{col}"))
  psp %<>% dplyr::mutate(dplyr::across(c(lasso_vars, "epr_reg_aut_dum", "tb_other_provis"), .fn = ~ dplyr::lag(.x, n = 5), .names = "lag5_{col}"))
  psp %<>% dplyr::mutate(dplyr::across(c(lasso_vars, "epr_reg_aut_dum", "tb_other_provis"), .fn = ~ dplyr::lag(.x, n = 10), .names = "lag10_{col}"))

  lag1_vars <- paste0("lag1_", lasso_vars)
  lag5_vars <- paste0("lag5_", lasso_vars)
  lag10_vars <- paste0("lag10_", lasso_vars)
  
  form1 <- formula(paste0("survival::Surv(year_from_zero, yfr_1, ", var, ") ~ (lag1_epr_reg_aut_dum * lag1_tb_other_provis) + ", paste0(lag1_vars[-5], collapse=" + ")))
  form5 <- formula(paste0("survival::Surv(year_from_zero, yfr_1, ", var, ") ~ (lag5_epr_reg_aut_dum * lag5_tb_other_provis) + ", paste0(lag5_vars[-5], collapse=" + ")))
  form10 <- formula(paste0("survival::Surv(year_from_zero, yfr_1, ", var, ") ~ (lag10_epr_reg_aut_dum * lag10_tb_other_provis) + ", paste0(lag10_vars[-5], collapse=" + ")))
  
  reg1 <- survival::coxph(form1, data = psp, cluster = country)
  reg5 <- survival::coxph(form5, data = psp, cluster = country)
  reg10 <- survival::coxph(form10, data = psp, cluster = country)
  
  summary(reg1)
  summary(reg5)
  summary(reg10)
  
  reporttools::displayCoxPH(reg1)
  reporttools::displayCoxPH(reg5)
  reporttools::displayCoxPH(reg10)
  return(list(reg1, reg5, reg10))
}

prepare_cox_for_latex <- 
  function(reg, name, caption){
    
    # Prepare the columns
    `Beta`      <- reg$coefficients
    `Robust SE` <- sqrt(diag(reg$var))
    `p-value`   <- 1 - pchisq((`Beta`/`Robust SE`)^2, 1)
    `CI`        <- round(confint(reg), 2)
    
    # Bind columns together, and select desired rows
    reg <- cbind(`Beta`, `Exp(coef)` = exp(`Beta`), `Robust SE`, CI, `p-value`)
    
    row.names(reg) <- cox_rownames
    
    # Print results in a LaTeX-ready gform
    xtable::print.xtable(xtable::xtable(reg, caption = caption), 
                         file = paste0(here::here(), "/paper/", name, ".tex"))
  }



