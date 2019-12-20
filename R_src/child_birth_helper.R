encode_data <- function(chds, cov_levels, cov, offset = 1)
{
  data_value <- chds[[cov]] + offset
  new_cov <- cov_levels[data_value]
  new_cov <- factor(new_cov, levels=cov_levels)
  chds[[cov]] <- new_cov
  
  return(chds)
}

encode_eth_data <- function(chds)
{
  max_eth <- max(chds$meth) + 1
  eth_threshold <- c(0, 6, 7, 8, 9, 10, 11)
  meth <- cut(chds$meth, breaks=eth_threshold, labels= eth_levels, right=FALSE)
  feth <- cut(chds$feth, breaks=eth_threshold, labels= eth_levels, right=FALSE)
  chds$meth <- meth
  chds$feth <- feth
  chds$meth <- factor(chds$meth, levels=eth_levels)
  chds$feth <- factor(chds$feth, levels=eth_levels)
  return(chds)
}

change_marital_factors <- function(chds)
{
  chds$marital[chds$marital != "married"] <- "not_married"
  chds$marital <- droplevels(chds$marital)
  return(chds)
}

change_meth_factor_levels <- function(chds)
{
  chds$meth[chds$meth == "Mexican"] <- "Other"
  chds$meth[chds$meth == "Asian"] <- "Other"
  chds$meth[chds$meth == "Mixed"] <- "Other"
  chds$meth <- droplevels(chds$meth)
  return(chds)
}

change_med_levels <- function(chds)
{
  # Adding new category "upto_medical_school" in categories for med
  med_levels <- c("upto_high_school", med_levels)
  chds$med <- factor(chds$med, levels=med_levels)
  
  # Shift observations in category "elementary" to category "upto_high_school"
  chds$med[chds$med == "elementary"] <- "upto_high_school"
  
  # Shift observations in category "middle" to category "upto_high_school"
  chds$med[chds$med == "middle"] <- "upto_high_school"
  
  # Shift observations in category "high" to category "upto_high_school"
  chds$med[chds$med == "high"] <- "upto_high_school"
  
  # Removing all observations with level "high_unclear"
  chds <- chds[chds$med != "high_unclear", ]
  
  # Drop all categories which have no observation and "high_unclear" category
  chds$med <- droplevels(chds$med)
  
  return(chds)
}

change_feth_levels <- function(chds)
{
  # Converting feth into a categorical covariate with categories "Caucasian", 
  # "African-American", "Other"
  chds$feth[chds$feth == "Mexican"] <- "Other"
  chds$feth[chds$feth == "Asian"] <- "Other"
  chds$feth[chds$feth == "Mixed"] <- "Other"
  chds$feth <- droplevels(chds$feth)
  
  return(chds)
}

change_fed_levels <- function(chds)
{
  # Handling fed 
  # adding new category "upto_medical_school" in categories for fed
  fed_levels <- c("upto_high_school", fed_levels)
  chds$fed <- factor(chds$fed, levels=fed_levels)
  
  # shift observations in category "elementary" to category "upto_high_school"
  chds$fed[chds$fed == "elementary"] <- "upto_high_school"
  
  # shift observations in category "middle" to category "upto_high_school"
  chds$fed[chds$fed == "middle"] <- "upto_high_school"
  
  # shift observations in category "high" to category "upto_high_school"
  chds$fed[chds$fed == "high"] <- "upto_high_school"
  
  # Removing all observations with level "trade"
  chds <- chds[chds$fed != "trade",]
  
  # Removing all observations with level "high_school_unclear"
  chds <- chds[chds$fed != "high_unclear", ]
  
  # drop all categories which have no observation and "high_unclear" category
  chds$fed <- droplevels(chds$fed)
  
  return(chds)
}

change_income_levels <- function(chds)
{
  # Adding new categories "0-4999" and ">=20000" to income
  income_levels <- c("0-4999", income_levels, "5000-14999", ">=15000")
  
  chds$income <- factor(chds$income, levels=income_levels)
  chds$income[chds$income == "<2500"] <- "0-4999"
  chds$income[chds$income == "2500-4999"] <- "0-4999"
  
  chds$income[chds$income == "5000-7499"] <- "5000-14999"
  chds$income[chds$income == "7500-9999"] <- "5000-14999"
  chds$income[chds$income == "10000-12499"] <- "5000-14999"
  chds$income[chds$income == "12500-14999"] <- "5000-14999"
  
  chds$income[chds$income == "15000-17499"] <- ">=15000"
  chds$income[chds$income == "17500-19999"] <- ">=15000"
  chds$income[chds$income == "20000-22499"] <- ">=15000"
  chds$income[chds$income == ">=22500"] <- ">=15000"
  
  # drop all income categories which have no observation
  chds$income <- droplevels(chds$income)
  
  return(chds)
}

change_time_levels <- function(chds)
{
  # Adding new categories "0-2years" ">2years" to time
  time_levels <- c("0-2years", ">2years", time_levels)
  
  chds$time <- factor(chds$time, levels=time_levels)
  chds$time[chds$time == "<1year"] <- "0-2years"
  chds$time[chds$time == "1-2years"] <- "0-2years"
  
  chds$time[chds$time == "2-3years"] <- ">2years"
  chds$time[chds$time == "3-4years"] <- ">2years"
  chds$time[chds$time == "5-9years"] <- ">2years"
  chds$time[chds$time == ">10years"] <- ">2years"
  
  # Removing all observations with level "quit_but_dont_know_when"
  chds <- chds[chds$time != "quit_but_dont_know_when",]
  
  # Removing all observations with level "still_smokes"
  chds <- chds[chds$time != "still_smokes",]
  
  # Drop all time categories which have no observation
  chds$time <- droplevels(chds$time)
  
  return(chds)
}

change_number_levels <- function(chds)
{
  #Adding new categories "1-9" ">=10" to number
  number_levels <- c(number_levels, "1-9", ">=10")
  
  chds$number <- factor(chds$number, levels=number_levels)
  chds$number[chds$number == "1-4"] <- "1-9"
  chds$number[chds$number == "5-9"] <- "1-9"
  
  chds$number[chds$number == "10-14"] <- ">=10"
  chds$number[chds$number == "10-14"] <- ">=10"
  chds$number[chds$number == "15-19"] <- ">=10"
  chds$number[chds$number == "20-29"] <- ">=10"
  chds$number[chds$number == "30-39"] <- ">=10"
  chds$number[chds$number == "40-60"] <- ">=10"
  chds$number[chds$number == ">60"] <- ">=10"
  
  # Removing all observations with level "smoked_but_dont_know_how_much"  
  chds <- chds[chds$number != "smoked_but_dont_know_how_much",]
  
  # Drop all number categories which have no observation
  chds$number <- droplevels(chds$number)  
  
  return(chds)
}

encode_parity <- function(chds)
{
  max_parity <- max(chds$parity) + 1
  parity_threshold <- c(0, 1, 2, 3, max_parity)
  parity2 <- cut(chds$parity, breaks=parity_threshold, labels=parity_levels, right=FALSE)
  chds$parity <- parity2
  chds$parity <- factor(chds$parity, levels=parity_levels)
  
  return(chds)
}

encode_mht <- function(chds)
{
  max_mht <- max(chds$mht)
  mht_threshold <- c(0, 60, 63, 66, max_mht)
  mht2 <- cut(chds$mht, breaks=mht_threshold, labels= mht_levels, right=TRUE)
  chds$mht <- mht2
  chds$mht <- factor(chds$mht, levels=mht_levels)
  
  return(chds)
}

forward_selection <- function(chds)
{
  # minimal model: intercept only 
  Mminimal <- lm(wt ~ 1, data=chds)
  # all main effects and interaction
  Mmaximal <- lm(wt ~ (.)^2, data=chds)
  
  system.time({
    Mfwd <- step(object=Mminimal, 
                 scope=list(lower=Mminimal, upper=Mmaximal),
                 direction="forward",
                 trace=FALSE)
  })
  
  return(Mfwd)
}

stepwise_selection <- function(chds)
{
  # minimal model: intercept only 
  Mminimal <- lm(wt ~ 1, data=chds)
  # all main effects and interaction
  Mmaximal <- lm(wt ~ (.)^2, data=chds)
  # starting model for stepwise selection
  Mstart <- lm(wt ~ ., data=chds)
  
  system.time({
    Mstep <- step(object=Mstart, 
                  scope=list(lower=Mminimal, upper=Mmaximal),
                  direction="both",
                  trace=FALSE)
  })
  
  return(Mstep)
}

cv <- function(chds, split, n_iter, M1, M2, M3, M4, names, set_name)
{
  n <- nrow(chds)
  
  mspe1 <- rep(NA, n_iter)
  mspe2 <- rep(NA, n_iter)
  mspe3 <- rep(NA, n_iter)
  mspe4 <- rep(NA, n_iter)
  
  for (i in 1:n_iter) {
    
    # shuffle dataset
    shuffled <- chds[sample(n), ]
    
    # extract training set 
    train_ind <- 1:round(split * n)
    train <- shuffled[train_ind, ]
    
    # extract test set
    test_ind <- (round(split * n) + 1):n
    test <- shuffled[test_ind, ]
    
    # refit the models on the subset of training data
    M1_cv <- lm(formula(M1), data=train)
    M2_cv <- lm(formula(M2), data=train)
    M3_cv <- lm(formula(M3), data=train)
    M4_cv <- lm(formula(M4), data=train)
    
    # out-of-sample residuals for all five models
    # that is, testing data - predictions with training parameters
    M1_res <- test$wt - predict(M1_cv, newdata=test)
    M2_res <- test$wt - predict(M2_cv, newdata=test)
    M3_res <- test$wt - predict(M3_cv, newdata=test)
    M4_res <- test$wt - predict(M4_cv, newdata=test)
    
    # mean-square prediction errors
    mspe1[i] <- mean(M1_res^2)
    mspe2[i] <- mean(M2_res^2)
    mspe3[i] <- mean(M3_res^2)
    mspe4[i] <- mean(M4_res^2)
  }
  
  # drop any NA's in our computed mspe
  mspe1 <- mspe1[!is.na(mspe1)]
  mspe2 <- mspe2[!is.na(mspe2)]
  mspe3 <- mspe3[!is.na(mspe3)]
  mspe4 <- mspe4[!is.na(mspe4)]
  
  # box-plot and histogram
  # plot rMSPE and out-of-sample log(Lambda)
  par(mfrow = c(1,2))
  par(mar = c(4.5, 4.5, .1, .1))
  boxplot(x = list(sqrt(mspe1), sqrt(mspe2), sqrt(mspe3), sqrt(mspe4)), names = names, cex = .7,
          ylab = expression(sqrt(MSPE)), col = c("yellow", "orange", "red", "blue"), main=set_name)
  
  return(c(mean(mspe1), mean(mspe2), mean(mspe3), mean(mspe4)))
}

qq_plot <- function(M, main)
{
  sigma_hat <- sqrt(sum(resid(M)^2) / M$df.residual)
  sigma_hat <- round(sigma_hat, 2) # round to 2 decimal places
  
  cex <- 0.8
  qqnorm(resid(M) / sigma_hat, pch = 16, cex = cex, cex.axis = cex, main=main)
  abline(a = 0, b = 1, col = "red") # add 45 degree line
}

residual_vs_predicted_plot <- function(M, main)
{
  res <- resid(M)
  sigma_hat <- sqrt(sum(res^2) / M$df.residual)
  sigma_hat <- round(sigma_hat, 2) # round to 2 decimal places
  h <- hatvalues(M)
  pred <- predict(M)
  res_stu <- resid(M) / sqrt(1-h) # studentized residuals, but on the data scale
  
  cex <- .8 # controls the size of the points and labels
  par(mar = c(4,4,.1,.1))
  plot(pred, resid(M), pch = 21, bg = "black", cex = cex, 
       cex.axis = cex, xlab = "Predicted Male Single-Fetus Birth Weight", 
       ylab = "Residuals", main=main)
  points(pred, res_stu, pch = 21, bg = "red", cex = cex)
  legend(x = "bottomleft", c("Residuals", "Studentized Residuals"),
         pch = 21, pt.bg = c("black", "red"), pt.cex = cex, cex = cex)
  abline(h=0, lty=2, col="grey")
}

cooks_distance_vs_leverage_plot <- function(M, data, main)
{
  p <- length(coef(M))
  n <- nrow(data)
  h <- hatvalues(M)
  res <- resid(M)
  hbar <- p / n
  
  # cook’s distance vs. leverage
  D <- cooks.distance(M)
  
  # flag some of the points
  infl_ind <- which.max(D) # top influence point
  lev_ind <- h > 2*hbar # leverage more than 2x the average
  clrs <- rep("black", len = n)
  
  clrs[lev_ind] <- "blue"
  clrs[infl_ind] <- "red"
  par(mfrow = c(1,1), mar = c(4,4,1,1))
  
  cex <- .8
  plot(h, D, xlab = "Leverage", ylab = "Cook Influence Measure",
       pch = 21, bg = clrs, cex = cex, cex.axis = cex, main=main)
  abline(v = 2*hbar, col = "grey60", lty = 2) # 2x average leverage
  legend("topleft", legend = c("High Leverage", "High Influence"), pch = 21,
         pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)
  
}