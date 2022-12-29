# 1
n_throws = c(100, 200, 400)
for (i in 1:3) {
  n = n_throws[i]
  count_discarded = 0
  
  for (k in 1:10000) {
    # n пъти хвърляме зар
    points = sample(1:6, size = n, replace = TRUE)
    test = chisq.test(points)
    if (test$p.value < 0.95) {
      count_discarded = count_discarded + 1
    }
  }
  
  print(paste("H0 was discarded", count_discarded, "times for n =", n))
}


# 2
n_values = c(15, 25, 50, 90)
for (i in 1:4) {
  n = n_values[i]
  count_discarded = 0
  
  for (k in 1:10000) {
    data = runif(n, 5, 12)
    test = shapiro.test(data)
    if (test$p.value < 0.05) {
      count_discarded = count_discarded + 1
    }
  }
  
  print(paste("The test is true", 10000 - count_discarded, "times for n =", n))
}


# 3 - колко % доверителен интервал искаме???
n_values = c(30, 50, 100, 500)

for (i in 1:4) {
  n = n_values[i]
  
  results = matrix(data = NA, nrow = 4, ncol = 3)
  rownames(results) = c("εi ∈ N(0,4)", 
                        "εi ∈ U(-3.5,3.5)", 
                        "εi = vi − wi; vi,wi ∈ Ex(0.7)", 
                        "εi = ui − 2; ui ∈ Ex(0.5)")
  colnames(results) = c("β1 in conf_int", 
                        "  mean len conf_int", 
                        "  mean β1")
  
  beta1 = 5
  # 4 реда - по 1 за всяка подточка
  beta1_est =  matrix(data = NA, nrow = 4, ncol = 10000)
  beta1_sd_err = matrix(data = NA, nrow = 4, ncol = 10000)
  
  for (k in 1:10000) {
      x = sample(1:10, size = n, replace = TRUE)
      
      # а)
      e = rnorm(n, mean = 0, sd = 2)
      y = beta1 * x + e
      
      df = data.frame(x, y)
      model = lm(y ~ x, data = df)
      model_summary = summary(model)
      beta1_est[1, k] = model_summary$coefficients[2, 1] # бета1 шапка
      beta1_sd_err[1, k] = model_summary$coefficients[2, 2]
      
      # б)
      e = runif(n, -3.5, 3.5)
      y = beta1 * x + e
      
      df = data.frame(x, y)
      model = lm(y ~ x, data = df)
      model_summary = summary(model)
      beta1_est[2, k] = model_summary$coefficients[2, 1] # бета1 шапка
      beta1_sd_err[2, k] = model_summary$coefficients[2, 2]
      
      # в)
      v = rexp(n, 0.7)
      w = rexp(n, 0.7)
      e = v - w
      y = beta1 * x + e
      
      df = data.frame(x, y)
      model = lm(y ~ x, data = df)
      model_summary = summary(model)
      beta1_est[3, k] = model_summary$coefficients[2, 1] # бета1 шапка
      beta1_sd_err[3, k] = model_summary$coefficients[2, 2]
      
      # г)
      u = rexp(n, 0.5)
      e = u - 2
      y = beta1 * x + e
      
      df = data.frame(x, y)
      model = lm(y ~ x, data = df)
      model_summary = summary(model)
      beta1_est[4, k] = model_summary$coefficients[2, 1] # бета1 шапка
      beta1_sd_err[4, k] = model_summary$coefficients[2, 2]
  }
  
  print(paste("n =", n))
  for (k in 1:4) {
    results[k, 1] = sum(abs(beta1 - beta1_est[k,]) < beta1_sd_err[k,])
    results[k, 2] = mean(2 * beta1_sd_err[k,])
    results[k, 3] = mean(beta1_est[k,])
  }
  print(results)
  cat("\n")
  
  hist(beta1_est)
  qqnorm(beta1_est)
}









