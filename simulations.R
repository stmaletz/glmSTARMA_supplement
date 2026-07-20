#################################################################
### D. Simulation Study
#################################################################
### This script contains the R-Code to replicate the simulations
### described in Appendix D.
### We recommend running this code on a HPC cluster, due to the
### runtime.
### This script only produces the results, but not the figures
### in the article.
### This script simulates additional settings, not reported in
### the article, as the results are similar.
#################################################################

library("glmSTARMA")
library("copula")
library("tictoc")
iterations <- 1000L

#################################################################
### Constant Dispersion - No feedback process in mean model
#################################################################
### Here we simulate data from a process with
### constant dispersion, but a model with time-varying dispersion
### is fitted to the data
### The mean model is WITHOUT feedback process.
#################################################################

model <- list(intercept = "homo", past_obs = 1, covariates = 0)
dispersion_model <- list(intercept = "homo", past_obs = 1)

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    settings <- settings[order(settings$dim, settings$obs), ]

    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }

    if(settings$copula[index] == "independent"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(dispersion = 2),
                           vnormal = vnormal(dispersion = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(dispersion = 2, copula = "frank", copula_param = 2),
                           vnormal = vnormal(dispersion = 2, copula = "frank", copula_param = 2))
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(fits = results, times = times, settings = settings)
        saveRDS(results2, file = paste0("constant_dispersion_mean_without_feedback_index_", index, ".rds"))
      }
      sim <- glmstarma.sim(ntime = settings$obs[index], parameters = params, model = model, wlist = W,
                           covariates = covariate, wlist_past_mean = W, wlist_covariates = W,
                           family = family_sim, n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, drop_max_mean_lag = TRUE, max_fits = 10, maxit = 10000))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
      times[i] <- time$toc - time$tic
    }


    results2 <- list(fits = results, times = times, settings = settings)
    saveRDS(results2, file = paste0("constant_dispersion_mean_without_feedback_index_", index, ".rds"))
}

#################################################################
### Constant Dispersion - With feedback process in mean model
#################################################################
### Here we simulate data from a process with
### constant dispersion, but a model with time-varying dispersion
### is fitted to the data
### The mean model is WITH feedback process.
#################################################################

model <- list(intercept = "homo", past_obs = 1, past_mean = 1, covariates = 0)
dispersion_model <- list(intercept = "homo", past_obs = 1)

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    settings <- settings[order(settings$dim, settings$obs), ]
    
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])
    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)
    
    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 2)
      family_fit <- vnormal()
    }

    if(settings$copula[index] == "independent"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(dispersion = 2),
                           vnormal = vnormal(dispersion = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(dispersion = 2, copula = "frank", copula_param = 2),
                           vnormal = vnormal(dispersion = 2, copula = "frank", copula_param = 2))
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        
        results2 <- list(fits = results, times = times, settings = settings)
        saveRDS(results2, file = paste0("constant_dispersion_mean_with_feedback_index_", index, ".rds"))
      }
      sim <- glmstarma.sim(ntime = settings$obs[index], parameters = params, model = model, wlist = W,
                           covariates = covariate, wlist_past_mean = W, wlist_covariates = W,
                           family = family_sim, n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, drop_max_mean_lag = TRUE, max_fits = 10, maxit = 10000))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
      times[i] <- time$toc - time$tic
    }

    results2 <- list(fits = results, times = times, settings = settings)
    saveRDS(results2, file = paste0("constant_dispersion_mean_with_feedback_index_", index, ".rds"))
}

#################################################################
### Constant Dispersion Fit
### True mean model without feedback process
### True dispersion model without feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, but fit a model with constant dispersion
### to the data.
#################################################################

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }
    params_dispersion <- list(intercept = 0.5, past_obs = c(0.5, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("constant_fit_without_without_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- glmstarma(sim$observations, model = model, covariates = covariate, wlist = W,
                       family = family_fit, control = list(method = "nloptr", maxit = 10000))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("constant_fit_without_without_index_", index, ".rds"))

}

#################################################################
### Constant Dispersion Fit
### True mean model with feedback process
### True dispersion model without feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, but fit a model with constant dispersion
### to the data.
#################################################################

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
                            
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, past_mean = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1)


    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 2)
      family_fit <- vnormal()
    }
    params_dispersion <- list(intercept = 0.5, past_obs = c(0.5, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("constant_fit_with_without_index_", index, ".rds"))
        
      }
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- glmstarma(sim$observations, model = model, covariates = covariate, wlist = W,
                       family = family_fit, control = list(method = "nloptr", maxit = 10000))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("constant_fit_with_without_index_", index, ".rds"))
}


#################################################################
### Constant Dispersion Fit
### True mean model without feedback process
### True dispersion model with feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, but fit a model with constant dispersion
### to the data.
#################################################################

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)

    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1, past_mean = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }

    params_dispersion <- list(intercept = 0.5, past_mean = c(0.15, 0.05), past_obs = c(0.3, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("constant_fit_without_with_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- glmstarma(sim$observations, model = model, covariates = covariate, wlist = W,
                       family = family_fit, control = list(method = "nloptr", maxit = 10000))

      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }


    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("constant_fit_without_with_index_", index, ".rds"))
}



#################################################################
### Constant Dispersion Fit
### True mean model with feedback process
### True dispersion model with feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, but fit a model with constant dispersion
### to the data.
#################################################################

for(index in seq(60)){
    settings <- expand.grid(dim = c(5, 10, 20),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_mean = 1, past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1, past_mean = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 2)
      family_fit <- vnormal()
    }

    params_dispersion <- list(intercept = 0.5, past_mean = c(0.15, 0.05), past_obs = c(0.3, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("constant_fit_with_with_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- glmstarma(sim$observations, model = model, covariates = covariate, wlist = W,
                       family = family_fit, control = list(method = "nloptr", maxit = 10000))

      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("constant_fit_with_with_index_", index, ".rds"))
}

#################################################################
### Time-varying Dispersion Fit
### True mean model without feedback process
### True dispersion model without feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, and fit the correct model(s) to the data.
#################################################################

for(index in seq(40)){
    settings <- expand.grid(dim = c(5, 10),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }
    params_dispersion <- list(intercept = 0.5, past_obs = c(0.5, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("tv_fit_without_without_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, dispersion_link = "log", wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, dispersion_link = "log", wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, maxit = 10000, max_fits = 10, previous_param_as_start = TRUE))
      
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("tv_fit_without_without_index_", index, ".rds"))
}



#################################################################
### Time-varying Dispersion Fit
### True mean model with feedback process
### True dispersion model without feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, and fit the correct model(s) to the data.
#################################################################

for(index in seq(40)){
    settings <- expand.grid(dim = c(5, 10),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, past_mean = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 2)
      family_fit <- vnormal()
    }
    params_dispersion <- list(intercept = 0.5, past_obs = c(0.5, 0.2))


    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }
    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("tv_fit_with_without_index_", index, ".rds"))
        
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, dispersion_link = "log", wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, dispersion_link = "log", wlist = W, mean_covariates = covariate,
                        control = list(print_progress = TRUE, maxit = 10000, max_fits = 10, previous_param_as_start = TRUE))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("tv_fit_with_without_index_", index, ".rds"))

}


#################################################################
### Time-varying Dispersion Fit
### True mean model without feedback process
### True dispersion model with feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, and fit the correct model(s) to the data.
#################################################################

for(index in seq(40)){
    settings <- expand.grid(dim = c(5, 10),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1, past_mean = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }

    params_dispersion <- list(intercept = 0.5, past_mean = c(0.15, 0.05), past_obs = c(0.3, 0.2))

    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("tv_fit_without_with_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, dispersion_link = "log", wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, dispersion_link = "log", wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, maxit = 10000, max_fits = 10, previous_param_as_start = TRUE))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("tv_fit_without_with_index_", index, ".rds"))
}


#################################################################
### Time-varying Dispersion Fit
### True mean model with feedback process
### True dispersion model with feedback process
#################################################################
### Here we simulate data from a process with space-time varying
### dispersion, and fit the correct model(s) to the data.
#################################################################


for(index in seq(40)){
    iterations <- 1000L

    settings <- expand.grid(dim = c(5, 10),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank", "independent"),
                            stringsAsFactors = FALSE)
    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_mean = 1, past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1, past_mean = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = 0.6, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = 5, past_mean = c(0.2, 0.1), past_obs = c(0.2, 0.1), covariates = 2)
      family_fit <- vnormal()
    }
    params_dispersion <- list(intercept = 0.5, past_mean = c(0.15, 0.05), past_obs = c(0.3, 0.2))
  
    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(5)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("tv_fit_with_with_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, dispersion_link = "log", wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model,
                        mean_family = family_fit, dispersion_link = "log", wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, maxit = 10000, max_fits = 10, previous_param_as_start = TRUE))
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("tv_fit_with_with_index_", index, ".rds"))
}

#################################################################
### Additional simulation change level of time series
### True mean model without feedback process
### True dispersion model without feedback process
#################################################################
### Here we simulate data at different levels, i.e.
### different intercepts of the mean model
#################################################################

for(index in seq(500)){
    intercept_mean_log <- seq(from = 0.1, to = 2, length.out = 50)
    intercept_mean_identity <- seq(from = 3, to = 10, length.out = 50)

    settings <- expand.grid(dim = c(5),
                            distribution = c("vquasipoisson", "vnormal"),
                            obs = c(50, 100, 250, 500, 1000),
                            copula = c("frank"),
                            intercept = c(intercept_mean_log, intercept_mean_identity),
                            stringsAsFactors = FALSE)

    settings <- subset(settings, (distribution == "vquasipoisson" & intercept < 2.5) | (distribution == "vnormal" & intercept > 2.5) )
    row.names(settings) <- 1:500


    W <- generateW("rectangle", dim = settings$dim[index]^2, 4, width = settings$dim[index])

    set.seed(42)
    covariates <- t(replicate(settings$dim[index]^2,
                              arima.sim(n = 1000,
                                        list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)),
                                        rand.gen = runif)))
    covariates <- covariates / max(covariates)
    covariates[covariates < 0] <- 0
    covariates <- covariates[, seq(settings$obs[index])]
    covariate <- list(X = covariates)

    model <- list(intercept = "homo", past_obs = 1, covariates = 0)
    dispersion_model <- list(intercept = "homo", past_obs = 1)

    if(settings$distribution[index] == "vquasipoisson"){
      params <- list(intercept = settings$intercept[index], past_obs = c(0.4, 0.2), covariates = 0.9)
      family_fit <- vquasipoisson()
    } else {
      params <- list(intercept = settings$intercept[index], past_obs = c(0.4, 0.2), covariates = 2)
      family_fit <- vnormal()
    }

    params_dispersion <- list(intercept = 0.5, past_obs = c(0.5, 0.2))


    if(settings$copula[index] == "frank"){
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(copula = "frank", copula_param = 2), vnormal = vnormal(copula = "frank", copula_param = 2))
    } else {
      family_sim <- switch(settings$distribution[index], vquasipoisson = vquasipoisson(), vnormal = vnormal())
    }

    results <- vector(mode = "list", length = iterations)
    times <- numeric(iterations)
    means <- numeric(iterations)

    for(i in seq(iterations)){
      set.seed(i)
      if(i %% 10 == 0){
        cat("Iteration", i, "\n")
      }
      if(i %% 50 == 0){
        gc()
        Sys.sleep(1)
        results2 <- list(result = results, times = time, settings = settings)
        saveRDS(results2, file = paste0("varying_mean_index_", index, ".rds"))
      }
      
      sim <- dglmstarma.sim(settings$obs[index], params, params_dispersion, model, dispersion_model,
                      mean_family = family_sim, dispersion_link = "log", wlist = W, pseudo_observations = "deviance",
                      mean_covariates = covariate, control = list(return_burn_in = FALSE), n_start = 100)
      means[i] <- mean(sim$observations)
      tic()
      fit <- dglmstarma(sim$observations, mean_model = model, dispersion_model = dispersion_model_fit,
                        mean_family = family_fit, dispersion_link = "log", wlist = W, mean_covariates = covariate,
                        control = list(print_progress = FALSE, maxit = 10000,
                                       drop_max_mean_lag = TRUE, max_fits = 10, previous_param_as_start = TRUE))
      
      time <- toc(quiet = TRUE)
      results[[i]] <- summary(fit)
    }

    cat("Mean observations:", mean(means))
    results2 <- list(result = results, times = time, settings = settings)
    saveRDS(results2, file = paste0("varying_mean_index_", index, ".rds"))
}





