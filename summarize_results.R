#################################################################
### D. Simulation Study - Summarize Results
#################################################################
### This script contains the R-Code to summarize the results of
### the simulation studies in (long) data.frames
### Before you run this script, make sure the results have been
### produces by the simulations.R file and been stored in the
### results subdirectory
#################################################################
### Note: Summarized results are stored in new .rds files.
#################################################################


library("data.table")

files <- list.files("results/", pattern = ".rds", full.names = TRUE)


#################################################################
### Constant Dispersion - No feedback process in mean model
#################################################################

results_1_0 <- files[grep("constant_dispersion_mean_without_feedback", files)]
index <- gsub("results//constant_dispersion_mean_without_feedback_index_", "", results_1_0)
index <- as.numeric(gsub(".rds", "", index))
results_1_0 <- lapply(results_1_0, readRDS)
settings <- results_1_0[[1]]$settings[index,]

resi <- vector(mode = "list", length = 60)
for(i in seq_along(results_1_0)){
  rn <- rownames(results_1_0[[i]]$fits[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_1_0[[i]]$fits[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_1_0[[i]]$fits, function(x) x$coefficients_mean[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  p_values_mean <- sapply(results_1_0[[i]]$fits, function(x) x$coefficients_mean[, 4])
  if(is.list(p_values_mean)){
    p_values_mean <- t(do.call("rbind", p_values_mean))
  }
  estimates_dispersion <- sapply(results_1_0[[i]]$fits, function(x) x$coefficients_dispersion[, 1])
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- t(do.call("rbind", estimates_dispersion))
  }
  p_values_dispersion <- sapply(results_1_0[[i]]$fits, function(x) x$coefficients_dispersion[, 4])
  if(is.list(p_values_dispersion)){
    p_values_dispersion <- t(do.call("rbind", p_values_dispersion))
  }
  
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$fitting_time <- results_1_0[[i]]$times[seq(nrow(est))]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_1_0 <- rbindlist(resi)
saveRDS(resi_1_0, "summarized_results/constant_dispersion_mean_without_feedback.rds")

#################################################################
### Constant Dispersion - Feedback process in mean model
#################################################################

results_1_1 <- files[grep("constant_dispersion_mean_with_feedback", files)]
index <- gsub("results//constant_dispersion_mean_with_feedback_index_", "", results_1_1)
index <- as.numeric(gsub(".rds", "", index))
results_1_1 <- lapply(results_1_1, readRDS)
settings <- results_1_1[[1]]$settings[index,]
relevant <- which(settings$copula != "independent" & settings$dim < 20)

resi <- vector(mode = "list", length = 60)
for(i in relevant){
  rn <- rownames(results_1_1[[i]]$fits[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_1_1[[i]]$fits[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_1_1[[i]]$fits, function(x) x$coefficients_mean[, 1])
  p_values_mean <- sapply(results_1_1[[i]]$fits, function(x) x$coefficients_mean[, 4])
  estimates_dispersion <- sapply(results_1_1[[i]]$fits, function(x) x$coefficients_dispersion[, 1])
  p_values_dispersion <- sapply(results_1_1[[i]]$fits, function(x) x$coefficients_dispersion[, 4])
  
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$fitting_time <- results_1_1[[i]]$times[seq(nrow(est))]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_1_1 <- rbindlist(resi)
saveRDS(resi_1_1, "summarized_results/constant_dispersion_mean_with_feedback.rds")


#################################################################
### Constant Dispersion Fit
### True mean model with feedback process
### True dispersion model without feedback process
#################################################################

results_2_0 <- files[grep("constant_fit_with_without", files)]
index <- gsub("results//constant_fit_with_without_index_", "", results_2_0)
index <- as.numeric(gsub(".rds", "", index))
results_2_0 <- lapply(results_2_0, readRDS)
settings <- results_2_0[[1]]$settings[index,]
resi <- vector(mode = "list", length = 60)
for(i in seq_along(results_2_0)){
  rn <- rownames(results_2_0[[1]]$result[[1]]$coefficients)
  rn_mean <- paste0("mean_", rn)
  estimates_mean <- sapply(results_2_0[[i]]$result, function(x) x$coefficients[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  
  p_values_mean <- sapply(results_2_0[[i]]$result, function(x) x$coefficients[, 4])
  estimates_dispersion <- sapply(results_2_0[[i]]$result, function(x) x$dispersion)
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- unlist(estimates_dispersion)
  }
  
  est <- cbind(t(estimates_mean))
  colnames(est) <- c(rn_mean)
  est <- as.data.frame(est)
  est$dispersion <- estimates_dispersion
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_2_0 <- rbindlist(resi)
saveRDS(resi_2_0, "summarized_results/constant_fit_with_without.rds")

#################################################################
### Constant Dispersion Fit
### True mean model without feedback process
### True dispersion model without feedback process
#################################################################

results_2_1 <- files[grep("constant_fit_without_without_", files)]
index <- gsub("results//constant_fit_without_without_index_", "", results_2_1)
index <- as.numeric(gsub(".rds", "", index))
results_2_1 <- lapply(results_2_1, readRDS)
settings <- results_2_1[[1]]$settings[index,]
resi <- vector(mode = "list", length = 60)
for(i in seq_along(results_2_1)){
  rn <- rownames(results_2_1[[1]]$result[[1]]$coefficients)
  rn_mean <- paste0("mean_", rn)
  estimates_mean <- sapply(results_2_1[[i]]$result, function(x) x$coefficients[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  
  p_values_mean <- sapply(results_2_1[[i]]$result, function(x) x$coefficients[, 4])
  estimates_dispersion <- sapply(results_2_1[[i]]$result, function(x) x$dispersion)
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- unlist(estimates_dispersion)
  }
  
  est <- cbind(t(estimates_mean))
  colnames(est) <- c(rn_mean)
  est <- as.data.frame(est)
  est$dispersion <- estimates_dispersion
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_2_1 <- rbindlist(resi)
saveRDS(resi_2_1, "summarized_results/constant_fit_without_without.rds")



#################################################################
### Constant Dispersion Fit
### True mean model with feedback process
### True dispersion model with feedback process
#################################################################


results_2_2 <- files[grep("constant_fit_with_with_", files)]
index <- gsub("results//constant_fit_with_with_index_", "", results_2_2)
index <- as.numeric(gsub(".rds", "", index))
results_2_2 <- lapply(results_2_2, readRDS)
settings <- results_2_2[[1]]$settings[index,]
resi <- vector(mode = "list", length = 60)
for(i in seq_along(results_2_2)){
  rn <- rownames(results_2_2[[1]]$result[[1]]$coefficients)
  rn_mean <- paste0("mean_", rn)
  estimates_mean <- sapply(results_2_2[[i]]$result, function(x) x$coefficients[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  
  p_values_mean <- sapply(results_2_2[[i]]$result, function(x) x$coefficients[, 4])
  estimates_dispersion <- sapply(results_2_2[[i]]$result, function(x) x$dispersion)
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- unlist(estimates_dispersion)
  }
  
  est <- cbind(t(estimates_mean))
  colnames(est) <- c(rn_mean)
  est <- as.data.frame(est)
  est$dispersion <- estimates_dispersion
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_2_2 <- rbindlist(resi)
saveRDS(resi_2_2, "summarized_results/constant_fit_with_with.rds")


#################################################################
### Constant Dispersion Fit
### True mean model without feedback process
### True dispersion model with feedback process
#################################################################




# results 2_3
results_2_3 <- files[grep("constant_fit_without_with_", files)]
index <- gsub("results//constant_fit_without_with_index_", "", results_2_3)
index <- as.numeric(gsub(".rds", "", index))
results_2_3 <- lapply(results_2_3, readRDS)
settings <- results_2_3[[1]]$settings[index,]
resi <- vector(mode = "list", length = 60)
for(i in seq_along(results_2_3)){
  rn <- rownames(results_2_3[[1]]$result[[1]]$coefficients)
  rn_mean <- paste0("mean_", rn)
  estimates_mean <- sapply(results_2_3[[i]]$result, function(x) x$coefficients[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  
  p_values_mean <- sapply(results_2_3[[i]]$result, function(x) x$coefficients[, 4])
  estimates_dispersion <- sapply(results_2_3[[i]]$result, function(x) x$dispersion)
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- unlist(estimates_dispersion)
  }
  
  est <- cbind(t(estimates_mean))
  colnames(est) <- c(rn_mean)
  est <- as.data.frame(est)
  est$dispersion <- estimates_dispersion
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_2_3 <- rbindlist(resi)
saveRDS(resi_2_3, "summarized_results/constant_fit_without_with.rds")


#################################################################
### Time-varying Dispersion Fit
### True mean model with feedback process
### True dispersion model without feedback process
#################################################################


results_3_0 <- files[grep("tv_fit_with_without_", files)]
index <- gsub("results//tv_fit_with_without_index_", "", results_3_0)
index <- as.numeric(gsub(".rds", "", index))
results_3_0 <- lapply(results_3_0, readRDS)
settings <- results_3_0[[1]]$settings[index,]
resi <- vector(mode = "list", length = length(index))
for(i in seq_along(results_3_0)){
  rn <- rownames(results_3_0[[i]]$result[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_3_0[[i]]$result[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_3_0[[i]]$result, function(x) x$coefficients_mean[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  p_values_mean <- sapply(results_3_0[[i]]$result, function(x) x$coefficients_mean[, 4])
  if(is.list(p_values_mean)){
    p_values_mean <- t(do.call("rbind", p_values_mean))
  }
  estimates_dispersion <- sapply(results_3_0[[i]]$result, function(x) x$coefficients_dispersion[, 1])
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- t(do.call("rbind", estimates_dispersion))
  }
  p_values_dispersion <- sapply(results_3_0[[i]]$result, function(x) x$coefficients_dispersion[, 4])
  if(is.list(p_values_dispersion)){
    p_values_dispersion <- t(do.call("rbind", p_values_dispersion))
  }
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_3_0 <- rbindlist(resi)
saveRDS(resi_3_0, "summarized_results/tv_fit_with_without.rds")



#################################################################
### Time-varying Dispersion Fit
### True mean model without feedback process
### True dispersion model without feedback process
#################################################################


results_3_1 <- files[grep("tv_fit_without_without_", files)]
index <- gsub("results//tv_fit_without_without_index_", "", results_3_1)
index <- as.numeric(gsub(".rds", "", index))
results_3_1 <- lapply(results_3_1, readRDS)
settings <- results_3_1[[1]]$settings[index,]
resi <- vector(mode = "list", length = 40)
for(i in seq_along(results_3_1)){
  rn <- rownames(results_3_1[[i]]$result[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_3_1[[i]]$result[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_3_1[[i]]$result, function(x) x$coefficients_mean[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }

  p_values_mean <- sapply(results_3_1[[i]]$result, function(x) x$coefficients_mean[, 4])
  if(is.list(p_values_mean)){
    p_values_mean <- t(do.call("rbind", p_values_mean))
  }
  estimates_dispersion <- sapply(results_3_1[[i]]$result, function(x) x$coefficients_dispersion[, 1])
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- t(do.call("rbind", estimates_dispersion))
  }
  p_values_dispersion <- sapply(results_3_1[[i]]$result, function(x) x$coefficients_dispersion[, 4])
  if(is.list(p_values_dispersion)){
    p_values_dispersion <- t(do.call("rbind", p_values_dispersion))
  }
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_3_1 <- rbindlist(resi)
saveRDS(resi_3_1, "summarized_results/tv_fit_without_without.rds")

#################################################################
### Time-varying Dispersion Fit
### True mean model with feedback process
### True dispersion model with feedback process
#################################################################

results_3_2 <- files[grep("tv_fit_with_with_", files)]
index <- gsub("results//tv_fit_with_with_index_", "", results_3_2)
index <- as.numeric(gsub(".rds", "", index))
results_3_2 <- lapply(results_3_2, readRDS)
settings <- results_3_2[[1]]$settings[index,]
resi <- vector(mode = "list", length = 40)
for(i in seq_along(results_3_2)){
  rn <- rownames(results_3_2[[i]]$result[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_3_2[[i]]$result[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_3_2[[i]]$result, function(x) x$coefficients_mean[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  p_values_mean <- sapply(results_3_2[[i]]$result, function(x) x$coefficients_mean[, 4])
  if(is.list(p_values_mean)){
      p_values_mean <- t(do.call("rbind", p_values_mean))
  }
  estimates_dispersion <- sapply(results_3_2[[i]]$result, function(x) x$coefficients_dispersion[, 1])
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- t(do.call("rbind", estimates_dispersion))
  }
  p_values_dispersion <- sapply(results_3_2[[i]]$result, function(x) x$coefficients_dispersion[, 4])
  if(is.list(p_values_dispersion)){
    p_values_dispersion <- t(do.call("rbind", p_values_dispersion))
  }
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_3_2 <- rbindlist(resi)
saveRDS(resi_3_2, "summarized_results/tv_fit_with_with.rds")


#################################################################
### Time-varying Dispersion Fit
### True mean model without feedback process
### True dispersion model with feedback process
#################################################################

results_3_3 <- files[grep("tv_fit_without_with_", files)]
index <- gsub("results//tv_fit_without_with_index_", "", results_3_3)
index <- as.numeric(gsub(".rds", "", index))
results_3_3 <- lapply(results_3_3, readRDS)
settings <- results_3_3[[1]]$settings[index,]
resi <- vector(mode = "list", length = 40)
for(i in seq_along(results_3_3)){
  rn <- rownames(results_3_3[[i]]$result[[1]]$coefficients_mean)
  rn_mean <- paste0("mean_", rn)
  rn <- rownames(results_3_3[[i]]$result[[1]]$coefficients_dispersion)
  rn_dispersion <- paste0("dispersion_", rn)
  rn_dispersion_pvals <- paste0("pvals_disp_", rn)
  estimates_mean <- sapply(results_3_3[[i]]$result, function(x) x$coefficients_mean[, 1])
  if(is.list(estimates_mean)){
    estimates_mean <- t(do.call("rbind", estimates_mean))
  }
  p_values_mean <- sapply(results_3_3[[i]]$result, function(x) x$coefficients_mean[, 4])
  if(is.list(p_values_mean)){
    p_values_mean <- t(do.call("rbind", p_values_mean))
  }
  estimates_dispersion <- sapply(results_3_3[[i]]$result, function(x) x$coefficients_dispersion[, 1])
  if(is.list(estimates_dispersion)){
    estimates_dispersion <- t(do.call("rbind", estimates_dispersion))
  }
  p_values_dispersion <- sapply(results_3_3[[i]]$result, function(x) x$coefficients_dispersion[, 4])
  if(is.list(p_values_dispersion)){
    p_values_dispersion <- t(do.call("rbind", p_values_dispersion))
  }
  
  
  est <- cbind(t(estimates_mean), t(estimates_dispersion), t(p_values_dispersion))
  colnames(est) <- c(rn_mean, rn_dispersion, rn_dispersion_pvals)
  est <- as.data.frame(est)
  est$dim <- settings$dim[i]
  est$distribution <- settings$distribution[i]
  est$obs <- settings$obs[i]
  est$copula <- settings$copula[i]
  est$index <- index[i]
  resi[[i]] <- est
}
#
resi_3_3 <- rbindlist(resi)
saveRDS(resi_3_3, "summarized_results/tv_fit_without_with.rds")
