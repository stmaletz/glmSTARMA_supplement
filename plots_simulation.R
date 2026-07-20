#################################################################
### D. Simulation Study - Figures
#################################################################
### This script contains the R-Code to plot the results of the
### simulations.
#################################################################


library("ggplot2")
library("tidyr")
library("dplyr")
library("patchwork")


## Size configurations for plots
texts <- 30
stript <- 24
axis_tit <- 30
axis_txt <- 24
legend_tit <- 30
legend_txt <- 24

plot_width <- 15
plot_height <- 8

## Load results
res_2_0 <- readRDS("summarized_results/constant_fit_with_without.rds")
res_2_1 <- readRDS("summarized_results/constant_fit_without_without.rds")
res_2_2 <- readRDS("summarized_results/constant_fit_with_with.rds")
res_2_3 <- readRDS("summarized_results/constant_fit_without_with.rds")
res_3_0 <- readRDS("summarized_results/tv_fit_with_without.rds")
res_3_1 <- readRDS("summarized_results/tv_fit_without_without.rds")
res_3_2 <- readRDS("summarized_results/tv_fit_with_with.rds")
res_3_3 <- readRDS("summarized_results/tv_fit_without_with.rds")


res_2_0 <- subset(res_2_0, copula == "frank" & dim == 10)
res_2_1 <- subset(res_2_1, copula == "frank" & dim == 10)
res_2_2 <- subset(res_2_2, copula == "frank" & dim == 10)
res_2_3 <- subset(res_2_3, copula == "frank" & dim == 10)
res_3_0 <- subset(res_3_0, copula == "frank" & dim == 10)
res_3_1 <- subset(res_3_1, copula == "frank" & dim == 10)
res_3_2 <- subset(res_3_2, copula == "frank" & dim == 10)
res_3_3 <- subset(res_3_3, copula == "frank" & dim == 10)


## Define true parameters
true_values_normal_0 <- data.frame(
  parameter = c("Intercept", "past_obs_0", "past_obs_1", "X"),
  true_value = c(5, 0.4, 0.2, 2)
)
true_values_normal_0$parameter <- factor(
  true_values_normal_0$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1", "X")
)

true_values_normal_1 <- data.frame(
  parameter = c("Intercept", "past_mean_0", "past_mean_1", 
                "past_obs_0", "past_obs_1", "X"),
  true_value = c(5, 0.2, 0.1, 0.2, 0.1, 2)
)
true_values_normal_1$parameter <- factor(
  true_values_normal_1$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", 
             "past_obs_0", "past_obs_1", "X")
)


true_values_quasipoisson_0 <- data.frame(
  parameter = c("Intercept", "past_obs_0", "past_obs_1", "X"),
  true_value = c(0.6, 0.4, 0.2, 0.9)
)

true_values_quasipoisson_0$parameter <- factor(
  true_values_quasipoisson_0$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1", "X")
)

true_values_quasipoisson_1 <- data.frame(
  parameter = c("Intercept", "past_mean_0", "past_mean_1", 
                "past_obs_0", "past_obs_1", "X"),
  true_value = c(0.6, 0.2, 0.1, 0.2, 0.1, 0.9)
)

true_values_quasipoisson_1$parameter <- factor(
  true_values_quasipoisson_1$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", 
             "past_obs_0", "past_obs_1", "X")
)

# true values for dispersion parameters
true_values_dispersion_ARCH <- data.frame(
  parameter = c("Intercept", "past_obs_0", "past_obs_1"),
  true_value = c(0.5, 0.5, 0.2)
)
true_values_dispersion_ARCH$parameter <- factor(
  true_values_dispersion_ARCH$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1")
)

true_values_dispersion_GARCH <- data.frame(
  parameter = c("Intercept", "past_mean_0", "past_mean_1", 
                "past_obs_0", "past_obs_1"),
  true_value = c(0.5, 0.15, 0.05, 0.3, 0.2)
)
true_values_dispersion_GARCH$parameter <- factor(
  true_values_dispersion_GARCH$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", 
             "past_obs_0", "past_obs_1")
)



## results for res_x_0


# Transform the data to long format for mean parameters
params_mean_const_0 <- subset(res_2_0, select = c("mean_(Intercept)", "mean_past_mean_{s_0, t_1}", "mean_past_mean_{s_1, t_1}", 
                                                  "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_const_0$type <- "const"
params_mean_varying_0 <- subset(res_3_0, select = c("mean_(Intercept)", "mean_past_mean_{s_0, t_1}", "mean_past_mean_{s_1, t_1}", 
                                                  "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_varying_0$type <- "varying"
params_mean_0 <- rbind(params_mean_const_0, params_mean_varying_0) 
nam <- names(params_mean_0)
nam[1:6] <- c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1", "X")
names(params_mean_0) <- nam

params_long_0 <- params_mean_0 %>%
  pivot_longer(
    cols = c(Intercept, past_mean_0, past_mean_1, 
             past_obs_0, past_obs_1, X),
    names_to = "parameter",
    values_to = "value"
  )

params_long_0$parameter <- factor(
  params_long_0$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", 
             "past_obs_0", "past_obs_1", "X")
)

# Transform the data to long format for dispersion parameters
params_dispersion_0 <- subset(res_3_0, select = c("dispersion_(Intercept)", "dispersion_past_obs_{s_0, t_1}", "dispersion_past_obs_{s_1, t_1}", 
                                                    "distribution", "obs"))
#params_mean_varying_0$type <- "varying"
#params_mean_0 <- rbind(params_mean_const_0, params_mean_varying_0) 
nam <- names(params_dispersion_0)
nam[1:3] <- c("Intercept", "past_obs_0", "past_obs_1")
names(params_dispersion_0) <- nam


params_dispersion_long_0 <- params_dispersion_0 %>%
  pivot_longer(
    cols = c(Intercept, past_obs_0, past_obs_1),
    names_to = "parameter",
    values_to = "value"
  )

params_dispersion_long_0$parameter <- factor(
  params_dispersion_long_0$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1")
)


## Plot results for Quasipoisson distribution
mean_INGARCH_ARCH_poisson <- ggplot(
  subset(params_long_0, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_quasipoisson_1,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free_y",
             labeller = as_labeller(
               c(
                 "Intercept"   = "Intercept",
                 "past_mean_0" = "alpha[0]",
                 "past_mean_1" = "alpha[1]",
                 "past_obs_0"  = "beta[0]",
                 "past_obs_1"  = "beta[1]",
                 "X"           = "Covariate"
               ),
               label_parsed
             )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



qq_mean_INGARCH_ARCH_poisson <- ggplot(subset(params_long_0, distribution == "vquasipoisson" & type == "varying" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
               c(
                 "Intercept"   = "Intercept",
                 "past_mean_0" = "alpha[0]",
                 "past_mean_1" = "alpha[1]",
                 "past_obs_0"  = "beta[0]",
                 "past_obs_1"  = "beta[1]",
                 "X"           = "Covariate"
               ),
               label_parsed
             )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


dispersion_INGARCH_ARCH_poisson <- ggplot(
  subset(params_dispersion_long_0, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_ARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INGARCH_ARCH_poisson <- ggplot(subset(params_dispersion_long_0, distribution == "vquasipoisson"  & obs == 500), 
                                       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



## plot results for Normal distribution

mean_INGARCH_ARCH_normal <- ggplot(
  subset(params_long_0, distribution == "vnormal"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_normal_1,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


qq_mean_INGARCH_ARCH_normal <- ggplot(subset(params_long_0, distribution == "vnormal" & type == "varying" & obs == 500), 
                                       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


dispersion_INGARCH_ARCH_normal <- ggplot(
  subset(params_dispersion_long_0, distribution == "vnormal"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_ARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INGARCH_ARCH_normal <- ggplot(subset(params_dispersion_long_0, distribution == "vnormal"  & obs == 500), 
                                             aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


ggsave(mean_INGARCH_ARCH_poisson, filename = "mean_INGARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INGARCH_ARCH_poisson, filename = "dispersion_INGARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(mean_INGARCH_ARCH_normal, filename = "mean_INGARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INGARCH_ARCH_normal, filename = "dispersion_INGARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INGARCH_ARCH_poisson, filename = "qq_mean_INGARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INGARCH_ARCH_poisson, filename = "qq_dispersion_INGARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INGARCH_ARCH_normal, filename = "qq_mean_INGARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INGARCH_ARCH_normal, filename = "qq_dispersion_INGARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)


## results for res_x_1

## Transform the data to long format for mean parameters
params_mean_const_1 <- subset(res_2_1, select = c("mean_(Intercept)", "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_const_1$type <- "const"
params_mean_varying_1 <- subset(res_3_1, select = c("mean_(Intercept)", "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_varying_1$type <- "varying"
params_mean_1 <- rbind(params_mean_const_1, params_mean_varying_1) 
nam <- names(params_mean_1)
nam[1:4] <- c("Intercept", "past_obs_0", "past_obs_1", "X")
names(params_mean_1) <- nam
params_long_1 <- params_mean_1 %>%
  pivot_longer(
    cols = c(Intercept, past_obs_0, past_obs_1, X),
    names_to = "parameter",
    values_to = "value"
  )

params_long_1$parameter <- factor(
  params_long_1$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1", "X")
)

## Transform the data to long format for dispersion parameters
params_dispersion_1 <- subset(res_3_1, select = c("dispersion_(Intercept)", "dispersion_past_obs_{s_0, t_1}", "dispersion_past_obs_{s_1, t_1}", 
                                                    "distribution", "obs"))
#params_mean_varying_0$type <- "varying"
#params_mean_0 <- rbind(params_mean_const_0, params_mean_varying_0) 
nam <- names(params_dispersion_1)
nam[1:3] <- c("Intercept", "past_obs_0", "past_obs_1")
names(params_dispersion_1) <- nam


params_dispersion_long_1 <- params_dispersion_1 %>%
  pivot_longer(
    cols = c(Intercept, past_obs_0, past_obs_1),
    names_to = "parameter",
    values_to = "value"
  )

params_dispersion_long_1$parameter <- factor(
  params_dispersion_long_1$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1")
)



## Plot results for Quasipoisson distribution

mean_INARCH_ARCH_poisson <- ggplot(
  subset(params_long_1, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_quasipoisson_0,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_mean_INARCH_ARCH_poisson <- ggplot(subset(params_long_1, distribution == "vquasipoisson" & type == "varying" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



dispersion_INARCH_ARCH_poisson <- ggplot(
  subset(params_dispersion_long_1, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_ARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INARCH_ARCH_poisson <- ggplot(subset(params_dispersion_long_1, distribution == "vquasipoisson"  & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


## Plot results for Normal distribution

mean_INARCH_ARCH_normal <- ggplot(
  subset(params_long_1, distribution == "vnormal"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_normal_0,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


qq_mean_INARCH_ARCH_normal <- ggplot(subset(params_long_1, distribution == "vnormal" & type == "varying" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


dispersion_INARCH_ARCH_normal <- ggplot(
  subset(params_dispersion_long_1, distribution == "vnormal"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_ARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INARCH_ARCH_normal <- ggplot(subset(params_dispersion_long_1, distribution == "vnormal" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

## save plots
ggsave(mean_INARCH_ARCH_poisson, filename = "mean_INARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INARCH_ARCH_poisson, filename = "dispersion_INARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(mean_INARCH_ARCH_normal, filename = "mean_INARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INARCH_ARCH_normal, filename = "dispersion_INARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INARCH_ARCH_poisson, filename = "qq_mean_INARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INARCH_ARCH_poisson, filename = "qq_dispersion_INARCH_ARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INARCH_ARCH_normal, filename = "qq_mean_INARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INARCH_ARCH_normal, filename = "qq_dispersion_INARCH_ARCH_normal.pdf", width = plot_width, height = plot_height)


## results for res_x_2

## Transform the data to long format for mean parameters
params_mean_const_2 <- subset(res_2_2, select = c("mean_(Intercept)", "mean_past_mean_{s_0, t_1}", "mean_past_mean_{s_1, t_1}", 
                                                  "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_const_2$type <- "const"
params_mean_varying_2 <- subset(res_3_2, select = c("mean_(Intercept)", "mean_past_mean_{s_0, t_1}", "mean_past_mean_{s_1, t_1}", 
                                                  "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_varying_2$type <- "varying"
params_mean_2 <- rbind(params_mean_const_2, params_mean_varying_2) 
nam <- names(params_mean_2)
nam[1:6] <- c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1", "X")
names(params_mean_2) <- nam
params_long_2 <- params_mean_2 %>%
  pivot_longer(
    cols = c(Intercept, past_mean_0, past_mean_1, 
             past_obs_0, past_obs_1, X),
    names_to = "parameter",
    values_to = "value"
  )

params_long_2$parameter <- factor(
  params_long_2$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", 
             "past_obs_0", "past_obs_1", "X")
)


# Transform the data to long format for dispersion parameters
params_dispersion_2 <- subset(res_3_2, select = c("dispersion_(Intercept)", "dispersion_past_mean_{s_0, t_1}", "dispersion_past_mean_{s_1, t_1}", "dispersion_past_obs_{s_0, t_1}" ,"dispersion_past_obs_{s_1, t_1}", "distribution", "obs"))
#params_mean_varying_0$type <- "varying"
#params_mean_0 <- rbind(params_mean_const_0, params_mean_varying_0) 
nam <- names(params_dispersion_2)
nam[1:5] <- c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1")
names(params_dispersion_2) <- nam


params_dispersion_long_2 <- params_dispersion_2 %>%
  pivot_longer(
    cols = c(Intercept, past_mean_0, past_mean_1, past_obs_0, past_obs_1),
    names_to = "parameter",
    values_to = "value"
  )

params_dispersion_long_2$parameter <- factor(
  params_dispersion_long_2$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1")
)


## plot results for Quasipoisson distribution

mean_INGARCH_GARCH_poisson <- ggplot(
  subset(params_long_2, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_quasipoisson_1,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


qq_mean_INGARCH_GARCH_poisson <- ggplot(subset(params_long_2, distribution == "vquasipoisson" & type == "varying" & obs == 1000), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



dispersion_INGARCH_GARCH_poisson <- ggplot(
  subset(params_dispersion_long_2, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_GARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INGARCH_GARCH_poisson <- ggplot(subset(params_dispersion_long_2, distribution == "vquasipoisson" & obs == 100), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

## Plot results for Normal distribution
mean_INGARCH_GARCH_normal <- ggplot(
  subset(params_long_2, distribution == "vnormal"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_normal_1,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_mean_INGARCH_GARCH_normal <- ggplot(subset(params_long_2, distribution == "vnormal" & type == "varying" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

dispersion_INGARCH_GARCH_normal <- ggplot(
  subset(params_dispersion_long_2, distribution == "vnormal"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_GARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INGARCH_GARCH_normal <- ggplot(subset(params_dispersion_long_2, distribution == "vnormal" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


## save plots
ggsave(mean_INGARCH_GARCH_poisson, filename = "mean_INGARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INGARCH_GARCH_poisson, filename = "dispersion_INGARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(mean_INGARCH_GARCH_normal, filename = "mean_INGARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INGARCH_GARCH_normal, filename = "dispersion_INGARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INGARCH_GARCH_poisson, filename = "qq_mean_INGARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INGARCH_GARCH_poisson, filename = "qq_dispersion_INGARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INGARCH_GARCH_normal, filename = "qq_mean_INGARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INGARCH_GARCH_normal, filename = "qq_dispersion_INGARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)



## ## results for res_x_3

## Transform the data to long format for mean parameters

params_mean_const_3 <- subset(res_2_3, select = c("mean_(Intercept)", "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_const_3$type <- "const"
params_mean_varying_3 <- subset(res_3_3, select = c("mean_(Intercept)", "mean_past_obs_{s_0, t_1}" ,"mean_past_obs_{s_1, t_1}", "mean_X_{s_0}", "distribution", "obs"))
params_mean_varying_3$type <- "varying"
params_mean_3 <- rbind(params_mean_const_3, params_mean_varying_3) 
nam <- names(params_mean_3)
nam[1:4] <- c("Intercept", "past_obs_0", "past_obs_1", "X")
names(params_mean_3) <- nam
params_long_3 <- params_mean_3 %>%
  pivot_longer(
    cols = c(Intercept, past_obs_0, past_obs_1, X),
    names_to = "parameter",
    values_to = "value"
  )

params_long_3$parameter <- factor(
  params_long_3$parameter,
  levels = c("Intercept", "past_obs_0", "past_obs_1", "X")
)

## Transform the data to long format for dispersion parameters

params_dispersion_3 <- subset(res_3_3, select = c("dispersion_(Intercept)", "dispersion_past_mean_{s_0, t_1}", "dispersion_past_mean_{s_1, t_1}", "dispersion_past_obs_{s_0, t_1}" ,"dispersion_past_obs_{s_1, t_1}", "distribution", "obs"))
#params_mean_varying_0$type <- "varying"
#params_mean_0 <- rbind(params_mean_const_0, params_mean_varying_0) 
nam <- names(params_dispersion_3)
nam[1:5] <- c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1")
names(params_dispersion_3) <- nam


params_dispersion_long_3 <- params_dispersion_3 %>%
  pivot_longer(
    cols = c(Intercept, past_mean_0, past_mean_1, past_obs_0, past_obs_1),
    names_to = "parameter",
    values_to = "value"
  )

params_dispersion_long_3$parameter <- factor(
  params_dispersion_long_3$parameter,
  levels = c("Intercept", "past_mean_0", "past_mean_1", "past_obs_0", "past_obs_1")
)


## plot results for Quasipoisson distribution

mean_INARCH_GARCH_poisson <- ggplot(
  subset(params_long_3, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_quasipoisson_0,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_mean_INARCH_GARCH_poisson <- ggplot(subset(params_long_3, distribution == "vquasipoisson" & type == "varying" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



dispersion_INARCH_GARCH_poisson <- ggplot(
  subset(params_dispersion_long_3, distribution == "vquasipoisson"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_GARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )

qq_dispersion_INARCH_GARCH_poisson <- ggplot(subset(params_dispersion_long_3, distribution == "vquasipoisson" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


## Plot results for Normal distribution
mean_INARCH_GARCH_normal <- ggplot(
  subset(params_long_3, distribution == "vnormal"),
  aes(x = factor(obs), y = value, fill = type)
) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_hline(
    data = true_values_normal_0,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


qq_mean_INARCH_GARCH_normal <- ggplot(subset(params_long_3, distribution == "vnormal" & type == "varying" & obs == 500), 
                                       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )



dispersion_INARCH_GARCH_normal <- ggplot(
  subset(params_dispersion_long_3, distribution == "vnormal"),
  aes(x = factor(obs), y = value)
) + 
  geom_boxplot(position = position_dodge(width = 0.8),
               fill = "#00BFC4",      # ggplot Standardrot
               color = "black") +
  geom_hline(
    data = true_values_dispersion_GARCH,
    aes(yintercept = true_value),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  labs(fill = "Dispersion model", x = "Observation times", y = "Estimate") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )


qq_dispersion_INARCH_GARCH_normal <- ggplot(subset(params_dispersion_long_3, distribution == "vnormal" & obs == 500), 
       aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(
    c(
      "Intercept"   = "Intercept",
      "past_mean_0" = "alpha[0]",
      "past_mean_1" = "alpha[1]",
      "past_obs_0"  = "beta[0]",
      "past_obs_1"  = "beta[1]",
      "X"           = "Covariate"
    ),
    label_parsed
  )) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = texts),
    strip.text = element_text(size = stript),
    axis.title = element_text(size = axis_tit),
    axis.text = element_text(size = axis_txt),
    legend.title = element_text(size = legend_tit),
    legend.text = element_text(size = legend_txt)
  )




## save plots
ggsave(mean_INARCH_GARCH_poisson, filename = "mean_INARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INARCH_GARCH_poisson, filename = "dispersion_INARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(mean_INARCH_GARCH_normal, filename = "mean_INARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(dispersion_INARCH_GARCH_normal, filename = "dispersion_INARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INARCH_GARCH_poisson, filename = "qq_mean_INARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INARCH_GARCH_poisson, filename = "qq_dispersion_INARCH_GARCH_poisson.pdf", width = plot_width, height = plot_height)
ggsave(qq_mean_INARCH_GARCH_normal, filename = "qq_mean_INARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)
ggsave(qq_dispersion_INARCH_GARCH_normal, filename = "qq_dispersion_INARCH_GARCH_normal.pdf", width = plot_width, height = plot_height)


## Plots for the additional simulation:

resi_mean <- matrix(NA, 4, 500)
resi_dispersion <- matrix(NA, 3, 500)

q05_mean <- matrix(NA, 4, 500)
q95_mean <- matrix(NA, 4, 500)
q05_dispersion <- matrix(NA, 3, 500)
q95_dispersion <- matrix(NA, 3, 500)

for(i in seq(500)){
  x <- readRDS(paste0("results/varying_mean_index_", i, ".rds"))
  y <- x[[1]]
  esti <-  sapply(y, function(z) z$coefficients_mean$Estimate)
  resi_mean[, i] <- rowMeans(esti)
  q05_mean[, i] <- apply(esti, 1, quantile, probs = 0.05)
  q95_mean[, i] <- apply(esti, 1, quantile, probs = 0.95)
  esti <-  sapply(y, function(z) z$coefficients_dispersion$Estimate)
  resi_dispersion[, i] <- rowMeans(esti)
  q05_dispersion[, i] <- apply(esti, 1, quantile, probs = 0.05)
  q95_dispersion[, i] <- apply(esti, 1, quantile, probs = 0.95)
}

settings <- x$settings

row.names(resi_mean) <- paste0("mean_", c("intercept", "beta_0", "beta_1", "gamma"))
row.names(q05_mean) <- paste0("mean_q05_", c("intercept", "beta_0", "beta_1", "gamma"))
row.names(q95_mean) <- paste0("mean_q95_", c("intercept", "beta_0", "beta_1", "gamma"))
row.names(resi_dispersion) <- paste0("dispersion_", c("intercept", "beta_0", "beta_1"))
row.names(q05_dispersion) <-  paste0("dispersion_q05_", c("intercept", "beta_0", "beta_1"))
row.names(q95_dispersion) <-  paste0("dispersion_q95_", c("intercept", "beta_0", "beta_1"))

settings <- cbind(settings, t(resi_mean))
settings <- cbind(settings, t(q05_mean))
settings <- cbind(settings, t(q95_mean))
settings <- cbind(settings, t(resi_dispersion))
settings <- cbind(settings, t(q05_dispersion))
settings <- cbind(settings, t(q95_dispersion))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── 1. include true_value of the parameters ──────────────────────────────────────
MEAN_PARAMS <- list(
  list(mean = "mean_intercept", q05 = "mean_q05_intercept", q95 = "mean_q95_intercept",
       label = "Intercept",          true_value = NA_real_),
  list(mean = "mean_beta_0",    q05 = "mean_q05_beta_0",    q95 = "mean_q95_beta_0",
       label = expression(beta[0]),  true_value = 0.4),
  list(mean = "mean_beta_1",    q05 = "mean_q05_beta_1",    q95 = "mean_q95_beta_1",
       label = expression(beta[1]),  true_value = 0.2),
  list(mean = "mean_gamma",     q05 = "mean_q05_gamma",     q95 = "mean_q95_gamma",
       label = expression(gamma),    true_value = 0.9)
)
DISP_PARAMS <- list(
  list(mean = "dispersion_intercept", q05 = "dispersion_q05_intercept", q95 = "dispersion_q95_intercept",
       label = "Intercept",          true_value = 0.5),
  list(mean = "dispersion_beta_0",    q05 = "dispersion_q05_beta_0",    q95 = "dispersion_q95_beta_0",
       label = expression(beta[0]),  true_value = 0.5),
  list(mean = "dispersion_beta_1",    q05 = "dispersion_q05_beta_1",    q95 = "dispersion_q95_beta_1",
       label = expression(beta[1]),  true_value = 0.2)
)

# ── 2. to_long: true_value als Spalte mitnehmen ───────────────────────────────
to_long <- function(df, params) {
  do.call(rbind, lapply(params, function(p) {
    data.frame(
      intercept  = df$intercept,
      value      = df[[p$mean]],
      q05        = df[[p$q05]],
      q95        = df[[p$q95]],
      parameter  = factor(p$label, levels = sapply(params, `[[`, "label")),
      true_value = p$true_value          # neu
    )
  }))
}

# ── 3. plot_group: hline where true_value is not NA  ───────────────────────
plot_group <- function(df_long, title, color, nrow = 1, show_x = FALSE) {
  
  
  ref_df <- unique(df_long[, c("parameter", "true_value")])
  ref_df <- ref_df[!is.na(ref_df$true_value), ]
  
  p <- ggplot(df_long, aes(x = intercept)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "grey80", alpha = 0.7) +
    geom_line(aes(y = value), color = color, linewidth = 0.9) +
    facet_wrap(~ parameter, nrow = nrow, scales = "free_y", labeller = label_parsed) +
    labs(
      title = title,
      x     = if (show_x) "Intercept of the mean model" else NULL,
      y     = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(size = 12, face = "bold", color = color),
      strip.text       = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA),
      panel.grid.minor = element_blank(),
      axis.title.x     = element_text(size = 9, color = "grey40")
    )
  
  if (nrow(ref_df) > 0) {
    p <- p + geom_hline(
      data        = ref_df,
      aes(yintercept = true_value),
      color       = "firebrick",
      linetype    = "dashed",
      linewidth   = 0.65,
      inherit.aes = FALSE
    )
  }
  p
}

# ── plot_simulation ────────────────────────────────────────

plot_simulation <- function(data, distr, nobs) {
  df <- subset(data, data$distribution == distr & data$obs == nobs)
  if (nrow(df) == 0) stop("Keine Daten für diese Auswahl.")
  df <- df[order(df$intercept), ]
  
  mean_plot <- plot_group(to_long(df, MEAN_PARAMS),
                          title = "Mean model", color = "#1a4f82",
                          nrow = 2, show_x = FALSE)
  disp_plot <- plot_group(to_long(df, DISP_PARAMS),
                          title = "Dispersion model", color = "#7a1f1f",
                          nrow = 1, show_x = TRUE)
  
  dist_label <- c(vnormal = "Normal distribution", vquasipoisson = "Quasi-Poisson")[distr]
  (mean_plot / disp_plot) +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(
      title   = paste0("Model estimates - ", dist_label, "  |  n = ", nobs),
      caption = "Line: Mean  ·  Gray area: 5%-95% quantile  ·  Dashed: true value",
      theme   = theme(plot.title   = element_text(size = 14, hjust = 0.5, face = "bold"),
                      plot.caption = element_text(color = "grey50", hjust = 0.5))
    )
}


for(obi in c(50, 100, 250, 500, 1000)){
  plot_it <- plot_simulation(settings, "vquasipoisson", obi)
  ggsave(paste0("vquasipoisson_", obi, ".pdf"), plot_it, device = "pdf", width = 7, height = 5)
}


MEAN_PARAMS <- list(
  list(mean = "mean_intercept", q05 = "mean_q05_intercept", q95 = "mean_q95_intercept",
       label = "Intercept",          true_value = NA_real_),
  list(mean = "mean_beta_0",    q05 = "mean_q05_beta_0",    q95 = "mean_q95_beta_0",
       label = expression(beta[0]),  true_value = 0.4),      # ← dein wahrer Wert
  list(mean = "mean_beta_1",    q05 = "mean_q05_beta_1",    q95 = "mean_q95_beta_1",
       label = expression(beta[1]),  true_value = 0.2),
  list(mean = "mean_gamma",     q05 = "mean_q05_gamma",     q95 = "mean_q95_gamma",
       label = expression(gamma),    true_value = 2)
)



for(obi in c(50, 100, 250, 500, 1000)){
  plot_it <- plot_simulation(settings, "vnormal", obi)
  ggsave(paste0("vnormal_", obi, ".pdf"), plot_it, device = "pdf", width = 7, height = 5)
}
