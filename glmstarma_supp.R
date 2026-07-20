library("glmSTARMA")
library("sf")
library("spdep")
library("dplyr")
library("grid")

library("ggplot2")
library("tictoc")


## Required for second example:
## install.packages("devtools")
## devtools::install_github("andrewzm/STRbook")
library("STRbook")
library("rnaturalearth")
library("rnaturalearthdata")
library("Matrix")
library("starma")
library("GNAR")


#################################################################
### 6. Usage and Examples
#################################################################
### 6.1 Rotavirus infections in Germany
#################################################################


# We load the preprocessed dataset included in the glmSTARMA package.
# All pre-processing of the raw data can be found in the GitHub repository of
# this R package:
# https://github.com/stmaletz/glmSTARMA/blob/main/data-raw/rota/rota.R

dat <- load_data("rota")
rota <- dat$rota
gdr_feature <- dat$gdr_feature
population_germany <- dat$population_germany
W_germany <- dat$W_germany



## Figure 1: German cities and districs:
# Load shapefiles of Germany
# Note: Two regions were merged in 2021 (SK Eisenach and LK Wartburgkreis)
# As the shapefiles are older, we have to merge the regions.

shape <- read_sf("https://raw.githubusercontent.com/stmaletz/glmSTARMA/refs/heads/main/data-raw/rota/germany_county_shapes.json")
shape$RKI_NameDE <- gsub("\u0096", " ", shape$RKI_NameDE)
shape$RKI_NameDE <- gsub("-", " ", shape$RKI_NameDE)
wartburg_new <- shape %>% 
  filter(RKI_NameDE %in% c("SK Eisenach", "LK Wartburgkreis")) %>% 
  summarise(
    RKI_NameDE = "LK Wartburgkreis",
    RKI_ID = "16063",
    .groups = "drop"
  )
shape <- shape %>% 
  filter(!RKI_NameDE %in% c("SK Eisenach", "LK Wartburgkreis"))
shape <- bind_rows(shape, wartburg_new)

## Figure 1 (a): Log Mean Incidence of Rota Virus infections in Germany

shape$incidence <- log(rowMeans(rota / population_germany))

log_incidence <- ggplot(data = shape) +
  geom_sf(aes(fill = incidence), color = NA) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Log Incidence",
    direction = -1, 
    guide = guide_colorbar(
      barheight = unit(1.2, "cm"),
      barwidth  = unit(10, "cm"),
      title.position = "top"
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    legend.title = element_text(size = 36, face = "bold"),
    legend.text  = element_text(size = 30)
  )
# ggsave("log_incidence.pdf", log_incidence, width = 12, height = 16)

## Figure 1 (b): Neighbors up to order 2 of Schwalm Eder Kreis

shape$RKI_NameDE[38]
shape$id <- rep(0, 411)
shape$id[38] <- 1
shape$id[W_germany[[2]][38,] > 0] <- 2
shape$id[W_germany[[3]][38,] > 0] <- 3
shape$id <- factor(shape$id)

nb_german <- ggplot(data = shape) + geom_sf(aes(fill = id)) + 
  scale_fill_manual(values = c("0" = "transparent", 
                               "1" = "#141852", 
                               "2" = "#CB181D", 
                               "3" = "#58A5CC"))+ 
  theme_void() +
  theme(legend.position = "none")
# ggsave("neighbors_germany.pdf", nb_german, width = 12, height = 15)



##  Model fitting

covariates <- list(
       population = log(population_germany),
       gdr = TimeConstant(1 * (gdr_feature > 0)),
       season_cos = SpatialConstant(cos(2 * pi / 52 * 1:1252)),
       season_sin = SpatialConstant(sin(2 * pi / 52 * 1:1252)),
       vaccine_west = (gdr_feature == 0) %*% t(seq(ncol(rota)) >= 654),
       vaccine_east = (gdr_feature > 0) %*% t(seq(ncol(rota)) >= 654))


tic()
fit_poisson <- glmstarma(rota, list(past_obs = rep(2, 4)), wlist = W_germany,
                         covariates = covariates, family = vpoisson("log"))
toc()

tic()
fit_qpoisson <- glmstarma(rota, list(past_obs = rep(2, 4)), wlist = W_germany,
                         covariates = covariates, family = vquasipoisson("log"))
toc()

tic()
fit_nb <- glmstarma(rota, list(past_obs = rep(2, 4)), wlist = W_germany,
              covariates = covariates, family = vnegative.binomial("log"))
toc()

AIC(fit_poisson)
AIC(fit_qpoisson)
AIC(fit_nb)

#################################################################
### 6.2 Sea Surface Temperature Anomalies
#################################################################
## We only model the area in the latitude -29 to 29 and 
## longitude between 160 (east) and 240 (east)


## Figure 2: Pixel-wise mean of sea surface temperature anomalies in the Pacific

data(SST_df)
sst_subset <- subset(SST_df, date == unique(date)[1])
land_sf <- ne_countries(scale = "medium", returnclass = "sf")
plot_xlim <- range(sst_subset$lon)
plot_ylim <- range(sst_subset$lat)

land_sf <- ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_shift_longitude()
lat_min <- min(sst_subset$lat)
lat_max <- max(sst_subset$lat)
land_sf_cropped <- land_sf[land_sf$continent %in% c("Asia", "Oceania", 
                                                    "North America", 
                                                    "South America"), ]

mean_sst <- aggregate(sst ~ lon + lat, data = SST_df, FUN = mean)


area_sst <- ggplot() + 
    geom_raster(data = mean_sst, aes(x = lon, y = lat, fill = sst)) + 
    scale_fill_viridis_c() +
    coord_sf(
      xlim = range(SST_df$lon),
      ylim = range(SST_df$lat),
      expand = FALSE
    ) +
    geom_sf(data = land_sf_cropped,
            color = "gray30",
            fill = "gray80",
            linewidth = 0.2) +
    coord_sf(crs = st_crs(4326), 
             xlim = plot_xlim,
             ylim = plot_ylim) +
    theme_minimal() +
    labs(x = "Longitude", y = "Latitude") +
    geom_rect(
      aes(xmin = 160, ymin = -29, xmax = 240, ymax = 29),
      fill = "red",
      alpha = 0.1, # Transparency
      color = "red",
      linewidth = 0.5
    ) + 
    theme(plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0))
# ggsave("area_sst.pdf", area_sst, width = 11, height = 5.8)


## Model fitting:
# We now use the pre-processed data from the package.
# All pre-processing of the raw data can be found in the GitHub repository of
# this R package:
# https://github.com/stmaletz/glmSTARMA/blob/main/data-raw/SST/sst.R

dat <- load_data("sst")
SST <- dat$SST
W_directed <- dat$W_directed
locations <- dat$locations

times <- seq(from = as.Date("1970-01-01"), to = as.Date("2002-12-01"), by = "m")
times <- format(times, "%b %Y")
covariates <- list(trend = SpatialConstant(seq(times) / length(times)),
                longitude = TimeConstant(locations$lon / 360),
                season_cos = SpatialConstant(cos(2 * pi / 12 * seq(times))),
                season_sin = SpatialConstant(sin(2 * pi / 12 * seq(times))),
                abs_lat_inc = TimeConstant(pmin(abs(locations$lat), 6) / 90),
                abs_lat_dec = TimeConstant(pmax(abs(locations$lat) - 6, 0) / 90))

# Fitting takes around 50 seconds:
tic()
fit1 <- dglmstarma(SST, mean_model = list(past_obs = 4), 
                   dispersion_model = list(past_obs = 4),
                   mean_family = vnormal(),
                   dispersion_link = "log",
                   wlist = W_directed, 
                   mean_covariates = covariates, 
                   dispersion_covariates = covariates)
toc()

summary(fit1)


# Fit starma-model for reference (takes around 240 seconds)
W_starma <- lapply(W_directed, as.matrix)
tic()
sta <- starma(t(SST), wlist = W_starma, ar = matrix(1, nrow = 5), 
              ma = matrix(0, nrow = 1), iterate = 1)
toc()

## Comparison with GNAR

W <- W_starma[[2]] + W_starma[[3]] + W_starma[[4]] + W_starma[[5]]
W <- W / colSums(W)
W <- list(diag(1230), W)


tic()
fit <- glmstarma(SST, model = list(past_obs = 1), 
                 wlist = W, 
                 covariates = covariates, family = vnormal())
toc()


covariates_gnar <- list()
covariates_gnar$trend <- replicate(1230, covariates$trend)
covariates_gnar$longitude <- t(replicate(396, covariates$longitude))
covariates_gnar$season_cos <- (replicate(1230, covariates$season_cos))
covariates_gnar$season_sin <- (replicate(1230, covariates$season_sin))
covariates_gnar$abs_lat_inc <- t(replicate(396, covariates$abs_lat_inc))
covariates_gnar$abs_lat_dec <- t(replicate(396, covariates$abs_lat_dec))
covariates_gnar$intercept <- matrix(1, nrow = 396, ncol = 1230)


sst_net <- matrixtoGNAR(W[[2]])


tic()
gnar_fit <- GNARXfit(vts = t(SST), net = sst_net, globalalpha = TRUE, 
                  alphaOrder = 1,
                  betaOrder = 1, xvts = covariates_gnar, 
                  lambdaOrder = rep(0, length(covariates_gnar)))
toc()
summary(gnar_fit)



#################################################################
### Appendix
#################################################################
### C. Simulation Examples
#################################################################

## Figure 3: Visualization of neighborhoods on a uniform 10 x 10 grid

W <- generateW("rectangle", dim = 100, maxOrder = 2, width = 10)
df <- as.data.frame(as.table(t(W[[2]])))
colnames(df) <- c("x", "y", "value")
# revert y-axis
df$y <- max(as.numeric(df$y)) - as.numeric(df$y) + 1
df$value <- factor(df$value,
                   levels = c(0, 0.25, 1/3, 0.5),
                   labels = c("0", "0.25", "1/3", "0.5"))

W1 <- ggplot(df, aes(x = as.numeric(x),
                     y = as.numeric(y),
                     fill = value)) +
  geom_tile() +
  scale_fill_manual(
    values = c(
      "0" = "transparent",           
      "0.25" = "#e41a1c",  
      "1/3" = "#377eb8",  
      "0.5" = "#4daf4a" 
    ),
    breaks = c("0.25", "1/3", "0.5"), 
    drop = TRUE
  ) +
  coord_fixed(expand = FALSE) +
  labs(fill = "Value") + 
  theme_void() + theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 48, face = "bold"), 
    legend.text = element_text(size = 36),                
    legend.key.size = unit(1.2, "cm"),                    
    legend.spacing.x = unit(0.5, "cm")
  )

# ggsave("W1.pdf", W1, width = 10, height = 13.25)


df <- as.data.frame(as.table(t(W[[3]])))
colnames(df) <- c("x", "y", "value")
# revert y-axis
df$y <- max(as.numeric(df$y)) - as.numeric(df$y) + 1
df$value <- factor(df$value,
                   levels = c(0, 0.25, 0.5, 1),
                   labels = c("0", "0.25", "0.5", "1"))

W2 <- ggplot(df, aes(x = as.numeric(x),
                     y = as.numeric(y),
                     fill = value)) +
  geom_tile() +
  scale_fill_manual(
    values = c(
      "0" = "transparent",        
      "0.25" = "#e41a1c", 
      "0.5" = "#377eb8",  
      "1" = "#4daf4a"   
    ),
    breaks = c("0.25", "0.5", "1"), 
    drop = TRUE
  ) +
  coord_fixed(expand = FALSE) +
  labs(fill = "Value") + 
  theme_void() + theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 48, face = "bold"),
    legend.text = element_text(size = 36),                
    legend.key.size = unit(1.2, "cm"),                  
    legend.spacing.x = unit(0.5, "cm")
  )


ggsave("W2.pdf", W2, width = 10, height = 13.25)

#################################################################
### C.1 Linear PSTARMA Process
#################################################################


W <- generateW("rectangle", dim = 100, maxOrder = 2L, width = 10L)

## Sampling using marginal Poisson distributions

set.seed(42)
sim <- glmstarma.sim(
  150,
  parameters = list(
    intercept = 1,
    past_mean = 0.2,
    past_obs  = cbind(c(0.3, 0.2, 0.1),
                      c(0.1, 0, 0))),
  model = list(
    intercept = "homogeneous",
    past_mean= 0,
    past_obs = c(2, 0),
    past_obs_time_lags = c(1, 7)),
  family = vpoisson("identity", copula = "frank", copula_param = 2, 
                    sampling_method = "poisson_process"),
  wlist = W
)

mean(sim$observations)
mean((sim$observations - sim$link_values)^2 / sim$link_values)


#################################################################
### C.2 Continous-data Process 
#################################################################

covariates_mean <- list(
    sine   = SpatialConstant(sin(2 * pi / 52 * seq(250))),
    cosine = SpatialConstant(cos(2 * pi / 52 * seq(250))))

covariates_dispersion <- list(
    sine   = SpatialConstant(exp(sin(2 * pi / 26 * seq(250)))),
    column = TimeConstant(c(1:10 %x% rep(1, 10))))

mean_model <- list(
    intercept  = "homogeneous",
    past_obs   = matrix(c(1, 1, 1), ncol = 1),
    covariates = c(0, 0))

mean_parameters <- list(
    intercept  = 0.1,
    past_obs   = matrix(c(0.3, 0.2, 0.1), ncol = 1),
    covariates = matrix(c(0.3, -0.2), nrow = 1))

dispersion_model <- list(
    intercept  = "homogeneous",
    past_mean  = matrix(c(1, 0, 1), ncol = 1),
    past_obs   = 0,
    covariates = c(0, 0))

dispersion_parameters <- list(
    intercept  = 2,
    past_mean  = matrix(c(0.1, 0, 0.05), ncol = 1),
    past_obs   = matrix(0.3),
    covariates = matrix(c(3, 0.5), nrow = 1))

sim <- dglmstarma.sim(
    250,
    mean_parameters,
    dispersion_parameters,
    mean_model,
    dispersion_model,
    mean_family = vnormal("log", copula = "joe", copula_param = 1.5),
    dispersion_link      = "identity",
    wlist                = W,
    mean_covariates      = covariates_mean,
    dispersion_covariates = covariates_dispersion)


#################################################################
### D. Simulation Study 
#################################################################

# The code for the simulation studies is in the simulation.R file
# If you want to reproduce the results, we recommend doing so on a
# high-performance computing cluster.

# The .rda-files in the subdirectory "results" contain the raw results
# from the simulations.
# The .rda-files in the summarized_results contain the results summarized
# by the summarize_results.R script
# The plots in the article can be reproduced with the plots_simulation.R


### Code for Table 5

res_1_0 <- readRDS("constant_dispersion_mean_without_feedback.rds")
res_1_1 <- readRDS("constant_dispersion_mean_with_feedback.rds")
res_1_0 <- subset(res_1_0, copula == "frank" & dim == 10)
res_1_1 <- subset(res_1_1, copula == "frank" & dim == 10)

aggregate(`pvals_disp_past_obs_{s_0, t_1}` ~ obs + distribution, function(x) mean(x < 0.05), data = res_1_1)
aggregate(`pvals_disp_past_obs_{s_1, t_1}` ~ obs + distribution, function(x) mean(x < 0.05), data = res_1_1)

## ARMA
aggregate(`pvals_disp_past_obs_{s_0, t_1}` ~ obs + distribution, function(x) mean(x < 0.05), data = res_1_0)
aggregate(`pvals_disp_past_obs_{s_1, t_1}` ~ obs + distribution, function(x) mean(x < 0.05), data = res_1_0)





































set.seed(42)
sim <- glmstarma.sim(
  150,
  parameters = list(
    intercept = 1,
    past_mean = 0.2,
    past_obs  = cbind(c(0.3, 0.2, 0.1),
                      c(0.1, 0, 0))),
  model = list(
    intercept = "homogeneous",
    past_mean= 0,
    past_obs = c(2, 0),
    past_obs_time_lags = c(1, 7)),
  family = vpoisson("identity", copula = "frank", copula_param = 2, 
                    sampling_method = "poisson_process"),
  wlist = W
)



covariates_mean <- list(
  sine   = SpatialConstant(sin(2 * pi / 52 * seq(250))),
  cosine = SpatialConstant(cos(2 * pi / 52 * seq(250))))

covariates_dispersion <- list(
  sine   = SpatialConstant(exp(sin(2 * pi / 26 * seq(250)))),
  column = TimeConstant(c(1:10 %x% rep(1, 10))))

mean_model <- list(
  intercept  = "homogeneous",
  past_obs   = matrix(c(1, 1, 1), ncol = 1),
  covariates = c(0, 0))

mean_parameters <- list(
  intercept  = 0.1,
  past_obs   = matrix(c(0.3, 0.2, 0.1), ncol = 1),
  covariates = matrix(c(0.3, -0.2), nrow = 1))


dispersion_model <- list(
  intercept  = "homogeneous",
  past_mean  = matrix(c(1, 0, 1), ncol = 1),
  past_obs   = 0,
  covariates = c(0, 0))

dispersion_parameters <- list(
  intercept  = 2,
  past_mean  = matrix(c(0.1, 0, 0.05), ncol = 1),
  past_obs   = matrix(0.3),
  covariates = matrix(c(3, 0.5), nrow = 1))


sim <- dglmstarma.sim(
  250,
  mean_parameters,
  dispersion_parameters,
  mean_model,
  dispersion_model,
  mean_family = vnormal("log", copula = "joe", copula_param = 1.5),
  dispersion_link      = "identity",
  wlist                = W,
  mean_covariates      = covariates_mean,
  dispersion_covariates = covariates_dispersion)


set.seed(42)
covariates <- t(replicate(
  100,
  arima.sim(n = 1000,
            list(ar = c(0.89, -0.3), ma = c(-0.1, 0.28)), 
            rand.gen = runif)))
covariates <- covariates / max(covariates)
covariates[covariates < 0] <- 0

