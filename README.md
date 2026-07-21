This repository contains additional code to reproduce the results from the article:
- Maletz, S., Fokianos, K., & Fried, R. (2026). glmSTARMA -- An R-Package for fitting autoregressive spatio-temporal models following generalized linear models. [DOI: 10.48550/arXiv.2607.08276](https://doi.org/10.48550/arXiv.2607.08276)

Files are as follows:
- `glmstarma_supp.R` contains the R-Code to reproduce the results/figures/tables from the main part and the appendix (excluding simulations)
- `simulations.R` contains the R-Code to run the simulation studies in the appendix (HPC Cluster recommended) and saves them in the `results` directory
- `summarize_results.R` summarizes the results in long `data.frame` objects and stores them in the `summarized_results` directory
- `plots_simulation.R` produces the plots from the simulations, included in the appendix of the article.

