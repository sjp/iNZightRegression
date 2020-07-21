# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@release/1.9",
    "iNZightVIT/iNZightPlots@release/2.12"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
