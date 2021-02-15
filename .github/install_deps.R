# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightPlots@dev",
    "iNZightVIT/iNZightMR@2.2.5"     # dependency of iNZightPlots
)

options(
    repos = c(
        RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

remotes::install_github(github_deps,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_deps(dependencies = TRUE,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_cran("rcmdcheck",
    INSTALL_opts = c("--no-multiarch")
)
