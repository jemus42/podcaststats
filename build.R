#! /usr/bin/env Rscript
# Build things

source("get_data.R")

rmarkdown::render_site(input = ".")
