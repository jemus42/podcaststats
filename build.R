#! /usr/bin/env Rscript
# Build site only. Use get_data.R to refetch data
renv::restore()

rmarkdown::render_site(input = ".")
