# Write the scraped datasets to disk as downloadable files for the "Raw Data"
# page (data.qmd). Returns the vector of written paths so the `data_files`
# target can track them with format = "file" and the site render can depend on
# them via tar_read(data_files).
#
# RDS is written for every dataset (preserves list-columns like ATP's per-episode
# shownotes links). CSV is written only for datasets without list-columns, since
# CSV cannot represent them; any list-columns are dropped before writing.

export_datasets <- function(
  atp,
  relay_episodes,
  incomparable_episodes,
  dir = here::here("data")
) {
  fs::dir_create(dir)

  datasets <- list(
    atp = atp,
    relay_episodes = relay_episodes,
    incomparable_episodes = incomparable_episodes
  )

  paths <- character()
  for (name in names(datasets)) {
    df <- datasets[[name]]

    rds_path <- fs::path(dir, name, ext = "rds")
    saveRDS(df, rds_path)
    paths <- c(paths, rds_path)

    flat <- df[, !vapply(df, is.list, logical(1)), drop = FALSE]
    if (ncol(flat) == ncol(df)) {
      csv_path <- fs::path(dir, name, ext = "csv")
      utils::write.csv(flat, csv_path, row.names = FALSE)
      paths <- c(paths, csv_path)
    }
  }

  unname(paths)
}
