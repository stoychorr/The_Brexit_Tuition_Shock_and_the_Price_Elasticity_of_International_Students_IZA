if (!file.exists("replication.Rmd")) {
  stop("Run this file from the Brexit_IZA_replication package root.")
}

dir.create(file.path("output", "audit"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)

rmarkdown::render(
  input = "replication.Rmd",
  output_file = "replication_tables_and_figures.html",
  output_dir = "output",
  clean = TRUE,
  envir = new.env(parent = globalenv())
)

capture.output(
  sessionInfo(),
  file = file.path("output", "audit", "sessionInfo.txt")
)
