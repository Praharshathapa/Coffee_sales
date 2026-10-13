# Posit Connect deploys exactly the files listed in Dashboard/manifest.json.
# A file missing from the manifest, or edited after the manifest was written,
# would break or silently change the deployed app. Regenerate with:
#   rsconnect::writeManifest("Dashboard", appFiles = ...)  (see README)

manifest <- jsonlite::fromJSON(file.path(app_dir, "manifest.json"))

test_that("every app source file is listed in the manifest", {
  app_files <- c("global.R", "ui.R", "server.R", file.path("lib", list.files(file.path(app_dir, "lib"), pattern = "\\.R$")))
  expect_true(all(app_files %in% names(manifest$files)),
              info = paste("Missing:", paste(setdiff(app_files, names(manifest$files)), collapse = ", ")))
})

test_that("manifest checksums match the files on disk", {
  for (f in names(manifest$files)) {
    path <- file.path(app_dir, f)
    expect_true(file.exists(path), info = f)
    expect_equal(unname(tools::md5sum(path)), manifest$files[[f]]$checksum, info = paste(f, "changed; regenerate the manifest"))
  }
})

test_that("every package the app attaches is in the manifest", {
  attached <- c("shiny", "shinydashboard", "arules", "plotly", "dplyr", "ggplot2", "readxl", "scales", "RColorBrewer", "DT")
  expect_true(all(attached %in% names(manifest$packages)))
})
