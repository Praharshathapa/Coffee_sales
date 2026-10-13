# Run from the project root:  Rscript tests/testthat.R
library(testthat)
test_dir("tests/testthat", reporter = c("summary", "fail"), stop_on_failure = TRUE)
