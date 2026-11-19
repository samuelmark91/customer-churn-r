# Run via source('tests/test_metrics.R') from the project root.
# Tiny known examples validate metrics; these are not project performance results.
source("R/helpers.R")
y <- factor(c("yes", "no", "yes", "no"), levels = c("yes", "no"))
m <- classification_metrics(y, c(0.9, 0.7, 0.6, 0.1))
stopifnot(isTRUE(all.equal(unname(m["ROC_AUC"]), 0.75)),
          isTRUE(all.equal(unname(m["Precision"]), 2 / 3)),
          m["Recall"] == 1, m["TP"] == 2, m["FP"] == 1,
          isTRUE(all.equal(average_precision(y, c(0.9, 0.7, 0.6, 0.1)), 5 / 6)),
          average_precision(y, rep(0.5, 4)) == 0.5,
          average_precision(y, c(1, 0, 1, 0)) == 1)
cap <- capacity_metrics(y, c(0.9, 0.7, 0.6, 0.1), 0.5)
stopifnot(cap["SelectedRecords"] == 2,
          cap["PrecisionAtCapacity"] == 0.5,
          cap["RecallAtCapacity"] == 0.5,
          cap["LiftAtCapacity"] == 1)
invalid <- try(average_precision(y, c(2, 0, 0, 0)), silent = TRUE)
stopifnot(inherits(invalid, "try-error"))
message("Metric checks passed.")
