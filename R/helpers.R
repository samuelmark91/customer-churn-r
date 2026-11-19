# All probabilities below are P(churn == 'yes').
check_scores <- function(actual, probability) {
  if (!length(actual) || length(actual) != length(probability) ||
      anyNA(actual) || anyNA(probability) ||
      !all(as.character(actual) %in% c("yes", "no")) ||
      !is.numeric(probability) || any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1)) {
    stop("Supply matching nonmissing yes/no outcomes and probabilities in [0,1].")
  }
}

# Non-interpolated average precision. Process tied scores as a group so results
# do not depend on the ordering of observations with identical probabilities.
average_precision <- function(actual, probability) {
  check_scores(actual, probability)
  positive <- as.character(actual) == "yes"
  n_positive <- sum(positive)
  if (!n_positive) return(NA_real_)
  index <- order(probability, decreasing = TRUE)
  y <- positive[index]
  score <- probability[index]
  endpoints <- c(which(diff(score) != 0), length(score))
  tp <- cumsum(y)[endpoints]
  recall <- tp / n_positive
  precision <- tp / endpoints
  sum(diff(c(0, recall)) * precision)
}

roc_object <- function(actual, probability) {
  check_scores(actual, probability)
  pROC::roc(actual, probability, levels = c("no", "yes"),
            direction = "<", quiet = TRUE)
}

classification_metrics <- function(actual, probability, threshold = 0.5) {
  check_scores(actual, probability)
  stopifnot(length(threshold) == 1L, is.finite(threshold),
            threshold >= 0, threshold <= 1)
  positive <- as.character(actual) == "yes"
  predicted <- probability >= threshold
  tp <- sum(positive & predicted)
  fp <- sum(!positive & predicted)
  tn <- sum(!positive & !predicted)
  fn <- sum(positive & !predicted)
  divide <- function(a, b) if (b == 0) NA_real_ else a / b
  c(Accuracy = mean(positive == predicted),
    Precision = divide(tp, tp + fp),
    Recall = divide(tp, tp + fn),
    Specificity = divide(tn, tn + fp),
    F1 = divide(2 * tp, 2 * tp + fp + fn),
    ROC_AUC = if (length(unique(positive)) == 2L)
      as.numeric(pROC::auc(roc_object(actual, probability))) else NA_real_,
    AveragePrecision = average_precision(actual, probability),
    BrierScore = mean((probability - as.numeric(positive))^2),
    TP = tp, FP = fp, TN = tn, FN = fn)
}

# Capacity is fixed before inspecting test outcomes. Ties use the stable source
# row order; customer_row is a traceability index and is never a predictor.
capacity_metrics <- function(actual, probability, fraction = 0.2) {
  check_scores(actual, probability)
  stopifnot(length(fraction) == 1L, is.finite(fraction), fraction > 0, fraction <= 1)
  positive <- as.character(actual) == "yes"
  k <- max(1L, ceiling(length(actual) * fraction))
  selected <- order(-probability, seq_along(probability))[seq_len(k)]
  tp <- sum(positive[selected])
  precision <- tp / k
  prevalence <- mean(positive)
  c(SelectedRecords = k, ObservedChurnersInSelection = tp,
    PrecisionAtCapacity = precision,
    RecallAtCapacity = if (sum(positive)) tp / sum(positive) else NA_real_,
    LiftAtCapacity = if (prevalence > 0) precision / prevalence else NA_real_)
}

# Training-data CV scoring: first factor level is explicitly 'yes'.
cv_summary <- function(data, lev = NULL, model = NULL) {
  stopifnot(setequal(as.character(lev), c("yes", "no")))
  positive <- data$obs == "yes"
  predicted <- data$pred == "yes"
  c(ROC = as.numeric(pROC::auc(roc_object(data$obs, data$yes))),
    AveragePrecision = average_precision(data$obs, data$yes),
    Recall = sum(positive & predicted) / sum(positive))
}

write_table <- function(x, filename) {
  write.csv(x, file.path("outputs", filename), row.names = FALSE, na = "")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

html_table <- function(x, digits = 3) {
  x <- as.data.frame(x)
  for (j in seq_along(x)) {
    if (is.numeric(x[[j]])) x[[j]] <- round(x[[j]], digits)
  }
  header <- paste0("<th>", html_escape(names(x)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(x)), function(i) {
    values <- vapply(x, function(column) as.character(column[i]), character(1))
    paste0("<tr><td>", paste(html_escape(values), collapse = "</td><td>"), "</td></tr>")
  }, character(1))
  paste0("<div class='table-wrap'><table><thead><tr>", header,
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}
