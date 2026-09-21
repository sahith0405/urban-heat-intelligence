# ==========================================
# Urban Heat Intelligence
# R Environment Test
# ==========================================

cat("====================================\n")
cat(" Urban Heat Intelligence\n")
cat(" R Environment Test\n")
cat("====================================\n\n")

cat("R Version:\n")
print(R.version.string)

cat("\nTesting required packages...\n\n")

packages <- c(
  "tidyverse",
  "lubridate",
  "sf",
  "jsonlite",
  "randomForest",
  "xgboost",
  "caret"
)

for (package in packages) {
  if (requireNamespace(package, quietly = TRUE)) {
    cat("✓", package, "installed\n")
  } else {
    cat("✗", package, "NOT installed\n")
  }
}

cat("\nEnvironment test completed successfully.\n")
