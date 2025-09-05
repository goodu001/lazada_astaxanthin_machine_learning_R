# lazada_astaxanthin_segmentation.R
# Read CSV, clean, feature-engineer, run K-Means, save segments
# Usage: edit file_path below and run in R / RStudio

# --- Packages --------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, stringr, readr, janitor, scales, broom, factoextra, cluster)

# --- Parameters / file path ------------------------------------------------
# Edit this path if your file is elsewhere.
file_path <- "C:/Users/User/OneDrive/Desktop/Web Scraping/lazada_astaxanthin_allpages - lazada_astaxanthin_allpages.csv"

# Output path
out_csv <- file.path(dirname(file_path), "lazada_astaxanthin_segments.csv")

# --- Helper functions -----------------------------------------------------
# Parse price strings: remove currency symbols, commas, non-number, return numeric or NA
parse_price <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Common patterns: "฿1,234", "1,234", "1,234 - 2,345", "฿1,200\xa0", etc.
  # Take lower bound if a range found
  x2 <- str_replace_all(x, "\u00A0", " ")    # non-breaking space
  x2 <- str_remove_all(x2, "[^0-9\\-\\.]")   # keep digits, dash, dot
  # If dash range e.g. "1234-2345", take first part
  x2 <- str_trim(x2)
  x2 <- ifelse(str_detect(x2, "-"), str_extract(x2, "^[0-9\\.]+"), x2)
  as.numeric(x2)
}

# Parse sold text into approximate numeric sold_est
# Examples: "ขายแล้ว 1.2k", "ขายแล้ว 50", "110 sold", "ขายแล้ว 5k+"
parse_sold_est <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_to_lower(x)
  # Remove non-ASCII thai words, keep digits, k, m, plus signs
  # Extract first numeric token with optional multiplier (k, m)
  m <- str_match(x, "([0-9]+\\.?[0-9]*)\\s*([km]?)")
  # m[,2] numeric, m[,3] multiplier
  num <- suppressWarnings(as.numeric(m[,2]))
  mult <- m[,3]
  num[is.na(num)] <- 0
  mult <- ifelse(is.na(mult), "", mult)
  mult_val <- ifelse(mult == "k", 1e3, ifelse(mult == "m", 1e6, 1))
  sold <- num * mult_val
  # If nothing matched, try to extract integer anywhere
  sold[sold == 0] <- as.numeric(str_extract(x[sold == 0], "[0-9]+")) %>% replace_na(0)
  sold
}

# Clean text helper
clean_text <- function(x) {
  x %>% as.character() %>% str_squish() %>% str_replace_all("[\\r\\n\\t]", " ")
}

# --- Read data ------------------------------------------------------------
# Try to read CSV; set encoding to handle Thai characters
df_raw <- read_csv(file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
message("Loaded data: ", nrow(df_raw), " rows and ", ncol(df_raw), " columns.")
message("Columns: ", paste(names(df_raw), collapse = ", "))

# Inspect common column names and attempt to map them
# We'll look for fields: 'name', 'price', 'sold', 'sold_vol', 'sold_est' etc.
col_lower <- tolower(names(df_raw))
get_col <- function(possible_names) {
  idx <- which(col_lower %in% possible_names)
  if (length(idx) >= 1) return(names(df_raw)[idx[1]])
  # try contains
  for (nm in possible_names) {
    idx2 <- which(str_detect(col_lower, nm))
    if (length(idx2) >= 1) return(names(df_raw)[idx2[1]])
  }
  return(NA_character_)
}

name_col  <- get_col(c("name","title","product_name","product"))
price_col <- get_col(c("price","listing_price","current_price","sale_price"))
sold_col  <- get_col(c("sold","sold_vol","sold_est","sold_text","sold_count","sales","sold_volume"))

# If not found, attempt common alternatives or raise note
if (is.na(name_col)) stop("Could not find product name column. Columns: ", paste(names(df_raw), collapse=", "))
if (is.na(price_col)) {
  warning("Could not find price column by common names. Attempting to guess 'price'.")
  price_col <- "price"
}
if (is.na(sold_col)) {
  warning("Could not find sold column by common names. Proceeding without sold volume; sold_est will be 0.")
  sold_col <- NULL
}

# --- Build cleaned dataset ------------------------------------------------
df <- df_raw %>%
  rename_with(~., all_of(names(df_raw))) %>% # ensure names preserved
  mutate(
    name = clean_text(.data[[name_col]]),
    price_raw = if (!is.null(price_col) && price_col %in% names(df_raw)) as.character(.data[[price_col]]) else NA_character_,
    sold_raw = if (!is.null(sold_col) && sold_col %in% names(df_raw)) as.character(.data[[sold_col]]) else NA_character_
  ) %>%
  select(-one_of(c(name_col, price_col, sold_col)), everything())

# Parse numeric price and sold estimate
df <- df %>%
  mutate(
    price = parse_price(price_raw),
    sold_est = if (!is.null(sold_col) && sold_col %in% names(df_raw)) parse_sold_est(sold_raw) else 0
  )

# If price has NA but there is another column like 'price_min' or 'price_display' try to coalesce
possible_price_cols <- names(df_raw)[str_detect(tolower(names(df_raw)), "price")]
if (all(is.na(df$price)) && length(possible_price_cols) > 0) {
  for (pc in possible_price_cols) {
    try_price <- parse_price(df_raw[[pc]])
    if (sum(!is.na(try_price)) > 0) {
      df$price <- coalesce(df$price, try_price)
    }
  }
}

# Basic summary
message("After parsing: non-missing price rows = ", sum(!is.na(df$price)), " ; sold_est non-zero = ", sum(df$sold_est > 0))

# --- Feature engineering --------------------------------------------------
# Create log transforms and other numeric features used for clustering
df_features <- df %>%
  mutate(
    price = ifelse(!is.na(price) & price > 0, price, NA_real_),
    sold = ifelse(!is.na(sold_est), sold_est, 0),
    log_price = ifelse(!is.na(price) & price > 0, log10(price + 1), NA_real_),
    log_sold = log10(sold + 1),
    price_per_sold = ifelse(sold > 0, price / sold, NA_real_),
    log_price_per_sold = ifelse(!is.na(price_per_sold) & price_per_sold > 0, log10(price_per_sold + 1), NA_real_)
  )

# Choose features for clustering. Use log transforms to reduce skew.
cluster_df <- df_features %>%
  select(name, price, sold, log_price, log_sold, price_per_sold, log_price_per_sold)

# Drop rows with no usable numeric data
cluster_input <- cluster_df %>%
  filter(!is.na(log_price) | !is.na(log_sold)) %>%
  mutate_at(vars(log_price, log_sold, log_price_per_sold), ~replace_na(., 0))

message("Rows used for clustering: ", nrow(cluster_input))

# --- Scale features -------------------------------------------------------
# We'll scale numeric features before kmeans
feat_mat <- cluster_input %>% select(log_price, log_sold, log_price_per_sold) %>% as.matrix()
feat_scaled <- scale(feat_mat)

# --- Choose k (elbow + silhouette quick check) ----------------------------
# Compute total within-cluster sum of squares for k = 1..8
wss <- map_dbl(1:8, function(k) {
  km <- kmeans(feat_scaled, centers = k, nstart = 25, iter.max = 100)
  km$tot.withinss
})

# Compute average silhouette for k = 2..8
sil_vals <- map_dbl(2:8, function(k) {
  km <- kmeans(feat_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(feat_scaled))
  mean(ss[, 3])
})

# Choose k by inspecting elbow + highest silhouette
k_elbow_plot <- data.frame(k = 1:8, wss = wss)
sil_plot <- data.frame(k = 2:8, silhouette = sil_vals)

# Print quick diagnostics
print(k_elbow_plot)
print(sil_plot)

# Default k selection: choose k with max silhouette (fallback to 4)
best_k <- ifelse(length(sil_vals) > 0, (2:8)[which.max(sil_vals)], 4)
if (is.na(best_k) || length(best_k) == 0) best_k <- 4
message("Selected k (by silhouette max or fallback): ", best_k)

# --- Run KMeans -----------------------------------------------------------
set.seed(1234)
km_res <- kmeans(feat_scaled, centers = best_k, nstart = 50, iter.max = 200)

cluster_input$cluster_km <- factor(km_res$cluster)

# Attach clusters back to original df_features
df_ml <- df_features %>%
  left_join(cluster_input %>% select(name, cluster_km), by = "name")

# --- PCA for visualization ------------------------------------------------
# Perform PCA on scaled features (if dimensions allow)
pca_res <- prcomp(feat_scaled, center = TRUE, scale. = FALSE)
pca_df <- data.frame(pca_res$x[, 1:2], cluster = cluster_input$cluster_km, name = cluster_input$name)
colnames(pca_df)[1:2] <- c("PC1", "PC2")

# Plot PCA scatter (will open plot in RStudio)
if (nrow(pca_df) > 0) {
  library(ggplot2)
  p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = paste0("PCA of features (k=", best_k, ")"),
         subtitle = "PC1 vs PC2 colored by KMeans cluster",
         x = "PC1", y = "PC2") +
    theme_minimal()
  print(p)
} else {
  message("Not enough rows for PCA.")
}

# --- Cluster summaries ----------------------------------------------------
# Produce cluster summary table
cluster_summary <- df_ml %>%
  group_by(cluster_km) %>%
  summarise(
    n = n(),
    median_price = median(price, na.rm = TRUE),
    median_sold = median(sold, na.rm = TRUE),
    mean_log_price = mean(log_price, na.rm = TRUE),
    mean_log_sold = mean(log_sold, na.rm = TRUE)
  ) %>%
  arrange(cluster_km)

print(cluster_summary)

# Optionally show top items per cluster
top_per_cluster <- df_ml %>%
  filter(!is.na(cluster_km)) %>%
  group_by(cluster_km) %>%
  slice_max(order_by = sold, n = 5) %>%
  select(cluster_km, name, price, sold) %>%
  ungroup()

print(top_per_cluster)

# --- Save segmented table -------------------------------------------------
save_df <- df_ml %>%
  select(name, price, sold_est = sold, cluster_km) %>%
  distinct()

write_csv(save_df, out_csv)
message("Saved segments CSV -> ", out_csv)

# --- End ------------------------------------------------------------------

