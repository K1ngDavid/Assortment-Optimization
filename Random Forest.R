# === CHARGEMENT ===
library(readr)
library(tidyverse)
library(lubridate)
library(ranger)

df <- read_csv("Assortment_dataset.csv")

# === FEATURE ENGINEERING ===

# Temporel
df <- df %>%
  mutate(
    month       = month(date),
    week        = isoweek(date),
    day_of_week = wday(date, label = FALSE),
    is_weekend  = as.integer(day_of_week %in% c(1, 7))
  )

# Concurrence intra-catégorie
df <- df %>%
  group_by(store_id, date, product_category) %>%
  mutate(
    n_competitors      = sum(is_present) - is_present,
    avg_price_category = mean(actual_price_euros[is_present == 1]),
    price_vs_category  = actual_price_euros - avg_price_category,
    total_facing_cat   = sum(facing_cm * is_present),
    facing_share       = ifelse(total_facing_cat > 0, (facing_cm * is_present) / total_facing_cat, 0)
  ) %>%
  ungroup()

# Encodage des catégorielles
df <- df %>%
  mutate(
    store_format_enc     = as.integer(factor(store_format)),
    region_enc           = as.integer(factor(region)),
    product_category_enc = as.integer(factor(product_category)),
    nutriscore_enc       = as.integer(factor(nutriscore))
  )

# === FEATURES ===
features <- c(
  "shelf_capacity_cm", "purchasing_power_idx",
  "store_format_enc", "region_enc",
  "facing_cm", "retail_price_euros", "actual_price_euros",
  "promo_discount_pct", "unit_margin_euros", "is_present",
  "product_category_enc", "nutriscore_enc",
  "month", "week", "day_of_week", "is_weekend",
  "n_competitors", "price_vs_category", "facing_share"
)

# === SPLIT TEMPOREL ===
dates  <- sort(unique(df$date))
cutoff <- dates[round(length(dates) * 0.8)]
cat("Cutoff:", as.character(cutoff), "\n")

train <- df %>% filter(date <= cutoff)
test  <- df %>% filter(date > cutoff)

X_train <- train[, features]
y_train <- train$sales_volume
X_test  <- test[, features]
y_test  <- test$sales_volume

cat("Train:", nrow(train), "| Test:", nrow(test), "\n")

# === RANDOM FOREST ===
rf_model <- ranger(
  x = X_train, y = y_train,
  num.trees = 300,
  mtry = floor(sqrt(length(features))),
  importance = "impurity",
  num.threads = 4
)

pred_rf <- predict(rf_model, data = X_test)$predictions
mae_rf  <- mean(abs(y_test - pred_rf))
r2_rf   <- 1 - sum((y_test - pred_rf)^2) / sum((y_test - mean(y_test))^2)
cat("MAE:", round(mae_rf, 3), "| R²:", round(r2_rf, 3), "\n")

# === IMPORTANCE DES VARIABLES ===
importance_df <- data.frame(
  variable   = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
) %>% arrange(desc(importance))

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importance des variables (Random Forest)", x = "", y = "Importance")

# === PRÉDIT VS RÉEL ===
set.seed(42)
idx <- sample(length(pred_rf), 2000)

ggplot(data.frame(reel = y_test[idx], predit = pred_rf[idx]),
       aes(x = reel, y = predit)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = paste("Prédit vs Réel | MAE:", round(mae_rf, 2), "| R²:", round(r2_rf, 3)),
       x = "Ventes réelles", y = "Ventes prédites")
