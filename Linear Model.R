library(tidyverse)
library(lubridate)
library(readr)

df <- read_csv("Assortment_dataset.csv")

# === EXPLORATION ===
str(df)
summary(df)
sapply(df, function(x) sum(is.na(x)))

# === FEATURE ENGINEERING ===

# Temporel
df <- df %>%
  mutate(
    month       = month(date),
    week        = isoweek(date),
    day_of_week = wday(date, label = FALSE),
    is_weekend  = as.integer(day_of_week %in% c(1, 7))
  )

boxplot(df$sales_volume ~ df$nutriscore)

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

boxplot(df$n_competitors)


vars <- c("sales_volume", "facing_cm", "facing_share", "actual_price_euros",
          "price_vs_category", "n_competitors", "promo_discount_pct",
          "is_present", "purchasing_power_idx", "shelf_capacity_cm",
          "unit_margin_euros", "retail_price_euros")

cor_matrix <- cor(df[, vars], use = "complete.obs")

# Affichage visuel
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.cex = 0.7, title = "Matrice de corrélation",
         mar = c(0, 0, 1, 0))



reg <- lm(sales_volume ~ facing_cm + actual_price_euros + promo_discount_pct +
            is_present + n_competitors + price_vs_category + facing_share +
            purchasing_power_idx + shelf_capacity_cm,
          data = df)

summary(reg)

