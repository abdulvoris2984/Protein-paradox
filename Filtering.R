df_pa <- read.csv("physical activity.csv", stringsAsFactors = FALSE)
df_ow <- read.csv("share-of-adults-who-are-overweight.csv", stringsAsFactors = FALSE)
df_ap <- read.csv("animal-protein-consumption.csv", stringsAsFactors = FALSE)
df_dp <- read.csv("daily-per-capita-protein-supply.csv", stringsAsFactors = FALSE)
df_tm <- read.csv("Protein_trend_map.csv", stringsAsFactors = FALSE)
df_t <- read.csv("Protein_trend.csv", stringsAsFactors = FALSE)


# ==========================================
# 2. Standardize column names and extract target data
# ==========================================
# Physical Activity (Also extracting ParentLocation to use as our Region column)

df_pa <- df_pa[, c("SpatialDimValueCode", "Location", "ParentLocation", "Period", "FactValueNumeric", "Dim1")]
names(df_pa) <- c("Code", "Country_PA", "Region", "Year_PA", "Physical_Activity_Insufficient_Pct", "Dim1")
df_pa <- subset(df_pa, Dim1 == "Both sexes")
df_pa$Dim1 <- NULL

# Overweight
ow_col_idx <- grep("overweight", names(df_ow), ignore.case = TRUE)[1]
names(df_ow)[ow_col_idx] <- "Overweight_Pct"
names(df_ow)[names(df_ow) == "Entity"] <- "Country_OW"
names(df_ow)[names(df_ow) == "Year"] <- "Year_OW"

# Animal & Daily Protein
names(df_ap)[names(df_ap) == "Entity"] <- "Country_AP"
names(df_ap)[names(df_ap) == "Year"] <- "Year_AP"

names(df_dp)[names(df_dp) == "Entity"] <- "Country_DP"
names(df_dp)[names(df_dp) == "Year"] <- "Year_DP"

# ==========================================
# 3. Handle Historical Data (Keep only the latest year)
# ==========================================
# COMMENT: In the AP dataset, ~188 countries report 2023 data, 11 report 2022, 
# and a few are older. We dynamically filter to keep only the max year per country.

df_ap <- do.call(rbind, lapply(split(df_ap, df_ap$Code), function(x) x[which.max(x$Year_AP), ]))
df_dp <- do.call(rbind, lapply(split(df_dp, df_dp$Code), function(x) x[which.max(x$Year_DP), ]))

# Reset row names after splitting and binding
rownames(df_ap) <- NULL
rownames(df_dp) <- NULL

# ==========================================
# 4. Find common countries and filter
# ==========================================
codes_pa <- na.omit(df_pa$Code)
codes_ow <- na.omit(df_ow$Code)
codes_ap <- na.omit(df_ap$Code)
codes_dp <- na.omit(df_dp$Code)

common_codes <- intersect(intersect(intersect(codes_pa, codes_ow), codes_ap), codes_dp)

df_pa <- subset(df_pa, Code %in% common_codes)
df_ow <- subset(df_ow, Code %in% common_codes)
df_ap <- subset(df_ap, Code %in% common_codes)
df_dp <- subset(df_dp, Code %in% common_codes)

# ==========================================
# 5. Handle N/A Values (Drop missing targets)
# ==========================================
df_pa <- subset(df_pa, !is.na(Physical_Activity_Insufficient_Pct))
df_ow <- subset(df_ow, !is.na(Overweight_Pct))
df_dp <- subset(df_dp, !is.na(Daily.protein.supply))

ap_targets <- c("Fish.and.seafood", "Poultry", "Pork", "Beef.and.buffalo")
df_ap <- df_ap[complete.cases(df_ap[, ap_targets]), ]

# Re-intersect common codes after N/A drops to ensure alignment
final_common <- intersect(intersect(intersect(df_pa$Code, df_ow$Code), df_ap$Code), df_dp$Code)

df_pa <- subset(df_pa, Code %in% final_common)
df_ow <- subset(df_ow, Code %in% final_common)
df_ap <- subset(df_ap, Code %in% final_common)
df_dp <- subset(df_dp, Code %in% final_common)

# ==========================================
# 6. Outlier Detection (Strategy: Flag, don't drop)
# ==========================================
# We use IQR to flag extreme values as TRUE. This retains valuable true anomalies 
# (e.g. extremely high beef consumption in specific nations) without skewing later blind averages.
flag_outliers <- function(df, cols) {
  for (col in cols) {
    if (is.numeric(df[[col]])) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR_val
      upper_bound <- Q3 + 1.5 * IQR_val
      
      flag_col_name <- paste0(col, "_outlier_flag")
      df[[flag_col_name]] <- ifelse(df[[col]] < lower_bound | df[[col]] > upper_bound, TRUE, FALSE)
      df[[flag_col_name]][is.na(df[[flag_col_name]])] <- FALSE
    }
  }
  return(df)
}

df_pa <- flag_outliers(df_pa, c("Physical_Activity_Insufficient_Pct"))
df_ow <- flag_outliers(df_ow, c("Overweight_Pct"))
df_ap <- flag_outliers(df_ap, c("Fish.and.seafood", "Poultry", "Pork", "Beef.and.buffalo"))
df_dp <- flag_outliers(df_dp, c("Daily.protein.supply"))

# ==========================================
# 7. Merge datasets into one Master Dataframe
# ==========================================
# Get standard country names from overweight data
country_map <- unique(df_ow[, c("Code", "Country_OW")])
names(country_map)[2] <- "Country_Name"

# Extract Region mapped to country code
region_map <- unique(df_pa[, c("Code", "Region")])

# Remove redundant columns before merging
df_pa$Country_PA <- NULL
df_pa$Region <- NULL
df_ow$Country_OW <- NULL
df_ap$Country_AP <- NULL
df_dp$Country_DP <- NULL

# Build the master dataframe
master_df <- data.frame(Code = final_common, stringsAsFactors = FALSE)
master_df <- merge(master_df, country_map, by = "Code", all.x = TRUE)
master_df <- merge(master_df, region_map, by = "Code", all.x = TRUE) # Adds the Region column
master_df <- merge(master_df, df_pa, by = "Code", all.x = TRUE)
master_df <- merge(master_df, df_ow, by = "Code", all.x = TRUE)
master_df <- merge(master_df, df_ap, by = "Code", all.x = TRUE)
master_df <- merge(master_df, df_dp, by = "Code", all.x = TRUE)

master_df$Daily.protein.supply..Original.Year. <- NULL


write.csv(master_df, "master_dataset_cleaned_with_regions.csv", row.names = FALSE)
print(paste("Success! Merged", nrow(master_df), "countries with Regions and Outlier Flags."))
