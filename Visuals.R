library(ggplot2)
library(reshape2)
library(igraph)
library(ggraph)

master_df <- read.csv("master_dataset_cleaned_with_regions.csv", stringsAsFactors = FALSE)

# Ensure Region is a factor and drop empty levels
master_df$Region <- as.factor(master_df$Region)


# Scatter + regression Daily.protein.supply -> Overweight_Pct

ggplot(master_df, aes(x = Daily.protein.supply, y = Overweight_Pct)) +
  geom_point(aes(color = Region), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  theme_minimal() +
  labs(title = "Daily Protein Supply vs. Overweight %",
       x = "Daily Protein Supply (g/capita/day)", 
       y = "Overweight Prevalence (%)")


# Scatter + regression Physical Activity -> Overweight_Pct

ggplot(master_df, aes(x = Physical_Activity_Insufficient_Pct, y = Overweight_Pct)) +
  geom_point(aes(color = Region), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  theme_minimal() +
  labs(title = "Insufficient Physical Activity vs. Overweight %",
       x = "Insufficient Physical Activity (%)", 
       y = "Overweight Prevalence (%)")


# Scatter Daily.protein.supply -> Physical Activity

ggplot(master_df, aes(x = Daily.protein.supply, y = Physical_Activity_Insufficient_Pct)) +
  geom_point(aes(color = Region), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkgray", linetype = "dashed", se = FALSE) +
  theme_minimal() +
  labs(title = "Protein Supply vs. Insufficient Physical Activity",
       x = "Daily Protein Supply (g/day)", 
       y = "Insufficient Physical Activity (%)")


# Boxplot of Daily.protein.supply grouped by Region

ggplot(master_df, aes(x = reorder(Region, Daily.protein.supply, FUN = median, na.rm=TRUE), 
                      y = Daily.protein.supply, fill = Region)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(title = "Distribution of Daily Protein Supply by Region",
       x = "Region", 
       y = "Daily Protein Supply (g/capita/day)")



# Stacked bar chart of average protein types by Region

protein_cols <- c("Fish.and.seafood", "Poultry", "Pork", "Beef.and.buffalo", "Eggs", "Dairy")
agg_protein <- aggregate(master_df[, protein_cols], 
                         by = list(Region = master_df$Region), 
                         FUN = mean, na.rm = TRUE)


# Melting 

melt_protein <- melt(agg_protein, id.vars = "Region", 
                     variable.name = "Protein_Type", value.name = "Supply")

ggplot(melt_protein, aes(x = Region, y = Supply, fill = Protein_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.2) +
  scale_fill_brewer(palette = "Set3") + # Uses a nice distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Source of Animal Protein by Region",
       x = "Region", 
       y = "Average Supply (kg/capita/year)",
       fill = "Protein Source")



# Correlation heatmap of all numeric variables
# Isolate numeric columns and drop the 'Year' columns

num_df <- master_df[, sapply(master_df, is.numeric)]
num_df <- num_df[, !grepl("Year", names(num_df))] 

cor_matrix <- cor(num_df, use = "pairwise.complete.obs")
melt_cor <- melt(cor_matrix)

ggplot(melt_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "royalblue", high = "firebrick", mid = "white", 
                       midpoint = 0, limit = c(-1,1), name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) +
  coord_fixed() + # Keeps the heatmap tiles perfectly square
  labs(title = "Correlation Heatmap")


# ==========================================
# 10. Process Google Trends Data (Protein Trend & Map)
# ==========================================

# 1. Read the data (CRITICAL: skip = 2 ignores the Google Trends metadata rows)
df_t <- read.csv("Protein_trend.csv", skip = 2, stringsAsFactors = FALSE)
df_tm <- read.csv("Protein_trend_map.csv", skip = 2, stringsAsFactors = FALSE)

# 2. Clean the Time Series Data (df_t)
names(df_t) <- c("Date", "Global_Trend_Score")
df_t$Date <- as.Date(df_t$Date, format = "%Y-%m-%d")

# 3. Clean the Map Data (df_tm)
names(df_tm) <- c("Country_EN", "Search_Interest")

# Google Trends sometimes uses "<1" for low volumes. This cleans that up and makes it numeric.
df_tm$Search_Interest <- suppressWarnings(as.numeric(gsub("<", "", df_tm$Search_Interest)))

# Remove countries with NA (blank) search interest
df_tm <- subset(df_tm, !is.na(Search_Interest))

# Sort the map data from highest interest to lowest
df_tm <- df_tm[order(-df_tm$Search_Interest), ]


# ==========================================
# 11. Google Trends Visualizations (ggplot2)
# ==========================================

# ---------------------------------------------------------
# Plot 8: Global Search Trend for "Protein" Over Time
# ---------------------------------------------------------
Sys.setlocale("LC_TIME", "C")
p8 <- ggplot(df_t, aes(x = Date, y = Global_Trend_Score)) +
  geom_line(color = "royalblue", size = 1) +
  geom_smooth(method = "loess", color = "darkorange", linetype = "dashed", se = FALSE) + 
  theme_minimal() +
  labs(title = "Global Google Search Interest for 'Protein' Over Time",
       subtitle = "Orange dashed line represents the smoothed overall trend",
       x = "Date", 
       y = "Relative Search Interest (0-100)") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p8)


#Test Statistic     

multiple_model <- lm(Overweight_Pct ~ Daily.protein.supply + Physical_Activity_Insufficient_Pct, 
                     data = master_df)

# Print the full summary to the console to get p-values and R-squared
summary(multiple_model)

# 1. First, assign your linear model to a variable so you can reference it
model <- lm(formula = Overweight_Pct ~ Daily.protein.supply + Physical_Activity_Insufficient_Pct, data = master_df)

# 2. Set up the plotting area to show 4 graphs in a 2x2 grid
par(mfrow = c(2, 2))

# 3. Generate the diagnostic plots
plot(model)

# 4. Reset the plotting area back to normal
par(mfrow = c(1, 1))

