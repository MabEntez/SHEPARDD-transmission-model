rm(list = ls())
library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(reshape2)


#### Data Prep ####
setwd(dirname(getActiveDocumentContext()$path))
inter1 <- read.csv("model_int.csv")
inter2 <- read.csv("model_int1.csv")
inter2$RunId <- inter2$RunId + max(inter1$RunId) + 1
inter3 <- read.csv("model_int2.csv")
inter3$RunId <- inter3$RunId + max(inter2$RunId) + 1
inter4 <- read.csv("model_int3.csv")
inter4$RunId <- inter4$RunId + max(inter3$RunId) + 1
inter5 <- read.csv("model_int4.csv")
inter5$RunId <- inter5$RunId + max(inter4$RunId) + 1
inter <- rbind(inter1, inter2, inter3, inter4, inter5)
inter <- inter[,-c(12,13)]
#keptid <- read.csv("KeptIDs.csv")
#keptid <- keptid[,2]
inter$Step <- as.integer(round((inter$Step + 10)/24))
spatial_dis <- read.csv("Spatial prev distribution.csv")
spatial_dis <- spatial_dis[,-1]
fitting <- inter[inter$Step == 40 & inter$sheep_vaccine_cov_alt == 0.8 & inter$dog_deworming_cov_alt == 0.65,]


# Assuming you want to find the RunIDs closest to a target value in sheep_prev
target_value <- 0.56  # Replace with your target value

# Calculate the absolute difference and sort
closest_runs <- fitting %>%
  mutate(diff = abs(sheep_prev - target_value)) %>%
  arrange(diff) %>%
  head(1000) %>%
  pull(RunId)

save(closest_runs, file = "Kept_runID_SD.RData")
load("Kept_runID_SD.Rdata")

kept_ID <- lapply(closest_runs, function(x) sample(x, 100))
kept_ID <- c(t(unlist(sapply(closest_runs, function(x) c(x, x + 1, x + 2, x + 3)))))

inter$groups <- with(inter, interaction(sheep_vaccine_cov_alt, dog_deworming_cov_alt))
inter_list <- split(inter, inter$groups)
new_names <- list("int1", "int2", "int3", "int4")
inter_list <- setNames(inter_list, new_names)
list2env(inter_list, envir = .GlobalEnv)

#### Intervention 1 ####

int1_dog <- int1[, c(1,2,12)]
int1_dog <- int1_dog[int1_dog$RunId %in% kept_ID, ]
int1_sheep <- int1[, c(1,2,13)]
int1_sheep <- int1_sheep[int1_sheep$RunId %in% kept_ID, ]

int1_sheep <- reshape(int1_sheep, idvar = "Step", timevar = "RunId", direction = "wide")
int1_dog <- reshape(int1_dog, idvar = "Step", timevar = "RunId", direction = "wide")

#### Intervention 2 ####

int2_dog <- int2[, c(1,2,12)]
int2_dog <- int2_dog[int2_dog$RunId %in% kept_ID, ]
int2_sheep <- int2[, c(1,2,13)]
int2_sheep <- int2_sheep[int2_sheep$RunId %in% kept_ID, ]

int2_sheep <- reshape(int2_sheep, idvar = "Step", timevar = "RunId", direction = "wide")
int2_dog <- reshape(int2_dog, idvar = "Step", timevar = "RunId", direction = "wide")

#### Intervention 3 ####

int3_dog <- int3[, c(1,2,12)]
int3_dog <- int3_dog[int3_dog$RunId %in% kept_ID, ]
int3_sheep <- int3[, c(1,2,13)]
int3_sheep <- int3_sheep[int3_sheep$RunId %in% kept_ID, ]

int3_sheep <- reshape(int3_sheep, idvar = "Step", timevar = "RunId", direction = "wide")
int3_dog <- reshape(int3_dog, idvar = "Step", timevar = "RunId", direction = "wide")

#### Intervention 4 ####

int4_dog <- int4[, c(1,2,12)]
int4_dog <- int4_dog[int4_dog$RunId %in% kept_ID, ]
int4_sheep <- int4[, c(1,2,13)]
int4_sheep <- int4_sheep[int4_sheep$RunId %in% kept_ID, ]

int4_sheep <- reshape(int4_sheep, idvar = "Step", timevar = "RunId", direction = "wide")
int4_dog <- reshape(int4_dog, idvar = "Step", timevar = "RunId", direction = "wide")


#### Basic Plot Sheep ####

library(patchwork)

# Prepare sheep data
int1_sheep_long <- gather(int1_sheep, "variable", "value", -Step)

# Plot for Sheep
s <- ggplot(int1_sheep_long, aes(x = Step, y = value, group = variable)) +
  geom_line() +
  labs(title = "Sheep Prevalence Burning Period", x = "Years", y = "Prevalence") +
  scale_x_continuous(limits = c(0, 40)) +
  theme_bw()

# Prepare dog data
int1_dog_long <- gather(int1_dog, "variable", "value", -Step)

# Plot for Dog
d <- ggplot(int1_dog_long, aes(x = Step, y = value, group = variable)) +
  geom_line() +
  labs(title = "Dog Prevalence Burning Period", x = "Years", y = "Prevalence") +
  scale_x_continuous(limits = c(0, 40)) +
  theme_bw()
# Combine plots horizontally
combined_plot <- s + d + plot_layout(ncol = 2)

# Display the combined plot
print(combined_plot)

#### ggplot Sheep ####
library(scales)
library(cowplot)
library(gridExtra)
## int1 vs int2 ##
int1_df = data.frame(matrix(nrow = (nrow(int1_sheep)), ncol = 4)) 
# Set the steps
int1_df[,1] <- int1_sheep[,1]
# Finding the median
int1_df[, 2] <- apply(subset(int1_sheep, select = 2:ncol(int1_sheep)), 1, median, na.rm = TRUE)
# Finding the 95% quantile
int1_df[, 3] <- apply(subset(int1_sheep, select = 2:ncol(int1_sheep)), 1, quantile, probs = 0.95, na.rm = TRUE)
int1_df[, 4] <- apply(subset(int1_sheep, select = 2:ncol(int1_sheep)), 1, quantile, probs = 0.05, na.rm = TRUE)

int2_df = data.frame(matrix(nrow = (nrow(int2_sheep)), ncol = 3)) 
# Finding the median
int2_df[, 1] <- apply(subset(int2_sheep, select = 2:ncol(int2_sheep)), 1, median, na.rm = TRUE)
# Finding the 95% quantile
int2_df[, 2] <- apply(subset(int2_sheep, select = 2:ncol(int2_sheep)), 1, quantile, probs = 0.95, na.rm = TRUE)
int2_df[, 3] <- apply(subset(int2_sheep, select = 2:ncol(int2_sheep)), 1, quantile, probs = 0.05, na.rm = TRUE)

int1_df <- int1_df %>% 
  rename(
    Steps = X1,
    Base_median = X2,
    Base_up_quantile = X3,
    Base_lo_quantile = X4
  )

int2_df <- int2_df %>% 
  rename(
    Median = X1,
    Up_quantile = X2,
    Lo_quantile = X3
  )

int2_df = cbind(int1_df, int2_df)


int3_df = data.frame(matrix(nrow = (nrow(int3_sheep)), ncol = 3)) 
# Finding the median
int3_df[, 1] <- apply(subset(int3_sheep, select = 2:ncol(int3_sheep)), 1, median, na.rm = TRUE)
# Finding the 95% quantile
int3_df[, 2] <- apply(subset(int3_sheep, select = 2:ncol(int3_sheep)), 1, quantile, probs = 0.95, na.rm = TRUE)
int3_df[, 3] <- apply(subset(int3_sheep, select = 2:ncol(int3_sheep)), 1, quantile, probs = 0.05, na.rm = TRUE)

int3_df <- int3_df %>% 
  rename(
    Median = X1,
    Up_quantile = X2,
    Lo_quantile = X3
  )

int3_df = cbind(int1_df, int3_df)


int4_df = data.frame(matrix(nrow = (nrow(int4_sheep)), ncol = 3)) 
# Finding the median
int4_df[, 1] <- apply(subset(int4_sheep, select = 2:ncol(int4_sheep)), 1, median, na.rm = TRUE)
# Finding the 95% quantile
int4_df[, 2] <- apply(subset(int4_sheep, select = 2:ncol(int4_sheep)), 1, quantile, probs = 0.95, na.rm = TRUE)
int4_df[, 3] <- apply(subset(int4_sheep, select = 2:ncol(int4_sheep)), 1, quantile, probs = 0.05, na.rm = TRUE)

int4_df <- int4_df %>% 
  rename(
    Median = X1,
    Up_quantile = X2,
    Lo_quantile = X3
  )

int4_df = cbind(int1_df, int4_df)


vac <- ggplot(data = int2_df, aes(x = Steps)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#e40404", alpha = 0.2) +
  coord_cartesian(xlim = c(30, NA)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Years") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Vaccine Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian" = "black", "Intensified intervention\nmedian" = "#e40404"), name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



dworm <- ggplot(data = int3_df, aes(x = Steps)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#ee7b00", alpha = 0.2) +
  coord_cartesian(xlim = c(30, NA)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Years") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Deworming Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian" = "black", "Intensified intervention\nmedian" = "#ee7b00"), name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



vacdworm <- ggplot(data = int4_df, aes(x = Steps)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#257600", alpha = 0.2) +
  coord_cartesian(xlim = c(30, NA)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Years") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Deworming & \nVaccination Coverage") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Current intervention\nmedian" = "black",
      "Intensified intervention\nmedian" = "#257600"
    ),name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))

# Generate a plot that includes all the legend elements
legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Current intervention"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Deworming"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination & Deworming"), data = data.frame(x = 1, y = 1)) +
  scale_color_manual(
    values = c(
      "Current intervention" = "black",
      "Intensified Vaccination" = "#e40404",
      "Intensified Deworming" = "#ee7b00",
      "Intensified Vaccination & Deworming" = "#257600"  
    ), 
    breaks = c("Current intervention",
               "Intensified Vaccination", 
               "Intensified Deworming", 
               "Intensified Vaccination & Deworming"),
    labels = c("Current intervention\n(65% deworming coverage, \n80% vaccination coverage)", 
               "Increased Vaccination Coverage\n(65% deworming coverage,\n90% vaccination coverage)", 
               "Increased Deworming Coverage\n(90% dewroming coverage, \n80% vaccination coverage)", 
               "Increased Deworming & Vaccination Coverage\n(90% dewroming coverage, \n90% vaccination coverage)"),  
    name = "Legend"
  ) +
  theme_void() +
  theme(legend.direction = "horizontal", legend.position = "bottom")


# Extract the legend
legend_grob <- cowplot::get_legend(legend_plot)

# Combine plots horizontally without their individual legends
combined_plot <- plot_grid(
  vac + theme(legend.position = "none"), 
  dworm + theme(legend.position = "none"), 
  vacdworm + theme(legend.position = "none"), 
  ncol = 3, 
  align = "h"
)

# Combine the plots with the legend
final_plot <- grid.arrange(combined_plot, legend_grob, nrow = 2, heights = c(10, 1))

# Display the final plot
print(final_plot)


#### Plotting Postintervention change ####
vac <- ggplot(data = int2_df, aes(x = Steps + 1939)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#e40404", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.05)) +  # Adjust y-axis here
  scale_y_continuous(labels = percent_format()) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Vaccine Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian" = "black",
                                "Intensified intervention\nmedian" = "#e40404"),
                     name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



dworm <- ggplot(data = int3_df, aes(x = Steps + 1939)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#ee7b00", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Deworming Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian" = "black",
                                "Intensified intervention\nmedian" = "#ee7b00"),
                     name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



vacdworm <- ggplot(data = int4_df, aes(x = Steps + 1939)) +
  geom_line(aes(y = Base_median, color = "Current intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Base_median, ymin = Base_lo_quantile, ymax = Base_up_quantile), fill = "black", alpha = 0.2) +
  geom_line(aes(y = Median, color = "Intensified intervention\nmedian"), size = 1) +
  geom_ribbon(aes(y = Median, ymin = Lo_quantile, ymax = Up_quantile), fill = "#257600", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Model years (after intervention)") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For\nIncreased Deworming & Vaccination Coverage") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Current intervention\nmedian" = "black",
      "Intensified intervention\nmedian" = "#257600"
    ),name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))

# Generate a plot that includes all the legend elements
legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Current intervention"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Deworming"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination & Deworming"), data = data.frame(x = 1, y = 1)) +
  scale_color_manual(
    values = c(
      "Current intervention" = "black",
      "Intensified Vaccination" = "#e40404",
      "Intensified Deworming" = "#ee7b00",
      "Intensified Vaccination & Deworming" = "#257600"  
    ), 
    breaks = c("Current intervention",
               "Intensified Vaccination", 
               "Intensified Deworming", 
               "Intensified Vaccination & Deworming"),
    labels = c("Current intervention", 
               "Increased Vaccination\nCoverage", 
               "Increased Deworming\nCoverage", 
               "Increased Deworming &\nVaccination Coverage"),  
    name = "Legend"
  ) +
  theme_void() +
  theme(legend.direction = "horizontal", legend.position = "bottom")


# Extract the legend
legend_grob <- cowplot::get_legend(legend_plot)

# Combine plots horizontally without their individual legends
combined_plot <- plot_grid(
  vac + theme(legend.position = "none"), 
  dworm + theme(legend.position = "none"), 
  vacdworm + theme(legend.position = "none"), 
  ncol = 3, 
  align = "h"
)

# Combine the plots with the legend
final_plot <- grid.arrange(combined_plot, legend_grob, nrow = 2, heights = c(10, 1))

# Display the final plot
print(final_plot)

#### Elimination percentage graph ####

proportion_zero1 <- apply(int1_sheep[2:ncol(int1_sheep)], 1, function(row) {
  sum(row == 0) / length(row)
})

proportion_zero2 <- apply(int2_sheep[2:ncol(int2_sheep)], 1, function(row) {
  sum(row == 0) / length(row)
})

proportion_zero3 <- apply(int3_sheep[2:ncol(int3_sheep)], 1, function(row) {
  sum(row == 0) / length(row)
})

proportion_zero4 <- apply(int4_sheep[2:ncol(int4_sheep)], 1, function(row) {
  sum(row == 0) / length(row)
})

proportion_zero <- data.frame(Steps = int1_sheep$Step, Proportion1 = proportion_zero1, Proportion2 = proportion_zero2, Proportion3 = proportion_zero3, Proportion4 = proportion_zero4)

p <- ggplot(data = proportion_zero, aes(x = Steps)) +
  geom_line(aes(y = Proportion1, color = "Current intervention"), size = 1.3)+
  geom_line(aes(y = Proportion2, color = "Intensified Vaccination"), size = 1.3)+
  geom_line(aes(y = Proportion3, color = "Intensified Deworming"), size = 1.3)+
  geom_line(aes(y = Proportion4, color = "Intensified Vaccination & Deworming"), size = 1.3)+
  coord_cartesian(xlim = c(60, NA))+
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    values = c(
      "Current intervention" = "#003f5c",
      "Intensified Vaccination" = "#7a5195",
      "Intensified Deworming" = "#ef5675",
      "Intensified Vaccination & Deworming" = "#ffa600"  
    ), 
    breaks = c("Current intervention",
               "Intensified Vaccination", 
               "Intensified Deworming", 
               "Intensified Vaccination & Deworming"),  # Add this line
    labels = c("Current intervention\n(65% deworming coverage,\n 80% vaccination coverage)", 
               "Increased Vaccination Coverage\n(65% deworming coverage,\n 90% vaccination coverage)", 
               "Increased Deworming Coverage\n(90% dewroming coverage,\n 80% vaccination coverage)", 
               "Increased Deworming &\nVaccination Coverage\n(90% dewroming coverage,\n 90% vaccination coverage)"),    
    name = "Legend"
  )+
  xlab("Years") +  # Label for the x-axis
  ylab("Percentage of simulations") +  # Label for the y-axis
  ggtitle("Percentage of simulations achieving 0% prevalence") +  # Plot title
  
  theme_bw()

p

#### Elim graph with quantiles ####

result_df <- data.frame(Step = unique(inter$Step))
result_df_1 <- data.frame(Step = unique(inter$Step))
result_df_2 <- data.frame(Step = unique(inter$Step))
result_df_3 <- data.frame(Step = unique(inter$Step))

# Function to compute percentage of zeros given a set of RunIds
compute_percentage_zeros <- function(run_ids) {
  inter %>%
    filter(RunId %in% run_ids) %>%
    group_by(Step) %>%
    summarise(percentage_zeros = mean(sheep_prev == 0, na.rm = TRUE) * 100)
}

# Loop over 100 sets
for (i in 1:100) {
  sampled_runs <- sample(closest_runs, 100)
  
  # Calculate percentages for RunId, RunId+1, RunId+2, and RunId+3
  temp_df <- compute_percentage_zeros(sampled_runs)
  temp_df_1 <- compute_percentage_zeros(sampled_runs + 1)
  temp_df_2 <- compute_percentage_zeros(sampled_runs + 2)
  temp_df_3 <- compute_percentage_zeros(sampled_runs + 3)
  
  # Rename the percentage columns
  colname <- paste0("Set_", i)
  colnames(temp_df)[2] <- colname
  colnames(temp_df_1)[2] <- colname
  colnames(temp_df_2)[2] <- colname
  colnames(temp_df_3)[2] <- colname
  
  # Merge with respective result_df
  result_df <- left_join(result_df, temp_df, by = "Step")
  result_df_1 <- left_join(result_df_1, temp_df_1, by = "Step")
  result_df_2 <- left_join(result_df_2, temp_df_2, by = "Step")
  result_df_3 <- left_join(result_df_3, temp_df_3, by = "Step")
}


set_cols <- colnames(result_df)[grepl("Set_", colnames(result_df))]
set_cols_1 <- colnames(result_df_1)[grepl("Set_", colnames(result_df_1))]
set_cols_2 <- colnames(result_df_2)[grepl("Set_", colnames(result_df_2))]
set_cols_3 <- colnames(result_df_3)[grepl("Set_", colnames(result_df_3))]

# Calculate mean, upper 95%, and lower 5% quantile for each Step
stats_df <- result_df %>%
  rowwise() %>%
  mutate(
    mean_val = mean(c_across(all_of(set_cols)), na.rm = TRUE),
    upper_95 = quantile(c_across(all_of(set_cols)), probs = 0.95, na.rm = TRUE),
    lower_5 = quantile(c_across(all_of(set_cols)), probs = 0.05, na.rm = TRUE)
  ) %>%
  select(Step, mean_val, lower_5, upper_95)  # Select only the required columns

stats_df_1 <- result_df_1 %>%
  rowwise() %>%
  mutate(
    mean_val = mean(c_across(all_of(set_cols)), na.rm = TRUE),
    upper_95 = quantile(c_across(all_of(set_cols)), probs = 0.95, na.rm = TRUE),
    lower_5 = quantile(c_across(all_of(set_cols)), probs = 0.05, na.rm = TRUE)
  ) %>%
  select(Step, mean_val, lower_5, upper_95)  # Select only the required columns

stats_df_2 <- result_df_2 %>%
  rowwise() %>%
  mutate(
    mean_val = mean(c_across(all_of(set_cols)), na.rm = TRUE),
    upper_95 = quantile(c_across(all_of(set_cols)), probs = 0.95, na.rm = TRUE),
    lower_5 = quantile(c_across(all_of(set_cols)), probs = 0.05, na.rm = TRUE)
  ) %>%
  select(Step, mean_val, lower_5, upper_95)  # Select only the required columns

stats_df_3 <- result_df_3 %>%
  rowwise() %>%
  mutate(
    mean_val = mean(c_across(all_of(set_cols)), na.rm = TRUE),
    upper_95 = quantile(c_across(all_of(set_cols)), probs = 0.95, na.rm = TRUE),
    lower_5 = quantile(c_across(all_of(set_cols)), probs = 0.05, na.rm = TRUE)
  ) %>%
  select(Step, mean_val, lower_5, upper_95)  # Select only the required columns


p1 <- ggplot() +
  geom_ribbon(data = stats_df, aes(x = Step, ymin = lower_5 / 100, ymax = upper_95 / 100), fill = "#003f5c", alpha = 0.3) +
  geom_line(data = stats_df, aes(x = Step, y = mean_val / 100, color = "Current intervention"), size = 1.3) +
  
  geom_ribbon(data = stats_df_1, aes(x = Step, ymin = lower_5 / 100, ymax = upper_95 / 100), fill = "#7a5195", alpha = 0.3) +
  geom_line(data = stats_df_1, aes(x = Step, y = mean_val / 100, color = "Intensified Vaccination"), size = 1.3) +
  
  geom_ribbon(data = stats_df_2, aes(x = Step, ymin = lower_5 / 100, ymax = upper_95 / 100), fill = "#ef5675", alpha = 0.3) +
  geom_line(data = stats_df_2, aes(x = Step, y = mean_val / 100, color = "Intensified Deworming"), size = 1.3) +
  
  geom_ribbon(data = stats_df_3, aes(x = Step , ymin = lower_5 / 100, ymax = upper_95 / 100), fill = "#ffa600", alpha = 0.3) +
  geom_line(data = stats_df_3, aes(x = Step, y = mean_val / 100, color = "Intensified Vaccination & Deworming"), size = 1.3) +
  
  coord_cartesian(xlim = c(55, NA)) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    values = c(
      "Current intervention" = "#003f5c",
      "Intensified Vaccination" = "#7a5195",
      "Intensified Deworming" = "#ef5675",
      "Intensified Vaccination & Deworming" = "#ffa600"  
    ), 
    breaks = c("Current intervention",
               "Intensified Vaccination", 
               "Intensified Deworming", 
               "Intensified Vaccination & Deworming"),  # Add this line
    labels = c("Current intervention\n(65% deworming coverage,\n 80% vaccination coverage)", 
               "Increased Vaccination Coverage\n(65% deworming coverage,\n 90% vaccination coverage)", 
               "Increased Deworming Coverage\n(90% dewroming coverage,\n 80% vaccination coverage)", 
               "Increased Deworming &\nVaccination Coverage\n(90% dewroming coverage,\n 90% vaccination coverage)"),    
    name = "Legend"
  )+
  xlab("Year") +
  ylab("Percentage of simulations") +
  ggtitle("Percentage of simulations achieving 0% prevalence") +
  theme_bw()

print(p1)
