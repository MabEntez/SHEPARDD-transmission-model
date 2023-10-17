rm(list = ls())
library(rstudioapi)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gridExtra)

#### Data Prep ####
setwd(dirname(getActiveDocumentContext()$path))

model_output <- read.csv("Cyst and Proto.csv")

model_output <- filter(model_output, Step == ((20 * 24)-10))
sheep_output <- filter(model_output, Species == "sheep")
sheep_output <- sheep_output[, c(3,6)]
dog_output <- filter(model_output, Species == "dog")
dog_output <- dog_output[, c(4,6)]
dog_output <- dog_output %>% 
  filter(N.parasite != 0)


# convert strings to list
sheep_output$Cyst.age.list <- strsplit(sheep_output$Cyst.age.list, " ")
sheep_output$Cyst.age.list <- lapply(sheep_output$Cyst.age.list, function(x) {
  x <- gsub("\\[", "", x) # Removes the opening bracket
  x <- gsub("\\]", "", x) # Removes the closing bracket
  as.numeric(unlist(x)) 
})

nCystPerYear <-  2.21
ProtPerCyst <-  10.68

sheep_output[,2] <- sheep_output[,2] + 1

for (i in 1:nrow(sheep_output)){
  sheep_output[i,3] <- nCystPerYear*sum(as.numeric(unlist(sheep_output[i,1]))*as.numeric(sheep_output[i,2])^3)
  sheep_output[i,4] <- sum(as.numeric(unlist(sheep_output[i,1])))
  print(i)
}
  
sheep_output <- sheep_output %>% rename(Protoscolex_count = V3,
                                        Total_Cysts = V4)
#model_output[,4] <- as.numeric(model_output[,4])
sheep_output <- subset(sheep_output, Total_Cysts != 0)
sheep_output <- subset(sheep_output, Protoscolex_count != 0)

summary_df <- sheep_output %>%
  group_by(Age) %>%
  summarise(Mean_Cysts = mean(Total_Cysts, na.rm = TRUE),
            uq_Cysts = quantile(Total_Cysts, 0.95, na.rm = TRUE),
            lq_Cysts = quantile(Total_Cysts, 0.05, na.rm = TRUE),
            Mean_Proto = mean(Protoscolex_count, na.rm = TRUE),
            uq_Proto = quantile(Protoscolex_count, 0.95, na.rm = TRUE),
            lq_Proto = quantile(Protoscolex_count, 0.05, na.rm = TRUE))

cyst <- ggplot(summary_df, aes(x = Age, y = Mean_Proto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = lq_Proto, ymax = uq_Proto), 
                width = 0.2, 
                colour = "black", 
                size = 0.5) +
  labs(x = "Age", 
       y = "Total Protoscolex", 
       title = "Total Protoscolex in Infected Sheep by Age") +
  theme_bw()

# Compute the mean values in a new data frame
mean_values <- sheep_output %>%
  group_by(Age) %>%
  summarise(mean_Total_Cysts = mean(Total_Cysts, na.rm = TRUE))

# Create the plot
proto <- ggplot(sheep_output, aes(x = factor(Age), y = Total_Cysts)) +
  geom_violin(fill = "darkblue", alpha = 0.5) +
  geom_point(data = mean_values, aes(y = mean_Total_Cysts, color = "black", shape = "Mean"), size = 3) +
  scale_color_manual(values = "black", name = "", labels = "Mean") +
  scale_shape_manual(values = 19, name = "", labels = "Mean") +
  labs(x = "Age", y = "Cyst Count", title = "Number of Mature Cysts Within\nInfected Sheep of Different Age Groups") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(1, 12)) +
  theme_bw()

combined_plot <- plot_grid(
  cyst, 
  proto, 
  ncol = 2, 
  align = "h"
)



ggplot(dog_output, aes(x=N.parasite)) + 
  geom_density(fill="blue", alpha=0.5) +
  labs(title="Adults parasite count in infected dogs", x="Number of parasites", y="Density")
