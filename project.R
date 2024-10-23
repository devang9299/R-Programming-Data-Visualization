shell('clr')

# Load necessary libraries
library(tidyverse)
library(gglot2)


# Import CSV files as tibble objects
men <- read_csv("men.csv")
women <- read_csv("Women.csv")
men
women

# Combine the two tibbles
combined <- bind_rows(men, women)

# Group by State and summarize the counts of Strong, Weak, and No-opinion
summary_data <- combined %>%
  group_by(State) %>%
  summarise(
    Strong = sum(Strong, na.rm = TRUE),
    Weak = sum(Weak, na.rm = TRUE),
    No_opinion = sum(`No-opinion`, na.rm = TRUE)
  )

# Find the state with maximum and minimum counts for each category
max_strong_state <- summary_data %>% filter(Strong == max(Strong)) %>% select(State, Strong)
max_weak_state <- summary_data %>% filter(Weak == max(Weak)) %>% select(State, Weak)
max_no_opinion_state <- summary_data %>% filter(No_opinion == max(No_opinion)) %>% select(State, No_opinion)

min_strong_state <- summary_data %>% filter(Strong == min(Strong)) %>% select(State, Strong)
min_weak_state <- summary_data %>% filter(Weak == min(Weak)) %>% select(State, Weak)
min_no_opinion_state <- summary_data %>% filter(No_opinion == min(No_opinion)) %>% select(State, No_opinion)

max_strong_state
max_weak_state
max_no_opinion_state
min_strong_state
min_weak_state
min_no_opinion_state



# Create pie chart for men with different colors
men_plot <- ggplot(men, aes(x = "", y = Weak, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Distribution of 'Weak' Responses by State - Men") +
  geom_text(aes(label = Strong), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = rainbow(n = nrow(men)))
men_plot


# Create pie chart for women with different colors
women_plot <- ggplot(women, aes(x = "", y = Strong, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Distribution of 'Strong' Responses by State - Women") +
  geom_text(aes(label = Strong), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = rainbow(n = nrow(women)))
women_plot


# Function to create the bar plot
create_bar_plot <- function(data, title) {
  # Create the bar plot
  barplot_obj <- barplot(data$`No-opinion`, 
                         names.arg = data$State, 
                         las = 2,  # Rotate x-axis labels
                         cex.names = 0.7,  # Reduce size of state names
                         main = title,
                         ylab = "No-opinion Count",
                         col = "skyblue",
                         ylim = c(0, max(data$`No-opinion`) * 1.1))  # Add space for labels
  
  # Add text labels on top of each bar
  text(x = barplot_obj, 
       y = data$`No-opinion`, 
       labels = data$`No-opinion`, 
       pos = 3,  # Position above the bar
       cex = 0.8)  # Adjust text size
}

# Create bar plot for men
png("men_no_opinion_barplot.png", width = 800, height = 600)
create_bar_plot(men, "Men's No-opinion Count by State")
dev.off()

# Create bar plot for women
png("women_no_opinion_barplot.png", width = 800, height = 600)
create_bar_plot(women, "Women's No-opinion Count by State")
dev.off()

# Display the plots in the R environment
create_bar_plot(men, "Men's No-opinion Count by State")
create_bar_plot(women, "Women's No-opinion Count by State")