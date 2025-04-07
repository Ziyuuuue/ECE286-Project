# Install required packages if not already installed
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("dplyr")
library(ggplot2)
library(patchwork)
library(dplyr)

## Plot 1: Interpretation Scores

# Define parameters for interpretation scores based on the provided test results
mean_sit_interp <- 4.545455  # Mean for sit_interp
mean_walk_interp <- 4.250000 # Mean for walk_interp
sd_interp <- 0.5             # Assumed standard deviation for demonstration
ci_lower_interp <- -0.33     # 95% CI for difference in means (annotation only)
ci_upper_interp <- 0.92

# Create a sequence of x-values covering both distributions
x_values_interp <- seq(3, 6, length.out = 1000)
df_interp <- data.frame(x = x_values_interp) %>%
  mutate(density_sit = dnorm(x, mean = mean_sit_interp, sd = sd_interp),
         density_walk = dnorm(x, mean = mean_walk_interp, sd = sd_interp),
         overlap = pmin(density_sit, density_walk))
max_density_interp <- max(c(df_interp$density_sit, df_interp$density_walk))

p1 <- ggplot(df_interp, aes(x = x)) +
  # Plot the distributions with specified colors
  geom_line(aes(y = density_sit, color = "Sit Distribution"), size = 1.2) +
  geom_line(aes(y = density_walk, color = "Walk Distribution"), size = 1.2) +
  # Shade the overlapping region in light green and outline it in purple
  geom_area(aes(y = overlap, fill = "Overlap Region"), alpha = 0.5) +
  geom_line(aes(y = overlap), color = "purple", size = 1) +
  # Add vertical dashed lines for the means
  geom_vline(xintercept = mean_sit_interp, linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = mean_walk_interp, linetype = "dashed", color = "red", size = 1) +
  # Add dashed lines for the 95% CI for each distribution (using qnorm)
  geom_vline(xintercept = qnorm(0.025, mean = mean_sit_interp, sd = sd_interp), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(0.975, mean = mean_sit_interp, sd = sd_interp), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(0.025, mean = mean_walk_interp, sd = sd_interp), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = qnorm(0.975, mean = mean_walk_interp, sd = sd_interp), linetype = "dashed", color = "red", size = 1) +
  # Annotate the means
  annotate("text", x = mean_sit_interp, y = max_density_interp * 0.9, 
           label = paste("Mean:", round(mean_sit_interp, 2)), 
           color = "blue", angle = 90, vjust = -0.5, size = 4) +
  annotate("text", x = mean_walk_interp, y = max_density_interp * 0.9, 
           label = paste("Mean:", round(mean_walk_interp, 2)), 
           color = "red", angle = 90, vjust = -0.5, size = 4) +
  # Annotate the confidence interval for the difference in means
  annotate("text", x = 4.3, y = max_density_interp * 0.7, 
           label = paste("95% CI (diff): [", ci_lower_interp, ", ", ci_upper_interp, "]", sep=""), 
           size = 4, color = "black") +
  labs(x = "Interpretation Score", y = "Density",
       color = "Legend", fill = "Legend") +
  scale_color_manual(values = c("Sit Distribution" = "blue",
                                "Walk Distribution" = "red",
                                "Overlap Region" = "purple")) +
  scale_fill_manual(values = c("Overlap Region" = "plum")) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_blank())

## Plot 2: Memorization Scores

# Define parameters for memorization scores based on the provided test results
mean_sit_memor <- 29.09091  # Mean for sit_memor
mean_walk_memor <- 15.75    # Mean for walk_memor
sd_memor <- 2.5             # Assumed standard deviation for demonstration
ci_lower_memor <- 5.434802
ci_upper_memor <- 21.247016

# Create a sequence of x-values covering both distributions
x_values_memor <- seq(10, 35, length.out = 1000)
df_memor <- data.frame(x = x_values_memor) %>%
  mutate(density_sit = dnorm(x, mean = mean_sit_memor, sd = sd_memor),
         density_walk = dnorm(x, mean = mean_walk_memor, sd = sd_memor),
         overlap = pmin(density_sit, density_walk))
max_density_memor <- max(c(df_memor$density_sit, df_memor$density_walk))

p2 <- ggplot(df_memor, aes(x = x)) +
  geom_line(aes(y = density_sit, color = "Sit Distribution"), size = 1.2) +
  geom_line(aes(y = density_walk, color = "Walk Distribution"), size = 1.2) +
  geom_area(aes(y = overlap, fill = "Overlap Region"), alpha = 0.5) +
  geom_line(aes(y = overlap), color = "purple", size = 1) +
  geom_vline(xintercept = mean_sit_memor, linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = mean_walk_memor, linetype = "dashed", color = "red", size = 1) +
  # Dashed lines for the 95% CI for each distribution
  geom_vline(xintercept = qnorm(0.025, mean = mean_sit_memor, sd = sd_memor), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(0.975, mean = mean_sit_memor, sd = sd_memor), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(0.025, mean = mean_walk_memor, sd = sd_memor), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = qnorm(0.975, mean = mean_walk_memor, sd = sd_memor), linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = mean_sit_memor, y = max_density_memor * 0.9, 
           label = paste("Mean:", round(mean_sit_memor, 2)), 
           color = "blue", angle = 90, vjust = -0.5, size = 4) +
  annotate("text", x = mean_walk_memor, y = max_density_memor * 0.9, 
           label = paste("Mean:", round(mean_walk_memor, 2)), 
           color = "red", angle = 90, vjust = -0.5, size = 4) +
  annotate("text", x = 22, y = max_density_memor * 0.7, 
           label = paste("95% CI (diff): [", ci_lower_memor, ", ", ci_upper_memor, "]", sep=""), 
           size = 4, color = "black") +
  labs(x = "Memorization Score", y = "Density",
       color = "Legend", fill = "Legend") +
  scale_color_manual(values = c("Sit Distribution" = "blue",
                                "Walk Distribution" = "red",
                                "Overlap Region" = "purple")) +
  scale_fill_manual(values = c("Overlap Region" = "plum")) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_blank())

## Combine the two plots horizontally with a shared legend
combined_plot <- p1 + p2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display the combined plot
print(combined_plot)

