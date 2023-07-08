library(tidyverse)
companies <- readxl::read_xlsx("data.xlsx", n_max = 129)

# H0: Relatively homogeneous board members have no effect on agency costs
# H1: The highest proportion of board members affiliated to one tribe will be positively associated with Type 1 agency costs.

companies_df <- companies %>%
  select(1:9, 29:37) %>%
  mutate(prop_HF_board = HausaFulani/TotalBoardMembers,
         prop_I_board = Igbo/TotalBoardMembers,
         prop_Y_board = Yoruba/TotalBoardMembers,
         prop_M_board = Minority/TotalBoardMembers,
         prop_NN_board = NonNative/TotalBoardMembers,
         homogeneity = pmax(prop_HF_board, prop_I_board, prop_Y_board, prop_M_board, prop_NN_board))

# Create a histogram
histogram <- ggplot(companies_df, aes(x = homogeneity)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(x = "Homogeneity", y = "Frequency", title = "Histogram of Homogeneity")

# Create a density plot
density_plot <- ggplot(companies_df, aes(x = homogeneity)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = "Homogeneity", y = "Density", title = "Density Plot of Homogeneity")

# Display the plots
print(histogram)
print(density_plot)

model1 <- lm(homogeneity ~ AssetTurnover, data = companies_df)
