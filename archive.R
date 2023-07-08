# Fit the linear regression model
model_int <- lm(AssetTurnover ~ dom_tribe_board * ceo_dom, data = companies_df)

# Generate values for the interaction plot
x1_vals <- levels(companies_df$dom_tribe_board)
x2_vals <- levels(companies_df$ceo_dom)
interaction_data <- expand.grid(x1 = x1_vals, x2 = x2_vals)
interaction_data$y_pred <- predict(model_int, newdata = interaction_data)

# Create the interaction plot
interaction.plot(x1_vals, x2_vals, interaction_data$y_pred, type = "b", legend = TRUE)


companies_df$ceo_tribe <- as.factor(companies_df$ceo_tribe)
companies_df$ceo_dom <- as.factor(companies_df$ceo_dom)



#For testing the first hypothesis, we have used two methods. The first is including all indicator 
#variables for the major tribes and the second is using the indicator variable for NonPredominant as a proxy for identifying companies where there is no major tribe having the highest proportion of board members.