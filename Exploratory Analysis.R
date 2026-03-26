#Multinomial Regression ####

merged_data0wide<- merged_data0 %>% pivot_wider(names_from = "K", 
                                                values_from = c("true_prob",
                                                                "pred_prob"))
merged_data0wide$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                apply(merged_data0wide[,c(9:12)], 1,
                                                      function(x) names(merged_data0wide[,c(9:12)])[which.max(x)])))
fc_pred<-read.csv("fc_pred.csv")
xy_predwide<-read.csv("xy_predwide.csv")
merged_data0wide$participant<- as.numeric(merged_data0wide$participant)

x_rep<-fit0corr_h$draws("x_rep", format = "matrix")
y_rep<-fit0corr_h$draws("y_rep", format = "matrix")



x_rep_mean <- as.data.frame(t(as.data.frame(t(colMeans(x_rep)))))
names(x_rep_mean)<- "x_rep"
y_rep_mean <- as.data.frame(t(as.data.frame(t(colMeans(y_rep)))))
names(y_rep_mean)<- "y_rep"

# Extract indices from row names
x_rep_mean$index <- rownames(x_rep_mean)

# Extract N and participant from the pattern "x_rep[participant, N]"
x_rep_mean$participant <- as.integer(gsub(".*\\[(\\d+),.*", "\\1", rownames(x_rep_mean)))
x_rep_mean$N <- as.integer(gsub(".*,(\\d+)\\]", "\\1", rownames(x_rep_mean)))
x_rep_mean$N <- paste0("t", x_rep_mean$N)
x_rep_mean$index <- NULL

# Reset row names to simple numeric sequence
rownames(x_rep_mean) <- NULL

# Final result
head(x_rep_mean)

# Extract indices from row names
y_rep_mean$index <- rownames(y_rep_mean)

# Extract N and participant from the pattern "x_rep[participant, N]"
y_rep_mean$participant <- as.integer(gsub(".*\\[(\\d+),.*", "\\1", rownames(y_rep_mean)))
y_rep_mean$N <- as.integer(gsub(".*,(\\d+)\\]", "\\1", rownames(y_rep_mean)))
y_rep_mean$N <- paste0("t", y_rep_mean$N)
y_rep_mean$index <- NULL

# Reset row names to simple numeric sequence
rownames(y_rep_mean) <- NULL

# Final result
head(y_rep_mean)

xy_rep<-left_join(x_rep_mean, y_rep_mean, 
          by = c("participant", "N"))
xy_rep$participant<- as.numeric(xy_rep$participant)

total_wide_0_rep<- left_join(merged_data0wide, xy_rep, 
                         by = c("participant", "N"))

total_wide_0_rep$x_rep <- scale(total_wide_0_rep$x_rep)
total_wide_0_rep$y_rep <- scale(total_wide_0_rep$y_rep)


log1_0_rep<- nnet::multinom(highval_pred~1, data= total_wide_0_rep)
log2_0_rep<- nnet::multinom(highval_pred~x_rep, data= total_wide_0_rep)
log3_0_rep<- nnet::multinom(highval_pred~x_rep+y_rep, data= total_wide_0_rep)
log4_0_rep<- nnet::multinom(highval_pred~x_rep*y_rep, data= total_wide_0_rep)

est_0_rep <- summary(log4_0_rep)$coefficients
se_0_rep <- summary(log4_0_rep)$standard.errors
z_0_rep <- est_0_rep/se_0_rep
z_0_rep

2*(1-pnorm(abs(z_0_rep)))

car::Anova(log3_0_rep, type = 3)
car::Anova(log4_0_rep, type = 3)

model_comparison_0_rep <- data.frame(
  Model = c("Additive (log3)", "Interaction (log4)"),
  AIC = c(AIC(log3_0_rep), AIC(log4_0_rep)),
  BIC = c(BIC(log3_0_rep), BIC(log4_0_rep))
)

print(model_comparison_0_rep)

x_vals_0_rep <- seq(min(total_wide_0_rep$x_rep), max(total_wide_0_rep$x_rep), length.out = 45)
y_vals_0_rep <- seq(min(total_wide_0_rep$y_rep), max(total_wide_0_rep$y_rep), length.out = 45)

# Name the columns to match the training data exactly
grid_0_rep <- expand.grid(x_rep = x_vals_0_rep, y_rep = y_vals_0_rep)

# Convert grid columns to matrices to match the training data structure
grid_0_rep$x_rep <- as.matrix(grid_0_rep$x_rep)
grid_0_rep$y_rep <- as.matrix(grid_0_rep$y_rep)


grid_predictions3_0_rep <- predict(log3_0_rep, newdata = grid_0_rep, type = "probs")
grid_predictions4_0_rep <- predict(log4_0_rep, newdata = grid_0_rep, type = "probs")

predictions_long3_0_rep <- cbind(grid_0_rep, grid_predictions3_0_rep)
predictions_long4_0_rep <- cbind(grid_0_rep, grid_predictions4_0_rep)
predictions_melted3_0_rep <- melt(predictions_long3_0_rep, id.vars = c("x_rep", "y_rep"),
                                  variable.name = "Category", 
                                  value.name = "Probability")

predictions_melted4_0_rep <- melt(predictions_long4_0_rep, id.vars = c("x_rep", "y_rep"),
                                  variable.name = "Category", 
                                  value.name = "Probability")

ggplot(predictions_melted3_0_rep, aes(x = x_rep, y = y_rep, fill = Probability)) +
  geom_tile() +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  scale_fill_viridis_c() +
  labs(title = "Additive Effects by Category",
       x = "Aesthetic Appreciation", y = "Organisation", fill = "Probability") +
  theme_minimal()

ggplot(predictions_melted4_0_rep, aes(x = x_rep, y = y_rep, fill = Probability)) +
  geom_tile() +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  scale_fill_viridis_c() +
  labs(title = "Interaction Effects by Category",
       subtitle = "Generated Dataset",
       x = "Aesthetic Appreciation", y = "Organisation", fill = "Probability") +
  theme_minimal()


