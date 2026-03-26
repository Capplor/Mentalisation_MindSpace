total_wide_mn$participant<-as.numeric(total_wide_mn$participant)

total<- left_join(total_wide_0_rep, total_wide_mn, by=c("participant", "N"))


total <- total %>%
  rowwise() %>%
  mutate(
    alpha_val = {
      # Find the first column whose name contains fc
      col_name <- names(total)[sapply(names(total), function(n) grepl(fc, n))]
      if(length(col_name) > 0) cur_data()[[col_name[1]]] else NA_real_
    }
  ) %>%
  ungroup()

ggplot(total, aes(x = x, y = y, colour = fc, alpha = alpha_val)) +
  geom_point()


total$alpha_true <- sapply(1:nrow(total), function(i) {
  col_name <- names(total)[5:8][grepl(total$fc[i], names(total)[5:8])]
  if (length(col_name) > 0) {
    total[i, col_name[1]][[1]]  # [[1]] extracts the value
  } else {
    NA
  }
})

total$alpha_fake <- sapply(1:nrow(total), function(i) {
  col_name <- names(total)[9:12][grepl(total$highval_pred[i], names(total)[5:8])]
  if (length(col_name) > 0) {
    total[i, col_name[1]][[1]]  # [[1]] extracts the value
  } else {
    NA
  }
})

true_tot<- total[,c(1:2,16:19, 21, 22, 23,25)]
pred_tot<- total[,c(1:2,9:15, 26)]
true_tot$dat<- "Real"
pred_tot$dat<- "Synthetic"
names<- c("participants","N", "builder", "explorer", "competitor", "pacifist", "fc",
         "x", "y", "alpha","source")

names(true_tot)<-names
names(pred_tot)<-names
tot_long<-rbind(pred_tot, true_tot)

ggplot(tot_long, aes(x, y, colour = fc, group = fc)) +
  geom_point(aes(alpha = alpha), size=1) +  
  labs(
    title = "Real Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT",
  )+
  facet_grid(fc~source) +
  theme_classic()+
  scale_color_futurama()


part<-sample(total$participant, 10)

means_rmse<- rmse0%>%
  group_by(participant)%>%
  summarise(mean=mean(rmse))
means_cos<- cos0%>%
  group_by(participant)%>%
  summarise(mean=mean(cosine_sim))
means_cor<- c0%>%
  group_by(participant)%>%
  summarise(mean=mean(correlation))
means_js<- js0%>%
  group_by(participant)%>%
  summarise(mean=mean(js))
means_kl<- kl0%>%
  group_by(participant)%>%
  summarise(mean=mean(kl))
means_bs<- bs0%>%
  group_by(participant)%>%
  summarise(mean=mean(bs))
means_ll<- log_loss0%>%
  group_by(participant)%>%
  summarise(mean=mean(log_loss))

top_cos<-means_cos$participant[order(means_cos$mean, decreasing = TRUE)[1:30]]
top_rmse<-means_rmse$participant[order(means_rmse$mean)[1:30]]
top_cor<-means_cor$participant[order(means_cor$mean, decreasing = TRUE)[1:30]]
top_js<-means_js$participant[order(means_js$mean)[1:30]]
top_kl<-means_kl$participant[order(means_kl$mean)[1:30]]
top_bs<-means_cor$participant[order(means_bs$mean)[1:30]]
top_ll<-means_ll$participant[order(means_ll$mean)[1:30]]
top<- list(top_bs, top_cos,top_cor,top_js,top_kl,top_rmse,top_ll)

reduce(top, intersect)[order(reduce(top, intersect))]

tot_top<- subset(tot_long, participants%in%c(2,25,28,29,30,41))

first_41<- subset(tot_long, participants<=41)
second_41<- subset(tot_long, participants>41)
ggplot(first_41, aes(x,y, colour = fc, alpha = alpha))+
  geom_point()+  
  labs(
    title = "Real Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT",
  )+
  facet_grid(participants~source) +
  scale_color_futurama()

ggplot(second_41, aes(x,y, colour = fc, alpha = alpha))+
  geom_point()+  
  labs(
    title = "Real Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT",
  )+
  facet_grid(participants~source) +
  scale_color_futurama()

for(i in unique(tot_long$participants)){
  p <- ggplot(tot_long[tot_long$participants == i, ], aes(x, y, colour = fc, alpha = alpha)) +
    geom_point() +  
    labs(
      title = paste("Participant:", i),
      x = "Aesthetic Appreciation",
      y = "Organisation",
      colour = "Player Type (PT)",
      alpha = "Percentage Assigned to PT"
    ) +
    facet_grid(~source) +
    ggsci::scale_color_futurama()
  
  # Save each plot
  ggsave(filename = paste0("participant_", i, ".png"), 
         plot = p, 
         width = 8, 
         height = 6)
}

ggplot(tot_top, aes(x,y, colour = fc, alpha = alpha))+
  geom_point()+  
  labs(
    title = "Real Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT",
  )+
  facet_grid(source~participants) +
  scale_color_futurama()


ggplot(tot_top, aes(x,y, colour = fc, alpha = alpha))+
  geom_point()+
  geom_density2d()+
  scale_color_futurama()



types<-c("Builder", "Explorer", "Competitor", "Pacifist")


all_dims <- list()
set.seed(123)
# Loop over participants
for (i in 1:nrow(data_k_0$alphax)) {
  for (k in 1:4) {
    aes <- rbeta(100, shape1 = data_k_0$alphax[i,k], shape2 = data_k_0$betax[i,k])
    org <- rbeta(100, shape1 = data_k_0$alphay[i,k], shape2 = data_k_0$betay[i,k])
    temp <- data.frame(aes = aes, org = org,
                       Type = types[k],
                       part = i)
    all_dims[[length(all_dims) + 1]] <- temp
  }
}
dim_df <- do.call(rbind, all_dims)
dim_long <- dim_df %>%
  pivot_longer(cols = c(aes, org),
               names_to = "measure",
               values_to = "value")%>%
  mutate(measure = recode(measure,
                          aes = "Aesthetic Appreciation",
                          org = "Organisation"))



ggplot(dim_long, aes(x = value))  +  # overall density per type
  geom_density(aes(group = part), alpha = 0.1, colour = "grey") +
  geom_density(aes(fill = Type),alpha = 0.7)+  # participant densities
  facet_grid(Type ~ measure) +
  scale_fill_futurama()+
  theme_classic() +
  labs(x = "Simulated Dimension Value", y = "Density", 
       fill ="Target Categorical Player Type (PT)")





tot_long_long <- tot_long %>%
  pivot_longer(cols = c(x, y),
               names_to = "measure",
               values_to = "value")%>%
  mutate(measure = recode(measure,
                          x = "Aesthetic Appreciation",
                          y = "Organisation"))

ggplot(tot_long, aes(x, builder))+
  geom_smooth(aes(group = participants),se=F,alpha=0.2, color="grey70")+
  geom_smooth(aes(colour = y))+
  facet_wrap(~source)

library(ggside)
tot_long%>%
  subset(source=="Real") %>% 
ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Real Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    fill="Player Type (PT)",
    alpha = "Percentage Assigned to PT"
  ) +
  facet_grid(fc~N) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

tot_long%>%
  subset(source=="Synthetic") %>% 
  ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Synthetic Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    fill="Player Type (PT)",
    alpha = "Percentage Assigned to PT"
  ) +
  facet_grid(fc~N) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

tot_long$fc<- sapply(as.character(tot_long$fc), capitalize)
tot_long$source<-factor(tot_long$source, levels = c("Real", "Synthetic"),
                           label = c("Participants", "No ρ"))
tot_long%>%
  ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Aggregate Level",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Target Categorical Player Type (PT)",
    fill="Target Categorical Player Type (PT)",
    alpha = "Target Player Type Percentage"
  ) +
  facet_grid(fc~source) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "bottom") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)


tot_top$participants<-as.factor(tot_top$participants)
tot_top$fc<- sapply(as.character(tot_top$fc), capitalize)

tot_top%>%
  ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Participant Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Target Categorical Player Type (PT)",
    fill="Target Categorical Player Type (PT)",
    alpha = "Target Player Type"
  ) +
  facet_grid(source~participants) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "bottom") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

tot_top%>%
  subset(source=="Synthetic") %>% 
  ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "No ρ Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Target Categorical Player Type (PT)",
    fill="Target Categorical Player Type (PT)",
    alpha = "Target Player Type"
  ) +
  facet_wrap(~participants, nrow = 3, ncol = 3) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

tot_top%>%
  subset(source=="Real") %>% 
  ggplot(aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Participants Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Target Categorical Player Type (PT)",
    fill="Target Categorical Player Type (PT)",
    alpha = "Target Player Type"
  ) +
  facet_wrap(~participants, nrow = 3, ncol = 3) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

tot_long$participantxfc<- interaction(tot_long$participants,tot_long$fc)


type_long <- tot_long %>%
  pivot_longer(cols = names(tot_long)[3:6],
               names_to = "cluster",
               values_to = "probability")

ggplot(tot_long_long, aes(x = measure, y = value, colour = fc, fill = fc)) +
  # Half-violin
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.3,
    point_colour = NA,
    alpha = 0.4
  ) +
  # Boxplot summary
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.3,
    position = position_nudge(x = -0.15)
  ) +
  # Jittered points (rain)
  geom_point(
    position = position_jitter(width = 0.1, seed = 123),
    alpha = 0.3,
    size = 1
  ) +
  scale_fill_futurama()+
  scale_color_futurama()+
  facet_grid(fc~source) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Value") +
  theme(legend.position = "none")




ggplot(type_long, aes(x = cluster, y = probability, fill=cluster)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.6, width = 0.4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_fill_futurama()+
  facet_grid(~source)+
  theme_classic() +
  labs(x = "Cluster", y = "Mean Probability")

library(Hmisc)

type_long$cluster<- sapply(as.character(type_long$cluster), capitalize)
type_long$N<- sapply(as.character(type_long$N), capitalize)

ggplot(type_long, aes(x = cluster, y = probability, fill=cluster)) +
  geom_col(aes(group=participants),position = "dodge", fill="grey60", alpha=0.6)+
  stat_summary(fun = "mean", geom = "bar", alpha = 0.6, width = 0.4) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  scale_fill_futurama()+
  facet_grid(N ~ source)+
  theme_classic() +
  theme(legend.position = "bottom")+
  labs(x = "Player Types", y = "Mean Player Type Probability per Trial", fill="Player Types")

ggplot(type_long, aes(x = cluster, y = probability, fill = cluster)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_futurama() +
  facet_grid(source ~ N) +
  theme_classic() +
  labs(x = "Cluster", y = "Probability")

realtypelong<-subset(type_long, source == "Real")
reallong <-realtypelong%>%
  pivot_longer(cols = names(realtypelong)[4:5],
               names_to = "Dimensions",
               values_to = "Values")
realdimlong<-subset(tot_long_long, source == "Real")
realdimlong$fc<-as.factor(capitalize(as.character(realdimlong$fc)))

ggplot(true_tot, aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Synthetic Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT"
  ) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  facet_grid(~N)+
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

ggplot(tot_long, aes(x, y, colour = fc)) +
  geom_point(aes(alpha = alpha)) +  
  # For the x-side density, we use the same colour (fc) and we can set the fill by fc as well.
  geom_xsidedensity(aes(y = after_stat(density), fill = fc), alpha = 0.5) +
  geom_ysidedensity(aes(x = after_stat(density), fill = fc), alpha = 0.5) +
  labs(
    title = "Synthetic Data",
    x = "Aesthetic Appretiation",
    y = "Organisation",
    colour = "Player Type (PT)",
    alpha = "Percentage Assigned to PT"
  ) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() + # We need to set the fill scale as well if we use fill in the marginal plots.
  theme(legend.position = "none") +
  facet_grid(source~N)+
  theme_classic()+
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(breaks = NULL)

theta<- as.data.frame(theta)
colnames(theta)<-types
theta$participant<- as.factor(1:nrow(theta))
theta_long<- theta%>%
  pivot_longer(cols = 1:4,
               names_to = "Cluster",
               values_to = "Belief Percentage")


ggplot(theta_long, aes(Cluster, `Belief Percentage`, fill = Cluster))+
  geom_col(aes(group=participant),position = "dodge", fill="grey60", alpha=0.6)+
  stat_summary(fun = "mean", geom = "bar", alpha = 0.6, width = 0.4) +
  scale_fill_futurama()+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "Target Categorical Player Type (PT)",
       y= "Player Type Prevalence",
       fill=" ")

ggplot(theta_long, aes(x = Cluster, y = `Belief Percentage`, colour = Cluster, fill = Cluster)) +
  # Half-violin
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.3,
    point_colour = NA,
    alpha = 0.4
  ) +
  # Boxplot summary
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.3,
    position = position_nudge(x = -0.15)
  ) +
  # Jittered points (rain)
  geom_point(
    position = position_jitter(width = 0.1, seed = 123),
    alpha = 0.3,
    size = 1
  ) +
  scale_fill_futurama()+
  scale_color_futurama() +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Value") +
  theme(legend.position = "none")


ggplot(realdimlong, aes(measure, value))+
  # Half-violin
  stat_halfeye(
    aes(fill=fc),
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.3,
    point_colour = NA,
    alpha = 0.4
  ) +
  # Boxplot summary
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.3,
    position = position_nudge(x = -0.15)
  ) +
  # Jittered points (rain)
  geom_point(
    aes(color=fc),
    position = position_jitter(width = 0.1, seed = 123),
    alpha = 0.3,
    size = 1
  ) +
  scale_fill_futurama()+
  scale_color_futurama() +
  coord_flip() +
  facet_grid(~N)+
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Value") +
  theme(legend.position = "none")

  
ggplot(realdimlong, aes(x = measure, y = value, fill=fc)) +
  geom_violin(alpha = 0.2) +
  geom_jitter(aes(color=fc, alpha = alpha))+
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  facet_grid(~ fc) +
  scale_fill_futurama() +
  scale_color_futurama()+
  theme_classic() +
  labs(x = "Hexaco Dimensions", 
       y = "Target Ratings for Dimensions", 
       fill="Categorical Player Type", 
       colour="Categorical Player Type",
       alpha="Player Typer Percentage") 

psych::describeBy(data=realdimlong, value~measure*N)

real_type_long<- type_long %>% 
  subset(source=="Real")
psych::describeBy(probability~cluster*N, data =real_type_long)

rho<-as.data.frame(data_k_0$rho)
rho1<-as.data.frame(data_1$rho)
rho$participant<- 1:nrow(rho)
rho1$participant<- 1:nrow(rho1)
names(rho1)[1]<- "ρ"
rho<-pivot_longer(rho, 1:4, names_to = "Player Type", values_to = "ρ")
rho$`Player Type`<- str_to_title(rho$`Player Type`)
rho_mean <- rho %>% 
  mutate(`Player Type` = str_to_title(`Player Type`)) %>%  # Capitalize names safely
  group_by(`Player Type`) %>% 
  summarise(mean = mean(ρ, na.rm = TRUE))

ggplot(rho, aes(`Player Type`,ρ,fill=`Player Type`))+
  geom_col(aes(group = participant),position = "dodge", fill="grey60", alpha=0.6)+
  stat_summary(fun = "mean", geom = "bar", alpha = 0.6, width = 0.4)+
  geom_hline(yintercept =0)
names(theta_long)<- c("participant", "K", "bel_prob")

ggplot(rho, aes(`Player Type`,ρ,colour =`Player Type`))+
  geom_violin()+
  geom_point(aes(group = participant),position = "jitter", col="grey60", alpha=0.6)+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2)+
  stat_summary(fun = "mean", geom = "point", alpha = 0.6, size=5)+
  geom_hline(yintercept =0)

ggplot(rho, aes(`Player Type`, ρ)) +
  geom_abline(data = rho1, aes(slope = ρ, intercept = 0.5), colour="grey30", alpha=0.7, show.legend = FALSE) +
  geom_abline(data = rho1, aes(slope = mean(ρ), intercept = 0.5), colour="black", show.legend = FALSE, linewidth=1) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(
    x = "Organisation",
    y = "Aesthetic Appreciation",
    title = expression("ρ General")
  )

ggplot(rho, aes(`Player Type`, ρ)) +
  geom_abline(data = rho, aes(slope = ρ, intercept = 0.5), colour="grey60", alpha=0.5, show.legend = FALSE) +
  geom_abline(data = rho_mean, aes(slope = mean, intercept = 0.5, colour = `Player Type`),  linewidth=1) +
  facet_wrap(~`Player Type`, nrow=2)+
  ggsci::scale_color_futurama()+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(
    x = "Organisation",
    y = "Aesthetic Appreciation",
    title = expression("ρ per Player Type")
  )

