#----------------------------------------
# Calling data if not run Main Data ####

all_data <- readRDS("all_data_lists.rds")
data_k_0 <- all_data$data_k_0
data_1 <-all_data$data_1
data_gen <-all_data$data_gen
data_1_gen <- all_data$data_1_gen
rm(all_data)
w_pred<- read_xlsx("w_pred.xlsx")

# Models####

kcorr_model<-"k_constrained.stan" #K Correlation
kcorr_mod <- cmdstan_model( kcorr_model)
onecorr_model<-"one_constrained.stan"
onecorr_mod <- cmdstan_model(onecorr_model)
ZCcorr_model<-"zero constrained.stan"
ZCcorr_mod <- cmdstan_model( ZCcorr_model)
kgencorr_model<-"k general.stan"
kgencorr_mod <- cmdstan_model( kgencorr_model)
onegencorr_mod<-"one general.stan"
onegencorr_mod <- cmdstan_model(onegencorr_mod)
Zgen_model<-"zero general.stan"
Zgen_mod <- cmdstan_model(Zgen_model)

# Model Key ####
#N = Trial Number
#I = Participant
#K = Cluster
#x = Aesthetic Appreciation Trial Estimate
#y = Organisation Trial Estimate
#alphax = Shape 1 x
#betax = Shape 2 x
#alphay = Shape 1 y
#betay = Shape 2 y
#theta = Dirichlet parameters (concentration)
#rho = Correlation between Distribution of X and Y
#w = Estimate of likelihood to belong to each cluster


# Custom Functions####

#Correlation perfect


#Log Loss Formula
log_loss <- function(p_true, p_pred) {
  -sum(p_true * log(p_pred))
}

#RMSE function
rmse <- function(p_true, p_pred) {
  sqrt(mean((p_true - p_pred)^2))
}

#Cosine Similarity 

cosine_similarity <- function(p_true, p_pred) {
  sum(p_true * p_pred) / (sqrt(sum(p_true^2)) * sqrt(sum(p_pred^2)))
}

#KL Divergence

kl_divergence <- function(p, q, epsilon = 1e-12) {
  sum(p * log(p / q))
}

#Jensen-Shannon Divergence (JSD)

js_divergence <- function(p, q) {
  m <- 0.5*(p+q)
  (0.5*sum(p * log(p / m)))+(0.5*sum(q * log(q / m)))
}
#Brier-Score Divergence (JSD)

brier_score <- function(p, q) {
  mean((p-q)^2)
}


#Summarise

library(rlang)
#Simpler summetrics
#summetrics <- function(data, metric, ...) {
#  data %>%
#    group_by(...) %>% 
#    summarise(
#      mean   = mean({{ metric }}, na.rm = TRUE),
#      median = median({{ metric }}, na.rm = TRUE),
#      MAD    = mad({{ metric }}, na.rm = TRUE),
#      iqr    = IQR({{ metric }}, na.rm = TRUE),
#      sd     = sd({{ metric }}, na.rm = TRUE),
#      n      = sum(!is.na({{ metric }})),
#      se     = sd / sqrt(n),
#      ci_lower = mean - qt(0.975, df = n - 1) * se,
#      ci_upper = mean + qt(0.975, df = n - 1) * se,
#      max    = max({{ metric }}, na.rm = TRUE),
#      min    = min({{ metric }}, na.rm = TRUE),
#      .groups = "drop"
#    )
#}

summetrics <- function(data, metric, group_by = NULL, conf_level = 0.95) {
  # Convert metric to symbol
  metric_sym <- if (is.character(metric)) rlang::sym(metric) else rlang::ensym(metric)
  
  # Handle grouping
  if (!is.null(group_by)) {
    group_syms <- rlang::syms(group_by)
    data <- data %>% dplyr::group_by(!!!group_syms)
  }
  
  alpha <- 1 - conf_level
  
  # First calculate basic stats
  summary_df <- data %>%
    dplyr::summarise(
      across(
        .cols = !!metric_sym,
        .fns = list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          MAD = ~mad(.x, na.rm = TRUE),
          IQR = ~IQR(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          n = ~sum(!is.na(.x)),
          max = ~max(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          coeff_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),
          skewness = ~moments::skewness(.x, na.rm = TRUE),
          kurtosis = ~moments::kurtosis(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # Then calculate SE and CI using the already computed stats
  metric_name <- as.character(metric_sym)
  
  summary_df <- summary_df %>%
    dplyr::mutate(
      "{metric_name}_se" := .data[[paste0(metric_name, "_sd")]] / sqrt(.data[[paste0(metric_name, "_n")]]),
      "{metric_name}_ci_lower" := .data[[paste0(metric_name, "_mean")]] - 
        qt(1 - alpha/2, df = .data[[paste0(metric_name, "_n")]]-1) * .data[[paste0(metric_name, "_se")]],
      "{metric_name}_ci_upper" := .data[[paste0(metric_name, "_mean")]] + 
        qt(1 - alpha/2, df = .data[[paste0(metric_name, "_n")]]-1) * .data[[paste0(metric_name, "_se")]]
    )
  
  return(summary_df)
}


#----------------------------------------
# FITTING MODELS #####
#----------------------------------------
# K model ####

# Fit the model using the prepared data
fitkcorr_h <-  kcorr_mod$sample(
  data = data_k_0,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)



# 1 model ####


# Fit the model using the prepared data
fit1corr_h <-  onecorr_mod$sample(
  data = data_1,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)


# Zero#######


zerocorrmod <- ZCcorr_mod$sample(
  data = data_k_0,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)



#----------------------------------------
# General K ####


# Fit the model using the prepared data
fitkcorrgen <-  kgencorr_mod$sample(
  data = data_gen,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)




# General 1 ####

# Fit the model using the prepared data
fit1corrgen <-  onegencorr_mod$sample(
  data = data_1_gen,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)

# General Zero #######


zerocorrgen <- Zgen_mod$sample(
  data = data_gen,
  seed = 123,           # Set a seed for reproducibility
  chains = 4,           # Number of MCMC chains
  parallel_chains = 4,  # Number of chains to run in parallel
  iter_warmup = 5000,   # Number of warmup iterations
  iter_sampling = 5000,# Number of sampling iterations
  max_treedepth = 15,
  adapt_delta = 0.98,
  step_size = 0.1
)

#----------------------------------------
# DATA SET CREATION AND METRICS#####
#----------------------------------------
# K ----
w_probK  <-  fitkcorr_h$draws(variables = "w_prob", format="matrix")

summary_statsK  <- summarize_draws(w_probK , mean, median, sd)
summarydfK  <- summary_statsK  %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
           extra = "drop",convert = TRUE) %>%
  dplyr:: select(-prefix)
summarydfK <-summarydfK[,c(1:4)]
colnames(summarydfK ) <- c("participant", "N", "K","w_prob")
summarydfK$K <- dplyr:: recode(summarydfK$K, 
                               `1` = "builder", 
                               `2` = "explorer", 
                               `3` = "competitor", 
                               `4` = "pacifist")
summarydfK$model <- "K Correlations"

for(i in 1:nrow(summarydfK)){
  summarydfK$N[i]<- paste0("t",summarydfK$N[i])
}

summarydfK$participant<- as.factor(summarydfK$participant)

w_pred$model<- "real probability"

merged_dataK <- summarydfK %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))

merged_datakwide<- merged_dataK %>% pivot_wider(names_from = "K", 
                                                values_from = c("true_prob",
                                                                "pred_prob"))
merged_datakwide$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                apply(merged_datakwide[,c(5:8)], 1,
                                                      function(x) names(merged_datakwide[,c(5:8)])[which.max(x)])))
merged_datakwide$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                apply(merged_datakwide[,c(9:12)], 1,
                                                      function(x) names(merged_datakwide[,c(9:12)])[which.max(x)])))
merged_datakwide<- merge(merged_datakwide, fc_pred, 
                         by = intersect(names(merged_datakwide), names(fc_pred)), 
                         all = TRUE)

# Cross-Entropy calculation
log_lossK <- merged_dataK %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_lossK = sum(log_prob, na.rm = TRUE))

log_losspartK <- merged_dataK %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )


cK<-merged_dataK%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')

cpartk<-merged_dataK %>%
  group_by(participant, N) %>%
  summarise(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = "drop"
  )

errorkpart<- merged_dataK %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )
errork<-pracma::rmserr(merged_dataK$true_prob,merged_dataK$pred_prob,summary = TRUE)


cosK<- merged_dataK %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  )
klK<- merged_dataK %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

jsK<- merged_dataK %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bsK<- merged_dataK %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_resultK <- adjustedRandIndex(merged_datakwide$highval_true, 
                                 merged_datakwide$highval_pred)
ari_resultkfcK <- adjustedRandIndex(merged_datakwide$highval_pred, 
                                    merged_datakwide$fc)

ari_resultkpart <- merged_datakwide %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )

# 1----
w_prob1  <-  fit1corr_h$draws(variables = "w_prob", format="matrix")


summary_stats1  <- summarize_draws(w_prob1 , mean, median, sd)
summarydf1  <- summary_stats1  %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
           extra = "drop",convert = TRUE) %>%
  dplyr:: select(-prefix)
summarydf1 <-summarydf1[,c(1:4)]
colnames(summarydf1) <- c("participant", "N", "K","w_prob")
summarydf1$K <- dplyr:: recode(summarydf1$K, 
                               `1` = "builder", 
                               `2` = "explorer", 
                               `3` = "competitor", 
                               `4` = "pacifist")
summarydf1$model <- "One Correlation"

for(i in 1:nrow(summarydf1)){
  summarydf1$N[i]<- paste0("t",summarydf1$N[i])
}

summarydf1$participant<- as.factor(summarydf1$participant)

w_pred$model<- "real probability"

merged_data1 <- summarydf1 %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))

merged_data1wide<- merged_data1 %>% pivot_wider(names_from = "K", 
                                                values_from = c("true_prob",
                                                                "pred_prob"))
merged_data1wide$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                apply(merged_data1wide[,c(5:8)], 1,
                                                      function(x) names(merged_data1wide[,c(5:8)])[which.max(x)])))
merged_data1wide$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                apply(merged_data1wide[,c(9:12)], 1,
                                                      function(x) names(merged_data1wide[,c(9:12)])[which.max(x)])))
merged_data1wide<- merge(merged_data1wide, fc_pred, 
                         by = intersect(names(merged_data1wide), names(fc_pred)), 
                         all = TRUE)

# Cross-Entropy calculation
log_loss1 <- merged_data1 %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_loss1 = sum(log_prob, na.rm = TRUE))

log_losspart1 <- merged_data1 %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )

c1<-merged_data1%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')

cpart1<-merged_data1 %>%
  group_by(participant, N) %>%
  summarize(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = 'drop'
  )

error1part<- merged_data1 %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )

error1<-pracma::rmserr(merged_data1$true_prob,merged_data1$pred_prob,
                       summary = TRUE)

cos1<- merged_data1 %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  )

kl1<-merged_data1 %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

js1<- merged_data1 %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bs1<- merged_data1 %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_result1 <- adjustedRandIndex(merged_data1wide$highval_true, 
                                 merged_data1wide$highval_pred)
ari_result1fc <- adjustedRandIndex(merged_data1wide$highval_pred, 
                                   merged_data1wide$fc)

ari_result1part<- merged_data1wide %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )


# 0----
w_prob0 <- zerocorrmod$draws(variables = "w_prob", format="matrix")

summary_stats0 <- summarize_draws(w_prob0, mean, median, sd)
summarydf0 <- summary_stats0 %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
           extra = "drop",convert = TRUE) %>%
  dplyr::select(-prefix)
summarydf0<-summarydf0[,c(1:4)]
colnames(summarydf0) <- c("participant", "N", "K","w_prob")
summarydf0$K <- dplyr::recode(summarydf0$K, 
                              `1` = "builder", 
                              `2` = "explorer", 
                              `3` = "competitor", 
                              `4` = "pacifist")
summarydf0$model <- "No Correlations"
for(i in 1:nrow(summarydf0)){
  summarydf0$N[i]<- paste0("t",summarydf0$N[i])
}

summarydf0$participant<- as.factor(summarydf0$participant)


merged_data0 <- summarydf0 %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))
merged_data0wide<- merged_data0 %>% pivot_wider(names_from = "K", 
                                                values_from = c("true_prob",
                                                                "pred_prob"))
merged_data0wide$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                apply(merged_data0wide[,c(5:8)], 1,
                                                      function(x) names(merged_data0wide[,c(5:8)])[which.max(x)])))
merged_data0wide$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                apply(merged_data0wide[,c(9:12)], 1,
                                                      function(x) names(merged_data0wide[,c(9:12)])[which.max(x)])))
merged_data0wide<- merge(merged_data0wide, fc_pred, 
                         by = intersect(names(merged_data0wide), names(fc_pred)), 
                         all = TRUE)

# Cross-Entropy calculation
log_loss0 <- merged_data0 %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_loss0 = sum(log_prob, na.rm = TRUE))


log_losspart0 <-merged_data0 %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )

c0<-merged_data0%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')

cpart0<-merged_dataK %>%
  group_by(participant, N) %>%
  summarize(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = 'drop'
  )

error0<-pracma::rmserr(merged_data0$true_prob,merged_data0$pred_prob,summary = TRUE)

error0part<- merged_data0 %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )

cos0<-merged_data0 %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  ) 
kl0<-merged_data0 %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

js0<- merged_data0 %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bs0<- merged_data0 %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_result0 <- adjustedRandIndex(merged_data0wide$highval_true, 
                                 merged_data0wide$highval_pred)
ari_result0fc <- adjustedRandIndex(merged_data0wide$highval_pred, 
                                   merged_data0wide$fc)

ari_result0part <- merged_data0wide %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )

# Gen K ----

w_probgen  <-  fitkcorrgen$draws(variables = "w_prob", format="matrix")

summary_statsgen  <- summarize_draws(w_probgen , mean, median, sd)
summarydfgen  <- summary_statsgen  %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
           extra = "drop",convert = TRUE) %>%
  dplyr:: select(-prefix)
summarydfgen <-summarydfgen[,c(1:4)]
colnames(summarydfgen ) <- c("participant", "N", "K","w_prob")
summarydfgen$K <- dplyr:: recode(summarydfgen$K, 
                                 `1` = "builder", 
                                 `2` = "explorer", 
                                 `3` = "competitor", 
                                 `4` = "pacifist")
summarydfgen$model <- "K Correlations"

for(i in 1:nrow(summarydfgen)){
  summarydfgen$N[i]<- paste0("t",summarydfgen$N[i])
}

summarydfgen$participant<- as.factor(summarydfgen$participant)

meltDataframegen<- rbind( as.data.frame(w_pred), 
                          summarydfgen)

merged_datakgen <- summarydfgen %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))

merged_datakwidegen<- merged_datakgen %>% pivot_wider(names_from = "K", 
                                                      values_from = c("true_prob",
                                                                      "pred_prob"))
merged_datakwidegen$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                   apply(merged_datakwidegen[,c(5:8)], 1,
                                                         function(x) names(merged_datakwidegen[,c(5:8)])[which.max(x)])))

merged_datakwidegen$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                   apply(merged_datakwidegen[,c(9:12)], 1,
                                                         function(x) names(merged_datakwidegen[,c(9:12)])[which.max(x)])))
merged_datakwidegen<- merge(merged_datakwidegen, fc_pred, 
                            by = intersect(names(merged_datakwidegen), names(fc_pred)), 
                            all = TRUE)
merged_datakwidegen$fc<-as.factor(merged_datakwidegen$fc)

# Cross-Entropy calculation
log_losskgen <- merged_datakgen %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_losskgen = sum(log_prob, na.rm = TRUE))

log_losspartgen <- merged_datakgen %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )


# Cross-Entropy calculation
cgen<-merged_datakgen%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')
cgenpart<-merged_datakgen %>%
  group_by(participant, N) %>%
  summarize(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = 'drop'
  )

errorkgen<-pracma::rmserr(merged_datakgen$true_prob,merged_datakgen$pred_prob,summary = TRUE)
errorkgenpart<- merged_datakgen %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )

coskgen<-merged_datakgen %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  )
klkgen<-merged_datakgen %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

jskgen<- merged_datakgen %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bsKgen<- merged_datakgen %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_resultkgen <- adjustedRandIndex(merged_datakwidegen$highval_true, 
                                    merged_datakwidegen$highval_pred)

ari_resultkpartgen <- merged_datakwidegen %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )

# Gen 1 -----

w_probgen1  <-  fit1corrgen$draws(variables = "w_prob", format="matrix")

summary_statsgen1  <- summarize_draws(w_probgen1 , mean, median, sd)
summarydfgen1  <- summary_statsgen1  %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", extra = "drop",convert = TRUE) %>%
  dplyr:: select(-prefix)
summarydfgen1 <-summarydfgen1[,c(1:4)]
colnames(summarydfgen1) <- c("participant", "N", "K","w_prob")
summarydfgen1$K <- dplyr:: recode(summarydfgen1$K, 
                                  `1` = "builder", 
                                  `2` = "explorer", 
                                  `3` = "competitor", 
                                  `4` = "pacifist")
summarydfgen1$model <- "One Correlation"

for(i in 1:nrow(summarydfgen1)){
  summarydfgen1$N[i]<- paste0("t",summarydfgen1$N[i])
}

summarydfgen1$participant<- as.factor(summarydfgen1$participant)

merged_data1gen <- summarydfgen1 %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))

merged_data1widegen<- merged_data1gen %>% pivot_wider(names_from = "K", 
                                                      values_from = c("true_prob",
                                                                      "pred_prob"))
merged_data1widegen$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                   apply(merged_data1widegen[,c(5:8)], 1,
                                                         function(x) names(merged_data1widegen[,c(5:8)])[which.max(x)])))

merged_data1widegen$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                   apply(merged_data1widegen[,c(9:12)], 1,
                                                         function(x) names(merged_data1widegen[,c(9:12)])[which.max(x)])))
merged_data1widegen<- merge(merged_data1widegen, fc_pred, 
                            by = intersect(names(merged_data1widegen), names(fc_pred)), 
                            all = TRUE)
merged_data1widegen$fc<-as.factor(merged_data1wide$fc)

# Cross-Entropy calculation
log_loss1gen <- merged_data1gen %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_loss1gen = sum(log_prob, na.rm = TRUE))

log_losspartgen1 <- merged_data1gen %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )


# Cross-Entropy calculatio
cgen1<-merged_data1gen%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')

cgen1part<-merged_data1gen %>%
  group_by(participant, N) %>%
  summarize(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = 'drop'
  )

error1gen<-pracma::rmserr(merged_data1gen$true_prob,merged_data1gen$pred_prob,summary = TRUE)
error1genpart<- merged_data1gen %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )

#Cosine Similarity

cos1gen<- merged_data1gen %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  )

#KL divergence

kl1gen<- merged_data1gen %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

js1gen<- merged_data1gen %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bs1gen<- merged_data1gen %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_result1gen <- adjustedRandIndex(merged_data1widegen$highval_true, 
                                    merged_data1widegen$highval_pred)

ari_result1partgen <- merged_data1widegen %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )

# Gen 0 -----

w_probgen0 <- zerocorrgen$draws(variables = "w_prob", format="matrix")

summary_statsgen0 <- summarize_draws(w_probgen0, mean, median, sd)
summarydfgen0 <- summary_statsgen0 %>%
  separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
           extra = "drop",convert = TRUE) %>%
  dplyr::select(-prefix)
summarydfgen0<-summarydfgen0[,c(1:4)]
colnames(summarydfgen0) <- c("participant", "N", "K","w_prob")
summarydfgen0$K <- dplyr::recode(summarydfgen0$K, 
                                 `1` = "builder", 
                                 `2` = "explorer", 
                                 `3` = "competitor", 
                                 `4` = "pacifist")
summarydfgen0$model <- "No Correlations"
for(i in 1:nrow(summarydfgen0)){
  summarydfgen0$N[i]<- paste0("t",summarydfgen0$N[i])
}


summarydfgen0$participant<- as.factor(summarydfgen0$participant)

merged_datagen0 <- summarydfgen0 %>%
  rename(pred_prob = w_prob) %>%
  left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))



merged_data0widegen<- merged_datagen0 %>% pivot_wider(names_from = "K", 
                                                      values_from = c("true_prob",
                                                                      "pred_prob"))
merged_data0widegen$highval_true <- as.factor(gsub("true_prob_", "",  # Remove prefix
                                                   apply(merged_data0widegen[,c(5:8)], 1,
                                                         function(x) names(merged_data0widegen[,c(5:8)])[which.max(x)])))
merged_data0widegen$highval_pred <- as.factor(gsub("pred_prob_", "",  # Remove prefix
                                                   apply(merged_data0widegen[,c(9:12)], 1,
                                                         function(x) names(merged_data0widegen[,c(9:12)])[which.max(x)])))
merged_data0widegen<- merge(merged_data0widegen, fc_pred, 
                            by = intersect(names(merged_data0widegen), names(fc_pred)), 
                            all = TRUE)

# Cross-Entropy calculation
log_lossgen0 <- merged_datagen0 %>%
  mutate(log_prob = -true_prob * log(pred_prob)) %>%
  summarise(log_loss0 = sum(log_prob, na.rm = TRUE))


log_losspartgen0 <- merged_datagen0 %>%
  group_by(participant, N) %>%
  summarise(
    log_loss = log_loss(true_prob, pred_prob),
    .groups = "drop"
  )


c0gen<-merged_datagen0%>%
  group_by(K)%>%
  summarize(correlation = cor(true_prob, pred_prob), .groups = 'drop')

cpart0gen<-merged_datagen0 %>%
  group_by(participant, N) %>%
  summarize(
    correlation = ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
                         cor(true_prob, pred_prob),
                         NA_real_),
    .groups = 'drop'
  )



error0genpart<-merged_datagen0 %>%
  group_by(participant, N) %>%
  summarise(
    rmse = rmse(true_prob, pred_prob),
    .groups = "drop"
  )

error0gen<-pracma::rmserr(merged_datagen0$true_prob,merged_datagen0$pred_prob,summary = TRUE)

#Cosine Similarity

cos0gen<- merged_datagen0 %>%
  group_by(participant, N) %>%
  summarise(
    cosine_sim = cosine_similarity(true_prob, pred_prob),
    .groups = "drop"
  )

#KL divergence

kl0gen<- merged_datagen0 %>%
  group_by(participant, N) %>%
  summarize(
    kl = kl_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

js0gen<- merged_datagen0 %>%
  group_by(participant, N) %>%
  summarize(
    js = js_divergence(true_prob, pred_prob),
    .groups = 'drop'
  )

bs0gen<- merged_datagen0 %>%
  group_by(participant, N) %>%
  summarize(
    bs = brier_score(true_prob, pred_prob),
    .groups = 'drop'
  )


ari_result0gen <- adjustedRandIndex(merged_data0widegen$highval_true, 
                                    merged_data0widegen$highval_pred)
ari_result0fc0gen <- adjustedRandIndex(merged_data0widegen$highval_pred, 
                                       merged_data0widegen$fc)

ari_result0partgen <- merged_data0widegen %>%
  group_by(participant) %>%
  summarise(
    ari = adjustedRandIndex(highval_true, highval_pred),
    .groups = "drop"
  )



#-------------------------------------------
# PLOTS & REPORTS####
#-------------------------------------------
# Corr Plot ####
mergedmerged<- rbind(merged_data0, merged_data1, merged_dataK)%>%group_by(N, K, model.x)%>%
  summarise(
    mean_wpred = mean(pred_prob),
    mean_wtrue = mean(true_prob),
    .groups="drop"
  )


ggplot(mergedmerged, aes(mean_wpred, mean_wtrue))+
  geom_jitter()+
  geom_smooth(method="lm")+
  facet_grid(model.x~.)+
  labs(
    x = "P'",
    y = "P",
    colour = "Cluster",
    fill = "Cluster",
  )+
  theme_minimal()

combined_all <- bind_rows(
  cpart0  %>% mutate(model = "No ρ"),
  cpart1  %>% mutate(model = "General ρ"),
  cpartk  %>% mutate(model = "Cluster ρ"),
  cpart0gen %>% mutate(model = "No ρ General Prior"),
  cgen1part %>% mutate(model = "General ρ General Prior"),
  cgenpart  %>% mutate(model = "Cluster ρ General Prior")
)


# Add hierarchy info
combined_all <- combined_all %>%
  mutate(
    hier = ifelse(grepl("General Prior", model), "General", "Hierarchical"),
    model = str_replace(model, " General Prior", "")  # Clean model names
  )
cor_summary <- combined_all %>%
  group_by(model, hier) %>%
  summetrics("correlation")

# Plot
ggplot(cor_summary, aes(x = hier, y = correlation_mean, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggsci::scale_fill_futurama() +
  geom_errorbar(aes(ymin = correlation_mean - correlation_se, 
                    ymax = correlation_mean + correlation_se), 
                width = 0.2) +
  facet_grid(model~., scales = "free_y", space = "free") +
  labs(
    y = "Mean Correlation",
    x = NULL,
    title = "Comparison of\nMean Correlation Across Models"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # centers the title
  )

# LOO-CV ####

loo_kcorr_h <- fitkcorr_h$loo("log_lik")
loo_1corr_h <- fit1corr_h$loo("log_lik")
loo_zero <- zerocorrmod$loo("log_lik")
loo_gen_kcorr <- fitkcorrgen$loo("log_lik")
loo_gen_1corr <- fit1corrgen$loo("log_lik")
loo_gen_zero <- zerocorrgen$loo("log_lik")

# LOO Raw Values ####

# 1. Hierarchical models (k-correction)
loo_kcorr_h$estimates
# 2. Hierarchical models (1-correlation)
loo_1corr_h$estimates
# 3. Zero-correlation models
loo_zero$estimates
# 4. General models (k-correction)
loo_gen_kcorr$estimates
# 5. General models (1-correlation)
loo_gen_1corr$estimates
# 6. General zero-correlation models
loo_gen_zero$estimates


# LOO & Plots ####
# Create lists of models for comparison
# Create lists of models for comparison
model_groups <- list(
  hierarchical = list(loo_kcorr_h, loo_1corr_h, loo_zero),
  general = list(loo_gen_kcorr, loo_gen_1corr, loo_gen_zero)
)

# Names for models within each group
model_names <- list(
  hierarchical = c("Cluster ρ", "General ρ", "No ρ"),
  general = c("Cluster ρ", "General ρ", "No ρ")
)

# 1. Combine all models into one list (if not already)
all_models <- unlist(model_groups, recursive = FALSE)
names(all_models) <- paste(rep(names(model_groups), each = 3), 
                           unlist(model_names), sep = "_")

# 3. Perform LOO comparison
full_comparison <- loo::loo_compare(all_models)
# Combine all comparisons
full_comparison_df <- as.data.frame(full_comparison) %>%
  tibble::rownames_to_column("model") %>%
  mutate(
    group = gsub("_.*", "", model),
    model_name = gsub(".*_", "", model)
  ) %>%
  dplyr::select(model, group, model_name, elpd_diff, se_diff, everything())

full_comparison_df %>%
  ggplot(aes(x = elpd_diff, y = reorder(model_name, elpd_diff),
             color = group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggsci::scale_color_futurama() +
  labs(x = "ELPD difference (relative to best model in group)", 
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Show top models from each group
top_models <- full_comparison_df %>%
  group_by(group) %>%
  slice_max(order_by = elpd_diff, n = 3)

print(top_models %>% dplyr::select(group, model_name, elpd_diff, elpd_loo))

# Ridgeplots ####

meltDataframe <- rbind(summarydfK, summarydf1, as.data.frame(w_pred), 
                       summarydf0)

meltDataframe$model <- factor(meltDataframe$model, levels = c("Real Probability", 
                                                              "No Correlations", 
                                                              "One Correlation", 
                                                              "K Correlations"))

meltDataframegen<- rbind(summarydfgen0, summarydfgen1, w_pred, 
                         summarydfgen)
meltDataframegen$model<- factor(meltDataframegen$model, levels =c("Real Probability",
                                                                  "No Correlations",
                                                                  "One Correlation",
                                                                  "K Correlations"))

ggplot(meltDataframe, aes(x = w_prob, y = as.factor(K), fill = as.factor(K))) +
  geom_density_ridges(alpha = 0.7) +
  ggsci::scale_fill_futurama()+
  facet_grid(model ~ as.factor(N)) +
  labs(
    x = "Target Player Type Percentage",
    y = "Estimated Density",
    colour = "Cluster",
    fill = "Cluster",
    title = "Ridge Plot of P across Hierarchical Models"
  ) +
  theme_minimal()

ggplot(meltDataframe, aes(x = w_prob, y = model, fill = as.factor(K))) +
  geom_density_ridges(alpha = 0.7, quantile_lines = TRUE, quantiles = 2) +
  ggsci::scale_fill_futurama() +
  facet_grid(
    as.factor(K) ~ as.factor(N),
    labeller = labeller(
      .rows = function(x) stringr::str_to_title(x),  # Capitalize first letter only
      .cols = function(x) x                          # Leave N as-is
    )
  ) +
  labs(
    x = "Target Player Type Percentage",
    y = "Estimated Density",
    colour = "Cluster",
    fill = "Cluster",
    title = "Ridge Plot of P across Hierarchical Models"
  ) +
  theme_minimal()


ggplot(meltDataframegen, aes(x = w_prob, y = as.factor(K), fill = as.factor(K))) +
  geom_density_ridges(alpha = 0.7) +
  ggsci::scale_fill_futurama()+
  facet_grid(model ~ as.factor(N)) +
  labs(
    x = "Target Player Type Percentage",
    y = "Estimated Density",
    colour = "Cluster",
    fill = "Cluster",
    title = "Ridge Plot of P across General Prior Models"
  ) +
  theme_minimal()

ggplot(meltDataframegen, aes(x = w_prob, y = model, fill = as.factor(K))) +
  geom_density_ridges(alpha = 0.7, quantile_lines = TRUE, quantiles = 2) +
  ggsci::scale_fill_futurama() +
  facet_grid(
    as.factor(K) ~ as.factor(N),
    labeller = labeller(
      .rows = function(x) stringr::str_to_title(x),  # Capitalize first letter only
      .cols = function(x) x                          # Leave N as-is
    )
  ) +
  labs(
    x = "Target Player Type Percentage",
    y = "Estimated Density",
    colour = "Cluster",
    fill = "Cluster",
    title = "Ridge Plot of P across General Models"
  ) +
  theme_minimal()


# Error ParticipantxTrial ####

summetrics(error0genpart,"rmse")
summetrics(error1genpart,"rmse")
summetrics(errorkgenpart,"rmse")
summetrics(error0part,"rmse")
summetrics(error1part,"rmse")
summetrics(errorkpart,"rmse")

error <- bind_rows(
  error0part  %>% mutate(model = "No ρ",
                         hvsg = "Hierarchical"),
  error1part  %>% mutate(model = "General ρ",
                         hvsg = "Hierarchical"),
  errorkpart  %>% mutate(model = "Cluster ρ",
                         hvsg = "Hierarchical"),
  error0genpart %>% mutate(model = "No ρ",
                           hvsg = "General"),
  error1genpart %>% mutate(model = "General ρ",
                           hvsg = "General"),
  errorkgenpart  %>% mutate(model = "Cluster ρ",
                            hvsg = "General")
  )

ggplot(error, aes(rmse))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(error, rmse, groups= model)

# Cosine Similarity ParticipantxTrial ####

summetrics(cos0,"cosine_sim")
summetrics(cos1,"cosine_sim")
summetrics(cosK,"cosine_sim")
summetrics(cos0gen,"cosine_sim")
summetrics(cos1gen,"cosine_sim")
summetrics(coskgen,"cosine_sim")

cosim <- bind_rows(
  cos0  %>% mutate(model = "No ρ",
                         hvsg = "Hierarchical"),
  cos1  %>% mutate(model = "General ρ",
                         hvsg = "Hierarchical"),
  cosK  %>% mutate(model = "Cluster ρ",
                         hvsg = "Hierarchical"),
  cos0gen %>% mutate(model = "No ρ",
                           hvsg = "General"),
  cos1gen %>% mutate(model = "General ρ",
                           hvsg = "General"),
  coskgen  %>% mutate(model = "Cluster ρ",
                            hvsg = "General")
)

ggplot(cosim, aes(cosine_sim))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(cosim, cosine_sim, groups= model)

# KL Divergence ParticipantxTrial ####

summetrics(kl0,"kl")
summetrics(kl1,"kl")
summetrics(klK,"kl")
summetrics(kl0gen,"kl")
summetrics(kl1gen,"kl")
summetrics(klkgen,"kl")

kldiv <- bind_rows(
  kl0  %>% mutate(model = "No ρ",
                   hvsg = "Hierarchical"),
  kl1  %>% mutate(model = "General ρ",
                   hvsg = "Hierarchical"),
  klK  %>% mutate(model = "Cluster ρ",
                   hvsg = "Hierarchical"),
  kl0gen %>% mutate(model = "No ρ",
                     hvsg = "General"),
  kl1gen %>% mutate(model = "General ρ",
                     hvsg = "General"),
  klkgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(kldiv, aes(kl))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(kldiv, kl, groups= model)

# JS Divergence ParticipantxTrial ####

summetrics(js0,"js")
summetrics(js1,"js")
summetrics(jsK,"js")
summetrics(js0gen,"js")
summetrics(js1gen,"js")
summetrics(jskgen,"js")

jsdiv <- bind_rows(
  js0  %>% mutate(model = "No ρ",
                   hvsg = "Hierarchical"),
  js1  %>% mutate(model = "General ρ",
                   hvsg = "Hierarchical"),
  jsK  %>% mutate(model = "Cluster ρ",
                   hvsg = "Hierarchical"),
  js0gen %>% mutate(model = "No ρ",
                     hvsg = "General"),
  js1gen %>% mutate(model = "General ρ",
                     hvsg = "General"),
  jskgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(jsdiv, aes(js))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(jsdiv, js, groups= model)

# Brier-Score ParticipantxTrial ####

summetrics(bs0,"bs")
summetrics(bs1,"bs")
summetrics(bsK,"bs")
summetrics(bs0gen,"bs")
summetrics(bs1gen,"bs")
summetrics(bsKgen,"bs")

bsco <- bind_rows(
  bs0  %>% mutate(model = "No ρ",
                   hvsg = "Hierarchical"),
  bs1  %>% mutate(model = "General ρ",
                   hvsg = "Hierarchical"),
  bsK  %>% mutate(model = "Cluster ρ",
                   hvsg = "Hierarchical"),
  bs0gen %>% mutate(model = "No ρ",
                     hvsg = "General"),
  bs1gen %>% mutate(model = "General ρ",
                     hvsg = "General"),
  bsKgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(bsco, aes(bs))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(bsco, bs, groups = model)

# Log-Loss ParticipantxTrial ####

summetrics(log_losspart0, "log_loss")
summetrics(log_losspart1, "log_loss")
summetrics(log_losspartK, "log_loss")
summetrics(log_losspartgen0, "log_loss")
summetrics(log_losspartgen1, "log_loss")
summetrics(log_losspartgen, "log_loss")

logloss <- bind_rows(
  log_losspart0 %>% mutate(model = "No ρ",
                   hvsg = "Hierarchical"),
  log_losspart1  %>% mutate(model = "General ρ",
                   hvsg = "Hierarchical"),
  log_losspartK  %>% mutate(model = "Cluster ρ",
                   hvsg = "Hierarchical"),
  log_losspartgen0 %>% mutate(model = "No ρ",
                     hvsg = "General"),
  log_losspartgen1 %>% mutate(model = "General ρ",
                     hvsg = "General"),
  log_losspartgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(logloss, aes(log_loss))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(logloss, log_loss, groups= model)

# correlation ParticipantxTrial ####

#Participant level

summetrics(cpart0, "correlation")
summetrics(cpart1, "correlation")
summetrics(cpartk, "correlation")
summetrics(cpart0gen, "correlation")
summetrics(cgen1part, "correlation")
summetrics(cgenpart, "correlation")

corr <- bind_rows(
  cpart0  %>% mutate(model = "No ρ",
                  hvsg = "Hierarchical"),
  cpart1  %>% mutate(model = "General ρ",
                  hvsg = "Hierarchical"),
  cpartk  %>% mutate(model = "Cluster ρ",
                  hvsg = "Hierarchical"),
  cpart0gen %>% mutate(model = "No ρ",
                    hvsg = "General"),
  cgen1part %>% mutate(model = "General ρ",
                    hvsg = "General"),
  cgenpart  %>% mutate(model = "Cluster ρ",
                     hvsg = "General")
)

ggplot(corr, aes(correlation))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

summetrics(corr, "correlation", c("model", "hvsg"))

# ARI ParticipantxTrial ####

ari <- bind_rows(
  ari_result0part  %>% mutate(model = "No ρ",
                     hvsg = "Hierarchical"),
  ari_result1part  %>% mutate(model = "General ρ",
                     hvsg = "Hierarchical"),
  ari_resultkpart  %>% mutate(model = "Cluster ρ",
                     hvsg = "Hierarchical"),
  ari_result0partgen %>% mutate(model = "No ρ",
                       hvsg = "General"),
  ari_result1partgen %>% mutate(model = "General ρ",
                       hvsg = "General"),
  ari_resultkpartgen  %>% mutate(model = "Cluster ρ",
                       hvsg = "General")
)

ggplot(ari, aes(ari))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

summetrics(ari, "ari", c("model", "hvsg"))

# Total summary ####

error$metric<- "RMSE"
cosim$metric<- "Cosine Similarity"
kldiv$metric<- "KL Divergence"
jsdiv$metric<- "JS Divergence"
bsco$metric<- "Brier Score"
logloss$metric<- "Log-Loss"
corr$metric<- "Correlation"


names(error)[3]<- "value"
names(cosim)[3]<- "value"
names(kldiv)[3]<- "value"
names(jsdiv)[3]<- "value"
names(bsco)[3]<- "value"
names(logloss)[3]<- "value"
names(corr)[3]<- "value"


total_metric<- rbind(error,cosim,kldiv,jsdiv,bsco,logloss,corr)
metric_summary<- summetrics(total_metric, "value", c("model","hvsg","metric"))



