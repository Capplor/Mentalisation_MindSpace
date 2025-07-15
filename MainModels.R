#----------------------------------------
# Calling data if not run Main Data ####

all_data <- readRDS("all_data_lists.rds")
data_k_0 <- all_data$data_k_0
data_1 <-all_data$data_1
data_gen <-all_data$data_gen
data_1_gen <- all_data$data_1_gen
rm(all_data)
w_pred<- read_xlsx("w_pred.xlsx")
w_pred$model<- "real probability"
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

log_loss_expe<- function(p_pred, p_true) {-(sum(p_pred * log(p_true) + (1 - p_pred) * log(1 - p_true)))
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

#Brier-Score Divergence

brier_score <- function(p_pred, p_true) {
  sum((p_pred - p_true)^2+((1-p_pred)-(1-p_true))^2)
}


#Summarise

library(rlang)

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

metricdata<- function(fun, true, pred, dataset, letter, name, fullname,  ...){
  true <- enquo(true)
  pred <- enquo(pred)
  
  result <- dataset %>%
    group_by(...) %>%
    summarise(
      metric = fun(!!true, !!pred),
      .groups = "drop"
    )%>%
    rename(!!fullname := metric) 
  
  # Create the new object name
  new_name <- paste0(name, letter)
  # Assign to global environment
  assign(new_name, result, envir = .GlobalEnv)
  
  # Return the result invisibly (optional)
  invisible(result)
}

newcor<- function(true_prob, pred_prob){
  ifelse(sd(true_prob) > 0 & sd(pred_prob) > 0,
         cor(true_prob, pred_prob),
         NA_real_)}

create_summary<- function(mod, variable, model_type, true_vals, model_letter){
  
  w_pred <- true_vals
  w_prob <- mod$draws(variables=variable, format= "matrix")
  summary_stats  <- summarize_draws(w_prob , mean)
  summarydf  <- summary_stats  %>%
    separate(variable, into = c("prefix", "I", "N", "K"), sep = "\\[|,|\\]", 
             extra = "drop",convert = TRUE) %>%
    dplyr:: select(-prefix)
  summarydf <-summarydf[,c(1:4)]
  colnames(summarydf) <- c("participant", "N", "K","w_prob")
  summarydf$K <- dplyr:: recode(summarydf$K, 
                                `1` = "builder", 
                                `2` = "explorer", 
                                `3` = "competitor", 
                                `4` = "pacifist")
  summarydf$model <- as_string(model_type)
  summarydf$model<- as.factor(summarydf$model)
  summarydf$N<- paste0("t",summarydf$N)
  summarydf$participant<- as.factor(summarydf$participant)
  
  merged_data <- summarydf %>%
    rename(pred_prob = w_prob) %>%
    left_join(w_pred %>% rename(true_prob = w_prob), by = c("participant", "K","N"))
  
  metricdata(
    fun = log_loss,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "log_loss",
    fullname = "log_loss",
    participant, N)
  metricdata(
    fun = newcor,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "c",
    fullname = "correlation",
    participant, N) 
  metricdata(
    fun = rmse,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "rmse",
    fullname = "rmse",
    participant, N) 
  metricdata(
    fun = cosine_similarity,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "cos",
    fullname = "cosine_sim",
    participant, N) 
  metricdata(
    fun = kl_divergence,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "kl",
    fullname="kl",
    participant, N) 
  metricdata(
    fun = js_divergence,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "js",
    fullname="js",
    participant, N) 
  metricdata(
    fun = brier_score,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "bs",
    fullname = "bs",
    participant, N) 
  # Create the new object name
  new_name <- paste0("merged_data", model_letter)
  
  # Assign to global environment
  assign(new_name, merged_data, envir = .GlobalEnv)
}

model_run <- function(model, data){
  return(
    model$sample(
    data = data,
    seed = 123,           # Set a seed for reproducibility
    chains = 4,           # Number of MCMC chains
    parallel_chains = 4,  # Number of chains to run in parallel
    iter_warmup = 3000,   # Number of warmup iterations
    iter_sampling = 3000,# Number of sampling iterations
    max_treedepth = 12,
    step_size = 0.1
  )
  )
}

prepare_data <- function(dat) {
  dat%>%
    dplyr::select(1:5) %>%  # Select first 5 columns
    rename(model:= model.x)
}

ridgeplot<- function(dat1, dat2, dat3, dat4, x, y, fill, gridX, gridY, title){
  
  ndat1 <- prepare_data(dat1)
  ndat2 <- prepare_data(dat2)
  ndat3 <- prepare_data(dat3)
  ndat4<-dat4%>%
    rename(pred_prob= w_prob) 
  
  print(head(ndat1))
  print(head(ndat4))
  
  melt<- rbind(ndat1, ndat2, ndat3, ndat4)
  print(head(melt))
  print(ggplot(melt, aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])) +
          geom_density_ridges(alpha = 0.7) +
          ggsci::scale_fill_futurama() +
          facet_grid(reformulate(gridX, gridY)) +  # This creates the facet formula
          labs(
            x = "Target Player Type Percentage",
            y = "Estimated Density",
            colour = "Cluster",
            fill = "Cluster",
            title = title
          ) +
          theme_minimal())
}

# model difference with statistical significance
prep<-function(dataset, metric_name){
  dataset$metric<-metric_name
  names(dataset)[3]<- "value"
  return(dataset)
}

model_comp<- function(dataset, formula =value~model*hvsg + (1|participant)+(1|N)){
  dataset$model<- as.factor(dataset$model)
  custom_contrasts <- matrix(
    c(-0.5,  0,
      -0.5,  1,
      1,   -1),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      levels(dataset$model),
      c("0_vs_Correlations", "1_vs_K")
    )
  )
  contrasts(dataset$model)<-custom_contrasts
  mod<- lmer(formula, dataset)
  res<-summary(mod)
  result<- list(
    metric = unique(dataset$metric),
    coefficients = res$coefficients)
}

# FITTING MODELS ####

# K model 
fitkcorr_h <-  model_run(kcorr_mod, data_k_0)
# 1 model 
fit1corr_h <-  model_run(onecorr_mod, data_1)
# Zero
zerocorrmod <-  model_run(ZCcorr_mod, data_k_0)
# General K 
fitkcorrgen <-  model_run(kgencorr_mod, data_gen)
# 1 model 
fit1corrgen <-  model_run(onegencorr_mod, data_1_gen)
# Zero
zerocorrgen <-  model_run(Zgen_mod, data_gen)

# summaries

create_summary(fitkcorr_h,"w_prob", "K Correlations", w_pred, "k")
create_summary(fit1corr_h,"w_prob", "One Correlation", w_pred, "1")
create_summary(zerocorrmod,"w_prob", "Zero Correlations", w_pred, "0")

create_summary(fitkcorrgen,"w_prob", "K Correlations", w_pred, "kgen")
create_summary(fit1corrgen,"w_prob", "One Correlation", w_pred, "1gen")
create_summary(zerocorrgen,"w_prob", "Zero Correlations", w_pred, "0gen")

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

# LOO-CV ####

loo_kcorr_h <- fitkcorr_h$loo("log_lik")
loo_1corr_h <- fit1corr_h$loo("log_lik")
loo_zero <- zerocorrmod$loo("log_lik")
loo_gen_kcorr <- fitkcorrgen$loo("log_lik")
loo_gen_1corr <- fit1corrgen$loo("log_lik")
loo_gen_zero <- zerocorrgen$loo("log_lik")

# LOO Raw Values ####

loo_kcorr_h$estimates
loo_1corr_h$estimate
loo_zero$estimates
loo_gen_kcorr$estimates
loo_gen_1corr$estimates
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

all_models <- unlist(model_groups, recursive = FALSE)
names(all_models) <- paste(rep(names(model_groups), each = 3), 
                           unlist(model_names), sep = "_")

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

titleH<-"Ridge Plot of P across Hierarchical Models"
ridgeplot(merged_data0, merged_data1, merged_datak, w_pred, 
          x = "pred_prob", y = "K", fill = "K", 
          gridY = "model", gridX = "N", 
          title = titleH)

titleG<-"Ridge Plot of P across General Models"
ridgeplot(merged_data0gen, merged_data1gen, merged_datakgen, w_pred, 
          x = "pred_prob", y = "K", fill = "K", 
          gridY = "model", gridX = "N", 
          title = titleG)

# Error ParticipantxTrial ####

rmse_list<-list(rmse0, rmse1, rmsek, rmse0gen, rmse1gen, rmsekgen)
lapply(rmse_list,  function(df) summetrics(data = df, metric = "rmse"))

rmse_data <- bind_rows(
  rmse0  %>% mutate(model = "No ρ",
                         hvsg = "Hierarchical"),
  rmse1  %>% mutate(model = "General ρ",
                         hvsg = "Hierarchical"),
  rmsek  %>% mutate(model = "Cluster ρ",
                         hvsg = "Hierarchical"),
  rmse0gen %>% mutate(model = "No ρ",
                           hvsg = "General"),
  rmse1gen %>% mutate(model = "General ρ",
                           hvsg = "General"),
  rmsekgen  %>% mutate(model = "Cluster ρ",
                            hvsg = "General")
  )

ggplot(rmse_data, aes(rmse))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(rmse_data, rmse, groups= model)

# Cosine Similarity ParticipantxTrial ####
cos_list<-list(cos0, cos1, cosK, cos0gen, cos1gen, coskgen)
lapply(cos_list,  function(df) summetrics(data = df, metric = "cosine_sim"))

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
kl_list<-list(kl0, kl1, klk, kl0gen, kl1gen, klkgen)
lapply(kl_list,  function(df) summetrics(data = df, metric = "kl"))

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

js_list<-list(js0, js1, jsk, js0gen, js1gen,jskgen)
lapply(js_list,  function(df) summetrics(data = df, metric = "js"))

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

bs_list<-list(bs0, bs1, bsk, bs0gen, bs1gen,bskgen)
lapply(bs_list,  function(df) summetrics(data = df, metric = "bs"))


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
  bskgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(bsco, aes(bs))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(bsco, bs, groups = model)

# Log-Loss ParticipantxTrial ####

log_loss_list<-list(log_loss0, log_loss1, log_lossk, 
           log_loss0gen, log_loss1gen, log_losskgen
           )

lapply(log_loss_list,  function(df) summetrics(data = df, metric = "log_loss"))

logloss <- bind_rows(
  log_loss0 %>% mutate(model = "No ρ",
                   hvsg = "Hierarchical"),
  log_loss1  %>% mutate(model = "General ρ",
                   hvsg = "Hierarchical"),
  log_lossk  %>% mutate(model = "Cluster ρ",
                   hvsg = "Hierarchical"),
  log_loss0gen %>% mutate(model = "No ρ",
                     hvsg = "General"),
  log_loss1gen %>% mutate(model = "General ρ",
                     hvsg = "General"),
  log_losskgen  %>% mutate(model = "Cluster ρ",
                      hvsg = "General")
)

ggplot(logloss, aes(log_loss))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(logloss, log_loss, groups= model)

# correlation ParticipantxTrial ####

c_list<-list(c0, c1, ck, c0gen, c1gen, ckgen)

lapply(c_list,  function(df) summetrics(data = df, metric = "correlation"))

corr <- bind_rows(
  c0  %>% mutate(model = "No ρ",
                  hvsg = "Hierarchical"),
  c1  %>% mutate(model = "General ρ",
                  hvsg = "Hierarchical"),
  ck  %>% mutate(model = "Cluster ρ",
                  hvsg = "Hierarchical"),
  c0gen %>% mutate(model = "No ρ",
                    hvsg = "General"),
  c1gen %>% mutate(model = "General ρ",
                    hvsg = "General"),
  ckgen  %>% mutate(model = "Cluster ρ",
                     hvsg = "General")
)

ggplot(corr, aes(correlation))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

cor_summary <- summetrics(corr, "correlation", group_by = c("model", "hvsg"))

# Plot
ggplot(cor_summary, aes(x = hvsg, y = correlation_mean, fill = model)) +
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


summetrics(corr, "correlation", c("model", "hvsg"))

# Total summary ####

rmse_data<-prep(rmse_data, "RMSE")
cosim<-prep(cosim, "Cosine Similarity")
kldiv<-prep(kldiv, "KL Divergence")
jsdiv<-prep(jsdiv, "JS Divergence")
bsco<-prep(bsco, "Brier Score")
logloss<-prep(logloss, "Log-Loss")
corr<-prep(corr, "Correlation")


total_metric<- rbind(rmse_data,cosim,kldiv,jsdiv,bsco,logloss,corr)
total_list<-list(rmse_data,cosim,kldiv,jsdiv,bsco,logloss,corr)
total_metric%>%
  group_by(model, hvsg)%>%
  dplyr::filter(metric=="Log-Loss")%>%
  summarise(total_logloss = sum(value, na.rm = TRUE)) 

sum(total_metric$value[total_metric$metric=="Log-Loss"])

lapply(total_list, function(x) model_comp(dataset=x))

metric_summary<- summetrics(total_metric, "value", c("model","hvsg","metric"))

print(metric_summary[,c(1:3,5,7)], n=42)

metric_summary %>%
  dplyr::select(model, hvsg, metric, value_median, value_IQR) %>%
  arrange(metric, hvsg, model) %>%  # Group by metric first
  gt::gt() %>%
  gt::fmt_number(columns = c(value_median, value_IQR), decimals = 3) %>%
  gt::tab_header(title = "Model Performance Comparison")

