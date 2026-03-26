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

x_observed <- as.vector(t(as.matrix(unlist(data_k_0$x))))
y_observed <- as.vector(t(as.matrix(unlist(data_k_0$y))))

# Models####

kcorr_model<-"k_hierarchical_gen.stan" #K Correlation
kcorr_mod <- cmdstan_model( kcorr_model)
onecorr_model<-"one_hierarchical_gen.stan"
onecorr_mod <- cmdstan_model(onecorr_model)
ZCcorr_model<-"zero hierarchical gen.stan"
ZCcorr_mod <- cmdstan_model( ZCcorr_model)
kgencorr_model<-"k general gen.stan"
kgencorr_mod <- cmdstan_model( kgencorr_model)
onegencorr_mod<-"one general gen.stan"
onegencorr_mod <- cmdstan_model(onegencorr_mod)
Zgen_model<-"zero general gen.stan"
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

#MAE function
mae <- function(p_true, p_pred) {
  mean(abs(p_true - p_pred))
}
#Cosine Similarity 

cosine_similarity <- function(p_true, p_pred) {
  sum(p_true * p_pred) / (sqrt(sum(p_true^2)) * sqrt(sum(p_pred^2)))
}

#KL Divergence

kl_divergence <- function(p, q) {
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

huber_loss <- function(p_true, p_pred, alpha = 0.25) {
  delta <- p_true - p_pred
  mean(ifelse(
    abs(delta) <= alpha,
    0.5 * delta^2,
    alpha * (abs(delta) - 0.5 * alpha)
  ))
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
    fun = huber_loss,
    true = true_prob,
    pred = pred_prob,
    dataset = merged_data,
    letter= model_letter,
    name = "hl",
    fullname="hl",
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

prior_check<-function(mod, var1, var2, obs1, obs2, modname){
  x_observed <- as.vector(as.matrix(unlist(obs1)))
  x_observed_long<- as.data.frame(unlist(obs1))
  x_observed_long$I<- 1:nrow(x_observed_long)
  colnames(x_observed_long)[1:9]<- c(1:9)
  x_observed_long<- pivot_longer(x_observed_long,cols = c(1:9), names_to = "N")%>%
    mutate(N = as.integer(N))
  
  y_observed <- as.vector(as.matrix(unlist(obs2)))
  y_observed_long<- as.data.frame(unlist(obs2))
  y_observed_long$I<- 1:nrow(y_observed_long)
  colnames(y_observed_long)[1:9]<- c(1:9)
  y_observed_long<- pivot_longer(y_observed_long,cols = c(1:9), names_to = "N")%>%
    mutate(N = as.integer(N))
  
  x_rep<-mod$draws(var1, format = "matrix")
  y_rep<-mod$draws(var2, format = "matrix")
  # Create plots
  p1 <- bayesplot::ppc_dens_overlay(y = x_observed, yrep = x_rep) +
    ggtitle(paste0("Prior Predictive Check:", var1," ", modname))
  
  p2 <- bayesplot::ppc_dens_overlay(y = y_observed, yrep = y_rep) +
    ggtitle(paste0("Prior Predictive Check:", var2, " ", modname))
  
  # Combine plots with patchwork
  combined_plot <- p1 + p2 + plot_layout(ncol = 2)
  
  # Save plot
  plot_filename <- paste0("PPC_", modname, ".png")
  ggsave(plot_filename, combined_plot, width = 12, height = 6, dpi = 300)
  
  x_rep_clean <-summarise_draws(x_rep, mean)
  x_rep_clean  <- x_rep_clean  %>%
    separate(variable, into = c("prefix", "I", "N"), sep = "\\[|,|\\]", 
             extra = "drop",convert = TRUE) %>%
    dplyr:: select(-prefix)
  
  x <- left_join(x_observed_long, x_rep_clean, by = c("I", "N"))
  
  
  y_rep_clean <-summarise_draws(y_rep, mean)
  y_rep_clean  <- y_rep_clean  %>%
    separate(variable, into = c("prefix", "I", "N"), sep = "\\[|,|\\]", 
             extra = "drop",convert = TRUE) %>%
    dplyr:: select(-prefix)
  y <- left_join(y_observed_long, y_rep_clean, by = c("I", "N"))
  x$variable<-"Aesthetic Appreciation"
  y$variable<-"Organisation"
  prior_check_tot<-rbind(x,y)
  
  results <- prior_check_tot %>%
    group_by(variable) %>%
    summarise(
      correlation = cor(value, mean),
      cor_p_value = cor.test(value, mean)$p.value,  # p-value for correlation
      R2 = cor(value, mean)^2,
      MAE = mae(value, mean),
      RMSE= rmse(value, mean),
      HL= huber_loss(value, mean),
      .groups = "drop"
    )
  results_p <- prior_check_tot %>%
    group_by(variable, I) %>%
    summarise(
      correlation = cor(value, mean),
      cor_p_value = cor.test(value, mean)$p.value,  # p-value for correlation
      R2 = cor(value, mean)^2,
      MAE = mae(value, mean),
      RMSE= rmse(value, mean),
      HL= huber_loss(value, mean),
      .groups = "drop"
    )
  
  results_p<- pivot_longer(results_p, cols = c(3:8), names_to = "metric", values_to = "value")
  summary<- summetrics(results_p, group_by = "metric", metric = "value")
  
  invisible(list(
    overall = results,
    per_participant = results_p,
    participant_summary = summary,
    plot_file = plot_filename
  ))
  
}


model_run <- function(model, data, seed = 123, chains= 4, parallel = 4, 
                      warmup = 3000, sampling = 3000, treedepth=12, step = 0.1){
  return(
    model$sample(
      data = data,
      seed = seed,           
      chains = chains,           
      parallel_chains = parallel,  
      iter_warmup = warmup,   
      iter_sampling = sampling,
      max_treedepth = treedepth,
      step_size = step
    )
  )
}

prepare_data <- function(dat) {
  dat%>%
    dplyr::select(1:5) %>%  # Select first 5 columns
    rename(model:= model.x)
}

top_participants<- function(dataset, metric, threshold, up_or_down, 
                            group, rank_by, sort_order=" "){
  newdat <- dataset%>%
    summetrics(metric=metric, group_by = c(group))
  col_sym <- sym(paste0(metric, "_", rank_by))
  newdat <- newdat %>%
    dplyr::select(all_of(group), !!col_sym)
  
  filter_expr <- call2(as.name(up_or_down), col_sym, threshold)
  
  newdat %>%
    dplyr::filter(!!filter_expr) %>%
    dplyr::arrange(if (sort_order == "desc") desc(!!col_sym) else !!col_sym) %>%
    dplyr::select(all_of(group), !!col_sym)
}

ridgeplot<- function(dat1, dat2, dat3, dat4, x, y, fill, gridX, gridY, title, h_vs_g= "hier"){
  
  ndat1 <- prepare_data(dat1)
  ndat2 <- prepare_data(dat2)
  ndat3 <- prepare_data(dat3)
  ndat4<-dat4%>%
    rename(pred_prob= w_prob) 
  
  print(head(ndat1))
  print(head(ndat4))
  
  melt<- rbind(ndat1, ndat2, ndat3, ndat4)
  new_name<- paste0("melt_", h_vs_g)
  assign(new_name, melt, envir = .GlobalEnv)
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

nest_as_dat<- function(list, metric){
  combined_data <- bind_rows(
    as.data.frame(list[1])  %>% mutate(model = "No ρ",
                                       hvsg = "Hierarchical"),
    as.data.frame(list[2])  %>% mutate(model = "General ρ",
                                       hvsg = "Hierarchical"),
    as.data.frame(list[3])  %>% mutate(model = "Cluster ρ",
                                       hvsg = "Hierarchical"),
    as.data.frame(list[4]) %>% mutate(model = "No ρ",
                                      hvsg = "General"),
    as.data.frame(list[5]) %>% mutate(model = "General ρ",
                                      hvsg = "General"),
    as.data.frame(list[6])  %>% mutate(model = "Cluster ρ",
                                       hvsg = "General")
  )
  
  combined_data$metric<- metric
  names(combined_data)[2]<- "median"
  return(combined_data)
}

metric_tot <- function(datametric){
  dat0 <- get(paste0(datametric, "0"))
  dat1 <- get(paste0(datametric, "1"))
  dat2 <- get(paste0(datametric, "k"))
  dat3 <- get(paste0(datametric, "0gen"))
  dat4 <- get(paste0(datametric, "1gen"))
  dat5 <- get(paste0(datametric, "kgen"))
  
  
  combined_data <- bind_rows(
    dat0  %>% mutate(model = "No ρ",
                     hvsg = "Hierarchical"),
    dat1  %>% mutate(model = "General ρ",
                     hvsg = "Hierarchical"),
    dat2  %>% mutate(model = "Cluster ρ",
                     hvsg = "Hierarchical"),
    dat3 %>% mutate(model = "No ρ",
                    hvsg = "General"),
    dat4 %>% mutate(model = "General ρ",
                    hvsg = "General"),
    dat5  %>% mutate(model = "Cluster ρ",
                     hvsg = "General")
  )
  combined_data
}

expected_vals <- function(alpha, beta) {
  alpha / (alpha + beta)
}

fit_lmm<- function(dv, iv_formula, ran_eff, data, correction="holm"){
  require(afex)
  require(emmeans)
  formula<- paste0(dv, iv_formula, "+", ran_eff)
  formula<- as.formula(formula)
  fit<-mixed(formula, data)
  summary(fit)
  emm<-emmeans(fit,as.formula(iv_formula), adjust=correction)
  p_all <- as.data.frame(pairs(emm, adjust = correction))
  sig_contrast<- subset(p_all, p.value<0.05)[,c(1,2)]
  sig_contrast <- arrange(sig_contrast, contrast, desc(estimate))
  dimwise_sig_contrast <- sig_contrast %>%
    filter(!grepl("Aesthetic.*Organisation|Organisation.*Aesthetic", contrast)) %>%
    arrange(contrast, desc(estimate))
  nsig_contrast<- subset(p_all, p.value>0.05)[,c(1,2)]
  nsig_contrast <- arrange(nsig_contrast, contrast)
  assign("fit_ev_priors", fit, .GlobalEnv)
  assign("p_ev_pairs", p_all, .GlobalEnv)
  assign("sig_ev_pairs", sig_contrast, .GlobalEnv)
  assign("dimwise_ev_pairs", dimwise_sig_contrast, .GlobalEnv)
  assign("nsig_ev_pairs", nsig_contrast, .GlobalEnv)
}

#ev evaluation####
ev_x <- expected_vals(data_k_0$alphax, data_k_0$betax)
ev_y <- expected_vals(data_k_0$alphay, data_k_0$betay)

ev_long <- data.frame(
  Trait = as.factor(rep(c("Builder", "Explorer", "Competitor", "Pacifist"), each = nrow(ev_x)*2)),
  Context = as.factor(rep(c("Aesthetic", "Organisation"), each = nrow(ev_x), times = 4)),
  EV = c(as.vector(ev_x), as.vector(ev_y)),
  participant=rep(1:82, times=8)
)

fit_lmm("EV","~Context*Trait", "(1|participant)", ev_long)


# FITTING MODELS ####

# K model 
fitkcorr_h <-  model_run(kcorr_mod, data_k_0)
# 1 model 
fit1corr_h <-  model_run(onecorr_mod, data_1)
# Zero
fit0corr_h <-  model_run(ZCcorr_mod, data_k_0)
# General K 
fitkcorrgen <-  model_run(kgencorr_mod, data_gen)
# 1 model 
fit1corrgen <-  model_run(onegencorr_mod, data_1_gen)
# Zero
fit0corrgen <-  model_run(Zgen_mod, data_gen)

# summaries

create_summary(fitkcorr_h,"w_prob", "K Correlations", w_pred, "k")
create_summary(fit1corr_h,"w_prob", "One Correlation", w_pred, "1")
create_summary(fit0corr_h,"w_prob", "Zero Correlations", w_pred, "0")

create_summary(fitkcorrgen,"w_prob", "K Correlations", w_pred, "kgen")
create_summary(fit1corrgen,"w_prob", "One Correlation", w_pred, "1gen")
create_summary(fit0corrgen,"w_prob", "Zero Correlations", w_pred, "0gen")

#prior check

PPC_kh<-prior_check(fitkcorr_h, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "k")
PPC_1h<-prior_check(fit1corr_h, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "1")
PPC_0h<-prior_check(fit0corr_h, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "0")
PPC_kgen<-prior_check(fitkcorrgen, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "kgen")
PPC_1gen<-prior_check(fit1corrgen, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "1gen")
PPC_0gen<-prior_check(fit0corrgen, "x_rep", "y_rep", data_k_0$x, data_k_0$y, "0gen")

#-------------------------------------------
# PLOTS & REPORTS####
#-------------------------------------------
# Corr Plot ####
mergedmerged<- rbind(merged_data0, merged_data1, merged_datak)%>%group_by(N, K, model.x)%>%
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
          title = titleG, h_vs_g = "gen")

# Error ParticipantxTrial ####

rmse_list<-list(rmse0, rmse1, rmsek, rmse0gen, rmse1gen, rmsekgen)
lapply(rmse_list,  function(df) summetrics(data = df, metric = "rmse"))

participant_rmse_rank<- lapply(rmse_list,
                               function(df) top_participants(dataset=df, 
                                                             "rmse", 
                                                             0.866,
                                                             "<=",
                                                             "participant", 
                                                             "median"))

participant_rmse_rank <- nest_as_dat(participant_rmse_rank, "RMSE")

rmse_data <- metric_tot("rmse")

normalised<- rmse_data%>%
  group_by(model,hvsg)%>%
  summarise(normalised_rmse= rmse/(max(rmse)-min(rmse)), .groups = "drop")

summetrics(normalised, "normalised_rmse", group_by = c("model","hvsg"))[,1:4]

ggplot(rmse_data, aes(rmse))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(rmse_data, rmse, groups= model)

# Cosine Similarity ParticipantxTrial ####
cos_list<-list(cos0, cos1, cosk, cos0gen, cos1gen, coskgen)
lapply(cos_list,  function(df) summetrics(data = df, metric = "cosine_sim"))
participant_cos_rank<- lapply(cos_list,
                              function(df) top_participants(dataset=df, 
                                                            "cosine_sim", 
                                                            0.5,
                                                            ">=",
                                                            "participant", 
                                                            "median", 
                                                            sort_order = "desc"))
participant_cos_rank <- nest_as_dat(participant_cos_rank, "Cosine Similarity")

cosim <- metric_tot("cos")

ggplot(cosim, aes(cosine_sim))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(cosim, cosine_sim, groups= model)

# KL Divergence ParticipantxTrial ####
kl_list<-list(kl0, kl1, klk, kl0gen, kl1gen, klkgen)
lapply(kl_list,  function(df) summetrics(data = df, metric = "kl"))

participant_kl_rank<- lapply(kl_list,
                             function(df) top_participants(dataset=df, 
                                                           "kl", 
                                                           1.386,
                                                           "<=",
                                                           "participant", 
                                                           "median"))

participant_kl_rank <- nest_as_dat(participant_kl_rank, "KL Divergence")

kldiv <- metric_tot("kl")


ggplot(kldiv, aes(kl))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(kldiv, kl, groups= model)

# HuberLoss ParticipantxTrial ####
hl_list<-list(hl0, hl1, hlk, hl0gen, hl1gen, hlkgen)
lapply(hl_list,  function(df) summetrics(data = df, metric = "hl"))

participant_hl_rank<- lapply(hl_list,
                             function(df) top_participants(dataset=df, 
                                                           "hl", 
                                                           1.386,
                                                           "<=",
                                                           "participant", 
                                                           "median"))

participant_hl_rank <- nest_as_dat(participant_hl_rank, "Huber Loss")

hl <- metric_tot("hl")

total_merge<- rbind(merged_data0, merged_data0gen, 
                    merged_data1, merged_data1gen, 
                    merged_datak, merged_datakgen)
maxhl<- huber_loss(max(total_merge$true_prob), min(total_merge$pred_prob))
minhl<- huber_loss(min(total_merge$true_prob), min(total_merge$pred_prob))

normalised_hl<- hl%>%
  group_by(model,hvsg)%>%
  summarise(normalised_hl= hl/(maxhl-minhl), .groups = "drop")

summetrics(normalised_hl, "normalised_hl", group_by = c("model","hvsg"))[,1:4]

ggplot(hl, aes(hl))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(hl, hl, groups= model)

# JS Divergence ParticipantxTrial ####

js_list<-list(js0, js1, jsk, js0gen, js1gen,jskgen)
lapply(js_list,  function(df) summetrics(data = df, metric = "js"))

participant_js_rank<- lapply(js_list,
                             function(df) top_participants(dataset=df, 
                                                           "js", 
                                                           0.207,
                                                           "<=",
                                                           "participant", 
                                                           "median"))
participant_js_rank <- nest_as_dat(participant_js_rank, "JS Divergence")
jsdiv <- metric_tot("js")

ggplot(jsdiv, aes(js))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(jsdiv, js, groups= model)

# Brier-Score ParticipantxTrial ####

bs_list<-list(bs0, bs1, bsk, bs0gen, bs1gen,bskgen)
lapply(bs_list,  function(df) summetrics(data = df, metric = "bs"))

participant_bs_rank<- lapply(bs_list,
                             function(df) top_participants(dataset=df, 
                                                           "bs", 
                                                           0.75,
                                                           "<=",
                                                           "participant", 
                                                           "median"))

participant_bs_rank <- nest_as_dat(participant_bs_rank, "Brier Score")

bsco <- metric_tot("bs")

ggplot(bsco, aes(bs))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(bsco, bs, groups = model)

# Log-Loss ParticipantxTrial ####

log_loss_list<-list(log_loss0, log_loss1, log_lossk, 
                    log_loss0gen, log_loss1gen, log_losskgen
)

log_loss_summary<- lapply(log_loss_list,  
                          function(df) summetrics(data = df, 
                                                  metric = "log_loss"))

participant_log_rank<- lapply(log_loss_list,
                              function(df) top_participants(dataset=df, 
                                                            "log_loss", 
                                                            -log(1/4),
                                                            "<=",
                                                            "participant", 
                                                            "median"))

participant_log_rank <- nest_as_dat(participant_log_rank, "Log Loss")

logloss <- metric_tot("log_loss")

ggplot(logloss, aes(log_loss))+
  geom_histogram(colour="black")+
  facet_grid(hvsg~model)+
  theme(legend.position= "none")

sdamr::plot_raincloud(logloss, log_loss, groups= model)

# correlation ParticipantxTrial ####

c_list<-list(c0, c1, ck, c0gen, c1gen, ckgen)

lapply(c_list,  function(df) summetrics(data = df, metric = "correlation"))
participant_cor_rank<- lapply(c_list,
                              function(df) top_participants(dataset=df, 
                                                            "correlation", 
                                                            0,
                                                            ">=",
                                                            "participant", 
                                                            "median", sort_order = "desc"))

participant_cor_rank <- nest_as_dat(participant_cor_rank, "Correlation")

corr <- metric_tot("c")

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
hl<-prep(hl, "Huber Loss")
bsco<-prep(bsco, "Brier Score")
logloss<-prep(logloss, "Log-Loss")
corr<-prep(corr, "Correlation")


total_metric<- rbind(rmse_data,cosim,kldiv,jsdiv,bsco,logloss,corr,hl)
total_list<-list(rmse_data,cosim,kldiv,jsdiv,bsco,logloss,corr,hl)
total_metric%>%
  group_by(model, hvsg)%>%
  dplyr::filter(metric=="Log-Loss")%>%
  summarise(total_logloss = sum(value, na.rm = TRUE)) 

sum(total_metric$value[total_metric$metric=="Log-Loss"])

metric_summary<- summetrics(total_metric, "value", c("model","hvsg","metric"))

print(metric_summary[,c(1:3,5,7)], n=48)

metric_summary %>%
  dplyr::select(model, hvsg, metric, value_median, value_IQR) %>%
  arrange(metric, hvsg, model) %>%  # Group by metric first
  gt::gt() %>%
  gt::fmt_number(columns = c(value_median, value_IQR), decimals = 3) %>%
  gt::tab_header(title = "Model Performance Comparison")

participant_ranking<- rbind(participant_bs_rank, participant_cor_rank,
                            participant_cos_rank, participant_hl_rank,
                            participant_js_rank, participant_kl_rank,
                            participant_log_rank, participant_rmse_rank)

final_ranks<- participant_ranking %>%
  dplyr::filter(hvsg=="Hierarchical")%>%
  distinct(participant, model , metric) %>%
  group_by(participant) %>%
  summarise(n_groups = n(),
            models = list(unique(model)),
            metrics = list(unique(metric)), .groups = "drop")%>%
  arrange(desc(n_groups))

keep_objects <- c(
  # Merged datasets
  "total_merge",
  "melt_hier", "melt_gen",  # Ridgeplot data
  "total_metric", "final_ranks",  # Summary metrics
  
   #and PPC results
  "PPC_kh", "PPC_1h", "PPC_0h", "PPC_kgen", "PPC_1gen", "PPC_0gen",
  "p_ev_pairs", "fit_ev_priors"
)

saveRDS(fitkcorr_h, "fitkcorr_h.rds")
saveRDS(fit1corr_h, "fit1corr_h.rds")
saveRDS(fit0corr_h, "fit0corr_h.rds")
saveRDS(fitkcorrgen, "fitkcorrgen.rds")
saveRDS(fit1corrgen, "fit1corrgen.rds")
saveRDS(fit0corrgen, "fit0corrgen.rds")

rm(list = setdiff(ls(), keep_objects))
gc()