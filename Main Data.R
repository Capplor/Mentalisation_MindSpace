# Importing dataset ####
set.seed(123)
study1 <- read_excel("study1.xlsx")
colnames(study1)

# Study Columns ####

# All Columns that are out of a hundred
col_100<- c(5:18,21:30,32:41,43:52,54:63,65:68,72:75,79:82,86:89,93:96,100:103,
            107:110, 114:117, 121:124) 

#Initial dimension columns per cluster
init_a<- c(9, 21, 32, 43, 54) #Aesthetic Appretiation
init_o <- c(14, 26, 37, 48, 59) #Organisation 


# Elicited distribution Aesthetic Appreciation
colag<- c(9:13)   # General
colab<- c(21:25)  # Builder
colae<- c(32:36)  # Explorer
colac<- c(43:47)  # Competitor
colap<- c(54:58)  # Pacifist

# Elicited distribution Organisation

colog<- c(14:18)  # General
colob<- c(26:30)  # Builder
coloe<- c(37:41)  # Explorer
coloc<- c(48:52)  # Competitor
colop<- c(59:63)  # Pacifist

# Dirichlet prior
col_theta<-(5:8)

#Correlation per cluster ~ General, Builder, Explorer, Competitor, Pacifist
col_rho<- c(19,31,42,53,64)

#Target dimension ratings per trial
col_dim<- c(70,71,77,78,84,85,91,92,98,99,105,106,112, 113,119,120,126,127)

# Target Player type per trial mixing probabilities
col_w<- c(65:68,72:75,79:82,86:89,93:96,100:103,107:110, 114:117, 121:124)

# Player type per trial
col_1 <- c(65:68)
col_2<- c(72:75)
col_3<-c(79:82)
col_4 <- c(86:89)
col_5<- c(93:96)
col_6 <- c(100:103)
col_7<- c(107:110)
col_8 <- c(114:117)
col_9<- c(121:124)

#Final Clusters Per Target Per trial
col_fc<- c(69,76,83,90,97,104,111, 118, 125)

# Columns to make numeric
col_conv<- c(col_100, col_dim, col_rho)

#Changing headers

colnames(study1)[col_theta]<-c("theta_builder","theta_explorer",
                               "theta_competitor","theta_pacifist")

colnames(study1)[col_1]<- c("t1_builder", "t1_explorer", "t1_competitor", 
                            "t1_pacifist")

colnames(study1)[col_2]<- c("t2_builder", "t2_explorer", "t2_competitor", 
                            "t2_pacifist")

colnames(study1)[col_3]<- c("t3_builder", "t3_explorer", "t3_competitor", 
                            "t3_pacifist")

colnames(study1)[col_4]<- c("t4_builder", "t4_explorer", "t4_competitor", 
                            "t4_pacifist")

colnames(study1)[col_5]<- c("t5_builder", "t5_explorer", "t5_competitor", 
                            "t5_pacifist")

colnames(study1)[col_6]<- c("t6_builder", "t6_explorer", "t6_competitor", 
                            "t6_pacifist")

colnames(study1)[col_7]<- c("t7_builder", "t7_explorer", "t7_competitor", 
                            "t7_pacifist")

colnames(study1)[col_8]<- c("t8_builder", "t8_explorer", "t8_competitor", 
                            "t8_pacifist")

colnames(study1)[col_9]<- c("t9_builder", "t9_explorer", "t9_competitor", 
                            "t9_pacifist")

colnames(study1)[col_dim]<-c("x_t1", "y_t1", "x_t2","y_t2", "x_t3", "y_t3", 
                             "x_t4", "y_t4", "x_t5","y_t5", "x_t6", "y_t6", 
                             "x_t7","y_t7", "x_t8", "y_t8", "x_t9","y_t9")

colnames(study1)[col_rho]<-c("rho_general", "rho_builder", "rho_explorer", 
                             "rho_competitor", "rho_pacifist")

# Making sure everything has the right shape and constraint
study1[,col_conv]<- lapply(study1[,col_conv], as.numeric)
study1[,col_100]<-study1[,col_100]/100
study1[,col_dim]<-study1[,col_dim]/50
study1[,col_rho]<-study1[,col_rho]/100

for(i in col_rho){
  study1[[i]]<- ifelse(study1[[i]]==1, 0.99, study1[[i]])
  study1[[i]]<- ifelse(study1[[i]]==-1, -0.99, study1[[i]])
}
for(i in col_rho){
  print(range(study1[[i]]))
}

# Participant Exclusion ####

##vals = bin edges
##p = values of the histogram
vals<- c(.15, .30, .45, .60, .75)
#study1[9, 50]<- 0.04

excl_1<- array(dim=nrow(study1))

for(i in 1:nrow(study1)){
  if (sum(study1[i, c(9:18)]) < 2) {
    print(paste("Row", i, "has not a sum of 2 for general"))
    excl_1[i]<-as.numeric(i)
  }
  if (sum(study1[i, c(21:30)]) < 2) {
    print(paste("Row", i, "has not a sum of 2 for builder"))
    excl_1[i]<-as.numeric(i)
  }
  if (sum(study1[i, c(32:41)]) < 2) {
    print(paste("Row", i, "has not a sum of 2 for explorer"))
    excl_1[i]<-as.numeric(i)
  }
  if (sum(study1[i, c(43:52)]) < 2) {
    print(paste("Row", i, "has not a sum of 2 for competitor"))
    excl_1[i]<-as.numeric(i)
  }
  if (sum(study1[i, c(54:63)]) < 2) {
    print(paste("Row", i, "has a sum of 1 in columns pacifist"))
    excl_1[i]<-as.numeric(i)
  }
}

study1<- study1[-as.numeric(unique(excl_1[!is.na(excl_1)])),]

# Elicited Dimensions Distribution ####

##vals = bin edges
##p = values of the histogram
vals<- c(.2, .4, .6, .8)

# Calculating Cumulative Distributions ####
# Dimension General ####
aes_cumulative<-numeric(nrow(study1))

for(f in colag){
  aes_cumulative<-aes_cumulative + study1[,f]
  study1[, paste0("aes_cum", f-8)]<-aes_cumulative
}



org_cumulative<-numeric(nrow(study1))

for(f in colog){
  org_cumulative<-org_cumulative + study1[,f]
  study1[, paste0("org_cum", f-13)]<-org_cumulative
}

# Dimension Builder ####

aes_cumulativeb<-numeric(nrow(study1))


for(f in colab){
  aes_cumulativeb <-aes_cumulativeb + study1[,f]
  study1[, paste0("aes_cum_b", f-20)]<-aes_cumulativeb
}


org_cumulativeb<-numeric(nrow(study1))


for(f in colob){
  org_cumulativeb <-org_cumulativeb + study1[,f]
  study1[, paste0("org_cum_b", f-25)]<-org_cumulativeb
}

# Dimensions Explorer ####

aes_cumulativee<-numeric(nrow(study1))


for(f in colae){
  aes_cumulativee <-aes_cumulativee + study1[,f]
  study1[, paste0("aes_cum_e", f-31)]<-aes_cumulativee
}

org_cumulativee<-numeric(nrow(study1))


for(f in coloe){
  org_cumulativee <-org_cumulativee + study1[,f]
  study1[, paste0("org_cum_e", f-36)]<-org_cumulativee
}

# Dimensions Competitors ####

aes_cumulativec<-numeric(nrow(study1))

for(f in colac){
  aes_cumulativec <-aes_cumulativec + study1[,f]
  study1[, paste0("aes_cum_c", f-42)]<-aes_cumulativec
}

org_cumulativec<-numeric(nrow(study1))


for(f in coloc){
  org_cumulativec <-org_cumulativec + study1[,f]
  study1[, paste0("org_cum_c", f-47)]<-org_cumulativec
}

# Dimensions pacifist ####

aes_cumulativep<-numeric(nrow(study1))


for(f in colap){
  aes_cumulativep <-aes_cumulativep + study1[,f]
  study1[, paste0("aes_cum_p", f-53)]<-aes_cumulativep
}

org_cumulativep<-numeric(nrow(study1))


for(f in colop){
  org_cumulativep <-org_cumulativep + study1[,f]
  study1[, paste0("org_cum_p", f-58)]<-org_cumulativep
}

# Replace 1s ####
study1 <- study1 %>%
  mutate(across(149:198, ~ replace(., . == 1, 0.7))) # to include only when p[4]< 0.6


range(study1[,149:198])

colnames(study1)

rm(aes_cumulative,org_cumulative,aes_cumulativeb,org_cumulativeb,
   aes_cumulativee,org_cumulativee,aes_cumulativec,
   org_cumulativec,aes_cumulativep,org_cumulativep)
# Fitting distributions ####

initial_number_aes<-c(149,159,169,179,189)
initial_number_org<-c(154,164,174,184,194)

alpha_aes<-matrix(nrow=nrow(study1),ncol=5)
beta_aes<-matrix(nrow=nrow(study1),ncol=5)
alpha_org<-matrix(nrow=nrow(study1),ncol=5)
beta_org<-matrix(nrow=nrow(study1),ncol=5)
shape_org<-matrix(nrow=nrow(study1),ncol=2*5)
shape_aes<-matrix(nrow=nrow(study1),ncol=2*5)

# Fitting Aesthetic Distribution ####
for(i in initial_number_aes){
  start_col <- i
  cluster_cols <- ((match(i, initial_number_aes) - 1) * 2 + 1):(match(i, initial_number_aes) * 2) 
  
  for(k in 1:nrow(study1)){
    p <- as.numeric(study1[k, i:(i+4)])
    p <- c(p, 0.30)
    p <- p[c(6, 1, 2, 3, 4, 5)]
    # Logic to make sure elicited probabilities have values that include p<0.4, p>0.6
    #Without including extreme values
    if(min(p[2:5]) >= 0.4 && max(p[2:5]) <= 0.6){
      vals1 <- c(0.10, 0.2, 0.4, 0.6, 0.8, 0.9)
      fit <- SHELF::fitdist(vals1, p, lower=0, upper=1)
    } else if(min(p[2:5]) >= 0.4 && max(p[2:5]) > 0.6){
      vals2 <- c(0.010, 0.2, 0.4, 0.6, 0.8)
      fit <- SHELF::fitdist(vals2, p[1:5], lower=0, upper=1)
    } else if(min(p[2:5]) < 0.4 && max(p[2:5]) <= 0.6){
      vals3 <- c(0.2, 0.4, 0.6, 0.8, 0.9)
      fit <- SHELF::fitdist(vals3, p[2:6], lower=0, upper=1)
    } else if(min(p[2:5]) <= 0.4 && max(p[2:5]) >= 0.6){
      vals4 <- c(0.2, 0.4, 0.6, 0.8)
      fit <- SHELF::fitdist(vals4, p[2:5], lower=0, upper=1)
    }else {
      message(paste0("Skipping row ", k, ", cluster_cols ", 
                     paste(cluster_cols, collapse = ","), "values:",
                     paste(p,collapse = ",")))
      next
    }
    
    shape_aes[k, cluster_cols] <- as.numeric(fit$Beta)
  }
}


alpha_aes<- shape_aes[,c(1,3,5,7,9)]
beta_aes<- shape_aes[,c(2,4,6,8,10)]

# Fitting Organisation Distribution ####
for(i in initial_number_org){
  start_col <- i
  cluster_cols <- ((match(i, initial_number_org) - 1) * 2 + 1):(match(i, initial_number_org) * 2) 
  
  for(k in 1:nrow(study1)){
    p <- as.numeric(study1[k, i:(i+4)])
    p <- c(p, 0.30)
    p <- p[c(6, 1, 2, 3, 4, 5)]
    
    if(min(p[2:5]) >= 0.4 && max(p[2:5]) <= 0.6){
      vals1 <- c(0.10, 0.2, 0.4, 0.6, 0.8, 0.9)
      fit <- SHELF::fitdist(vals1, p, lower=0, upper=1)
    } else if(min(p[2:5]) >= 0.4 && max(p[2:5]) > 0.6){
      vals2 <- c(0.010, 0.2, 0.4, 0.6, 0.8)
      fit <- SHELF::fitdist(vals2, p[1:5], lower=0, upper=1)
      
    } else if(min(p[2:5]) < 0.4 && max(p[2:5]) <= 0.6){
      vals3 <- c(0.2, 0.4, 0.6, 0.8, 0.9)
      fit <- SHELF::fitdist(vals3, p[2:6], lower=0, upper=1)
      
    }else if(min(p[2:5]) <= 0.4 && max(p[2:5]) >= 0.6){
      vals4 <- c(0.2, 0.4, 0.6, 0.8)
      fit <- SHELF::fitdist(vals4, p[2:5], lower=0, upper=1)
      
    }else {
      message(paste0("Skipping row ", k, ", cluster_cols ", 
                     paste(cluster_cols, collapse = ","), "values:",
                     paste(p,collapse = ",")))
      next
    }
    
    shape_org[k, cluster_cols] <- as.numeric(fit$Beta)
  }
}

alpha_org<- shape_org[,c(1,3,5,7,9)]
beta_org<- shape_org[,c(2,4,6,8,10)]

colnames(shape_aes) <- c("a_aes_g", "b_aes_g","a_aes_b","b_aes_b", "a_aes_e",
                         "b_aes_e","a_aes_c","b_aes_c","a_aes_p","b_aes_p")

colnames(shape_org) <- c("a_org_g", "b_org_g","a_org_b","b_org_b", "a_org_e",
                         "b_org_e","a_org_c","b_org_c","a_org_p","b_org_p")

range(shape_aes)
range(shape_org)

# Elicited Mixing Probabilities (w_pred) ####

names_w<-colnames(study1[,col_w])
w_predwide<- study1[,col_w]
w_predwide$participant<-1:nrow(w_predwide)
w_predwide<-w_predwide[,c(c(length(names_w)+1),1:length(names_w))]
w_pred<- w_predwide %>% pivot_longer(cols = names_w,
                                     names_to = "trialxk",
                                     values_to = "w_prob") %>%
  separate(col="trialxk", into= c("N", "K"), sep = "_")

w_predwide<- w_pred %>% pivot_wider(names_from = "K", values_from = "w_prob")
w_predwide$highval <- as.factor(apply(w_predwide[,c(3:6)], 1,
                                      function(x) names(w_predwide[,c(3:6)])[which.max(x)]))
w_predwide[,c(1,2,7)]<-lapply(w_predwide[,c(1,2,7)],as.factor)
w_pred[,c(1:3)]<-lapply(w_pred[,c(1:3)],as.factor)

# Final Clusters ####
names_fc<-colnames(study1[,col_fc])
fc_predwide<- study1[,col_fc]
fc_predwide$participant<-1:nrow(fc_predwide)
fc_predwide<-fc_predwide[,c(c(length(names_fc)+1),(1:length(names_fc)))]
fc_pred<- fc_predwide %>% pivot_longer(cols = names_fc,
                                       names_to = "t_fc",
                                       values_to = "fc") %>%
  separate(col="t_fc", into= c("f", "N"), sep = "r")
fc_pred$N<-paste0("t",fc_pred$N)
fc_pred$fc<-tolower(fc_pred$fc)
fc_pred<- fc_pred[,-2]

highval_tot <- merge(w_predwide, fc_pred, 
                     by = intersect(names(w_predwide), names(fc_pred)), 
                     all = TRUE)
for(i in 1:nrow(highval_tot)){
  if (highval_tot[i,7]!=highval_tot[i,8]){
    print(highval_tot[i,c(1,3:8)])
  }
}


# Dimension Target Ratings, Aesthetics = x, Organisation = y ####
names_xy<-colnames(study1[,col_dim])
xy_predwide<- study1[,col_dim]
xy_predwide$participant<-1:nrow(xy_predwide)
xy_predwide<-xy_predwide[,c(c(length(names_xy)+1),1:length(names_xy))]
xy_pred<- xy_predwide %>% pivot_longer(cols = names_xy,
                                       names_to = "trialxdim",
                                       values_to = "value") %>%
  separate(col="trialxdim", into= c("dim", "N"), sep = "_")
xy_pred$value<- ifelse(xy_pred$value>=0.99,0.99,xy_pred$value)
xy_pred$value<- ifelse(xy_pred$value==0,0.01,xy_pred$value)

xy_predwide<- xy_pred %>% pivot_wider(names_from = "dim", values_from = "value")
xy_predwide[,c(1,2)]<-lapply(xy_predwide[,c(1,2)],as.factor)
xy_pred[,c(1:3)]<-lapply(xy_pred[,c(1:3)],as.factor)

total_wide <- merge(highval_tot, xy_predwide, 
                    by = intersect(names(highval_tot), names(xy_predwide)), 
                    all = TRUE)
total_wide$fc<- as.factor(total_wide$fc)

# Correlation (rho) ####

names_r<-colnames(study1[,c(col_rho)])
r_predwide<- study1[,col_rho]
r_predwide$participant<-1:nrow(r_predwide)
r_predwide<-r_predwide[,c(c(length(names_r)+1),1:length(names_r))]
r_pred<- r_predwide %>% pivot_longer(cols = names_r,
                                     names_to = "typexrho",
                                     values_to = "rho") %>%
  separate(col="typexrho", into= c("r", "K"), sep = "_")
r_pred<-  r_pred[,c(1,3,4)]
r_predwide<- r_pred %>% pivot_wider(names_from = "K", values_from = "rho")

r_one<- r_predwide[,1:2]
r_k<- r_predwide[,c(1,3:6)]


color_scheme_set("brightblue")

# Final Data #####
theta<- as.array(as.matrix(study1[,c(col_theta)]))
alphax<- alpha_aes[,2:5]
betax<- beta_aes[,2:5]
alphay<- alpha_org[,2:5]
betay<- beta_org[,2:5]
rhok<- as.array(as.matrix(r_k[,2:5]))
rho1<- as.array(as.matrix(r_one[,2]))

for(i in 1:4){
  hist(theta[,i]*8)
}
theta_p<-as.data.frame(theta)
theta_p$p<-1:82
theta_long<-pivot_longer(theta_p,cols=colnames(theta), 
                         names_to = "cluster", values_to = "value")


xy_wide<-  xy_pred %>% pivot_wider(names_from = c("N", "dim"), values_from = "value")


x<- as.array(as.matrix(xy_wide[,c(2,4,6,
                                  8,10,12,
                                  14,16,18)]))
y<- as.array(as.matrix(xy_wide[,c(3,5,7,
                                  9,11,13,
                                  15,17,19)]))
w_pred$participant<-as.factor(w_pred$participant)
#new theta

thetan <- theta # Copy theta to preserve original data
thetan[thetan < 0.2] <- 0.19  # Ensure minimum value is 0.19
thetan <- (thetan / rowSums(thetan)) 
#----------------------------------------
# Non-Hierarchical Priors #####
abgen <- sapply(colab, function(i) mean(study1[[i]]))
shape_org_gen<-matrix(nrow=1,ncol=2*5)
shape_aes_gen<-matrix(nrow=1,ncol=2*5)


for(i in initial_number_aes){
  start_col <- i
  cluster_cols <- ((match(i, initial_number_aes) - 1) * 2 + 1):(match(i, initial_number_aes) * 2)
  p <- study1[, i:(i+4)]
  p <- cbind(0.001, p)
  p[6]<-0.999
  for(k in 1:nrow(p)){
    if(min(p[k,2:5]) >= 0.4 && max(p[k,2:5]) <= 0.6){
      p[k,1]<- 0.3
      p[k,6]<- 0.7} 
    else if(min(p[k,2:5]) >= 0.4 && max(p[k,2:5]) > 0.6){
      p[k,1]<- 0.3} 
    else if(min(p[k,2:5]) < 0.4 && max(p[k,2:5]) <= 0.6){
      p[k,6]<- 0.7}
  }
  fit <- SHELF::fitdist(vals1, t(p), lower=0, upper=1)
  shape_aes_gen[,cluster_cols]<-as.numeric(fit$Beta)
}


alpha_aes_gen<- shape_aes_gen[,c(1,3,5,7,9)]
beta_aes_gen<- shape_aes_gen[,c(2,4,6,8,10)]

for(i in initial_number_org){
  start_col <- i
  cluster_cols <- ((match(i, initial_number_org) - 1) * 2 + 1):(match(i, initial_number_org) * 2)
  p <- study1[, i:(i+4)]
  p <- cbind(0.001, p)
  p[6]<-0.999
  for(k in 1:nrow(p)){
    if(min(p[k,2:5]) >= 0.4 && max(p[k,2:5]) <= 0.6){
      p[k,1]<- 0.3
      p[k,6]<- 0.7} 
    else if(min(p[k,2:5]) >= 0.4 && max(p[k,2:5]) > 0.6){
      p[k,1]<- 0.3} 
    else if(min(p[k,2:5]) < 0.4 && max(p[k,2:5]) <= 0.6){
      p[k,6]<- 0.7}
  }
  fit <- SHELF::fitdist(vals1, t(p), lower=0, upper=1)
  shape_org_gen[,cluster_cols]<-as.numeric(fit$Beta)
}

# Final Data ####
alpha_org_gen<- shape_org_gen[,c(1,3,5,7,9)]
beta_org_gen<- shape_org_gen[,c(2,4,6,8,10)]
alphax_gen<- alpha_aes_gen[2:5]
betax_gen<- beta_aes_gen[2:5]
alphay_gen<- alpha_org_gen[2:5]
betay_gen<- beta_org_gen[2:5]
rhok_gen<- sapply(1:4, function(i) mean(rhok[,i]))
rho1_gen<- median(rho1)
theta_gen<- sapply(1:4, function(i) mean(theta[,i]))


#----------------------------------------
#Data ####

I<- nrow(alpha_aes)
K<- 4
N<- 9


data_k_0 <- list(
  N = N,
  I = I,
  K = K,
  x = x,
  y = y,
  alphax = alphax,
  betax = betax,
  alphay = alphay,
  betay = betay,
  theta = (thetan*0.9/0.1),
  rho = rhok
)

data_1 <- list(
  N = N,
  I = I,
  K = K,
  x = x,
  y = y,
  alphax = alphax,
  betax = betax,
  alphay = alphay,
  betay = betay,
  theta = (thetan*0.9/0.1),
  rho = as.vector(rho1)
)



#Data General Prior ####



data_gen <- list(
  N = N,
  I = I,
  K = K,
  x = x,
  y = y,
  alphax = alphax_gen,
  betax = betax_gen,
  alphay = alphay_gen,
  betay = betay_gen,
  theta = (theta_gen*0.9/0.1),
  rho = rhok_gen
)


data_1_gen <- list(
  N = N,
  I = I,
  K = K,
  x = x,
  y = y,
  alphax = alphax_gen,
  betax = betax_gen,
  alphay = alphay_gen,
  betay = betay_gen,
  theta = (theta_gen*0.9/0.1),
  rho = as.vector(rho1_gen)
)















