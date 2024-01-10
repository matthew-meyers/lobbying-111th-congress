lobby_bills_df <- readRDS("data.rds")



#Create dataframe with only the lobbying predictors
lobby_predictors <- lobby_bills_df %>% 
  dplyr::select(Code_appearances, max_type, actions.count, policyArea, subjects.count, cosponsors.count, introducedDate, relatedBills.count, sponsors.party, first_committee, committees.count, Chamber) %>% 
  mutate(actions.count = log(actions.count + 1), subjects.count = log(subjects.count + 1), cosponsors.count = log(cosponsors.count + 1), relatedBills.count = log(relatedBills.count + 1))



#Create empty data frame for bivariate relationships comparison
comparison_table <- data.frame(predictor = NULL, coef_lin = NULL, coef_nb = NULL, r2 = NULL, r2.adj = NULL, AIC_lin = NULL, p_val_lin = NULL, AIC_nb = NULL, p_val_nb = NULL)
#store indices of categorical predictors for later
cat_index <- NULL

#Find bivariate relationships for each predictor.
for(i in 2:ncol(lobby_predictors)) {
  lm <- lm(log(lobby_predictors$Code_appearances) ~ lobby_predictors[,i])
  lmsum <- summary(lm)
  
  nbm <- glm.nb(lobby_predictors$Code_appearances ~ lobby_predictors[,i])
  nbsum <- summary(nbm)
  
  ##Linear stats
  r2 <- lmsum$r.squared
  r2.adj <- lmsum$adj.r.squared
  AIC_lm <- AIC(lm)
  f <- lmsum$fstatistic
  p_val_lm <- pf(f[1], f[2], f[3], lower.tail = F)
  
  
  ##Neg Bin stats
  AIC_nb <- AIC(nbm)
  dev_diff <- nbsum$null - nbsum$deviance
  df_diff <- nbsum$df.null - nbsum$df.residual
  p_val_nb <- pchisq(dev_diff, df = df_diff, lower.tail = F)
  
  ##Coefficients
  if(is.numeric(lobby_predictors[,i]) | length(unique(lobby_predictors[,i])) <= 2 | names(lobby_predictors)[i] == "introducedDate") {
    lm_coef <- as.character(round(lmsum$coefficients[2, 1], 3))
    nb_coef <- as.character(round(nbsum$coefficients[2, 1], 3))
  } else {
    #For these variables there is more than one predictor so they need to be shown in a separate table
    lm_coef <- "See Appendix B"
    nb_coef <- "See Appendix B"
    cat_index <- c(cat_index, i)
  }
  #store results
  comparison_table <- rbind(comparison_table, data.frame(predictor = names(lobby_predictors)[i], coef_lin = lm_coef, coef_nb = nb_coef, r2 = r2, r2.adj = r2.adj, AIC_lm = AIC_lm, p_val_lm = p_val_lm, AIC_nb = AIC_nb, p_val_nb = p_val_nb))
}

comparison_table <- comparison_table %>% 
  arrange(AIC_nb)

#Write resuls so that we can reference in final report
write.csv(comparison_table, "bivariate_lobbying_table.csv")

#Get the results of the bivariate relationships for the categorical predictors
for(i in cat_index) {
  lm <- lm(log(lobby_predictors$Code_appearances) ~ lobby_predictors[,i])
  lmsum <- summary(lm)
  lm_coefs <- data.frame(lmsum$coefficients)
  
  lm_coefs <- lm_coefs %>% 
    tibble::rownames_to_column(var = "Predictor") %>% 
    mutate(Predictor = gsub(".*[,] i", "", Predictor)) %>% 
    mutate(Predictor = gsub("]", "", Predictor)) %>% 
    mutate(Predictor = gsub("[(]", "", Predictor)) %>% 
    mutate(Predictor = gsub("[)]", "", Predictor))
  
  lin_name <- paste0("Linear Bivariate Predictors ", names(lobby_predictors)[i], ".csv")
  write.csv(lm_coefs, lin_name)
  
  
  nbm <- glm.nb(lobby_predictors$Code_appearances ~ lobby_predictors[,i])
  nbsum <- summary(nbm)
  
  nb_coefs <- data.frame(nbsum$coefficients)
  nb_coefs <- nb_coefs %>% 
    tibble::rownames_to_column(var = "Predictor") %>% 
    mutate(Predictor = gsub(".*[,] i", "", Predictor)) %>% 
    mutate(Predictor = gsub("]", "", Predictor)) %>% 
    mutate(Predictor = gsub("[(]", "", Predictor)) %>% 
    mutate(Predictor = gsub("[)]", "", Predictor))
  
  nb_name <- paste0("NB Bivariate Coefficients ", names(lobby_predictors)[i], ".csv")
  write.csv(nb_coefs, nb_name)
}


#Chamber won't be part of a multivariate model because it is just a less detailed version of Committee
lobby_predictors <- lobby_predictors %>% 
  dplyr::select(-Chamber)



library(leaps)
#Lasso setup
X <- model.matrix(log(Code_appearances) ~ . -1, data = lobby_predictors)
X_norm <- scale(X)

#create response variables
y <- na.omit(lobby_predictors)$Code_appearances
y_log <- log(y)

library(glmnet)

#Lasso Linear
lasso_norm <- cv.glmnet(X_norm, y_log, alpha = 1)

lasso_norm_coefs_min <- coef(lasso_norm, s = lasso_norm$lambda.min)
lasso_norm_coefs_1se <- coef(lasso_norm, s = lasso_norm$lambda.1se)

#Check which variables are 0
varkeep <- rownames(lasso_norm_coefs_1se)[lasso_norm_coefs_1se[,1] != 0]
var_zero <- rownames(lasso_norm_coefs_1se)[lasso_norm_coefs_1se[,1] == 0]
print(var_zero)

#Create a new design matrix without the 0 variables
X_sparse <- X[,colnames(X) %in% varkeep]

#Run new model
screen_clean_m <- lm(y_log ~ X_sparse)
summary(screen_clean_m)

#Prepare linear coefficients for storage and store them for the final report
lin_coefs <- data.frame(summary(screen_clean_m)$coefficients)

lin_coefs <- lin_coefs %>% 
  tibble::rownames_to_column("variable") %>% 
  mutate(variable = gsub("X[_]sparse", "", variable))

write.csv(lin_coefs, "log_lin_coefs.csv")


#Lasso Negative Binomial
library(mpath)

lasso_nb <- glmregNB(y ~ X_norm, alpha = 1, n.cores = 5)



#Figure out which lasso index is the best
min_AIC <- which(lasso_nb$aic == min(lasso_nb$aic))
min_BIC <- which(lasso_nb$bic == min(lasso_nb$bic))

#choose more restrictive between AIC recommendation and BIC recommendation
model_num <- min(min_AIC, min_BIC)


#Figure out which coefficients were 0 and which had nonzero values
coefs_nb <- lasso_nb$beta[,model_num]

varkeep_nb <- names(coefs_nb)[coefs_nb != 0]
var_zero_nb <- names(coefs_nb)[coefs_nb == 0]
varkeep_nb <- gsub("X_norm", "", varkeep_nb)
var_zero_nb <- gsub("X_norm", "", var_zero_nb)


print(var_zero_nb)

X_sparse_nb <- X[,varkeep_nb]


#run new model on the restricted set of predictors
screen_clean_nb <- glm.nb(y ~ X_sparse_nb)
summary(screen_clean_nb)

#Store results of model for the final report
nb_coefs <- data.frame(summary(screen_clean_nb)$coefficients)

nb_coefs <- nb_coefs %>% 
  tibble::rownames_to_column("variable") %>% 
  mutate(variable = gsub("X[_]sparse[_]nb", "", variable))

write.csv(nb_coefs, "nb_coefs.csv")


##Cross Validating

#set up the folds
set.seed(100)
k <- 5

n <- nrow(lobby_predictors)

folds <- rep(1:k, n/k)
folds <- sample(folds)

lin_sparse_data <- data.frame(cbind(y_log, X_sparse))
nb_sparse_data <- data.frame(cbind(y, X_sparse_nb))


lin_sparse_data$fold <- folds
nb_sparse_data$fold <- folds

lin_sparse_data$prediction <- NA
nb_sparse_data$prediction <- NA

for(i in 1:k) {
  #separate training and test data
  training_lin <- lin_sparse_data %>% 
    filter(fold != i) %>% 
    dplyr::select(-prediction)
  test_lin <- lin_sparse_data %>% 
    filter(fold == i) %>%
    dplyr::select(-prediction)
  
  training_nb <- nb_sparse_data %>% 
    filter(fold != i) %>% 
    dplyr::select(-prediction)
  test_nb <- nb_sparse_data %>% 
    filter(fold == i) %>% 
    dplyr::select(-prediction)
  
  lm <- lm(y_log ~., data = training_lin)
  nb <- glm.nb(y ~ ., data = training_nb)
  
  #Get predictions for test data in this fold
  lin_preds <- predict(lm, newdata = test_lin)
  #exponentiate linear predictions
  lin_preds <- exp(lin_preds)
  nb_preds <- predict(nb, newdata = test_nb, type = "response")
  
  lin_sparse_data$prediction[lin_sparse_data$fold == i] <- lin_preds
  nb_sparse_data$prediction[nb_sparse_data$fold == i] <- nb_preds
}

#find and print both RMSE
lm_rmse <- sqrt(mean((y - lin_sparse_data$prediction)^2))
nb_rmse <- sqrt(mean((y - nb_sparse_data$prediction)^2))
lm_rmse
nb_rmse



#Create Fits vs residuals plot of log linear model to check validitiy of assumptions
library(ggplot2)
library(pubtheme)
g <- ggplot(data = NULL, aes(x = screen_clean_m$fitted.values, y = screen_clean_m$residuals)) +
  geom_point(color = "#429698") +
  labs(x = "Fitted Values", y = "Residuals", title = "Log Linear Model Fitted Values vs Residuals") +
  theme_pub(type = "scatter")
ggsave("Fit vs resid.jpeg", g)




# Create modified version of data that is used for ordinal modelling with a simplified max_type variable
lobby_bills_multinom <- lobby_bills_df %>% 
  mutate(max_type = as.character(max_type)) %>% 
  mutate(max_type = ifelse(max_type == "DischargePetition" | max_type == "CalendarEntry", 
                           "PassedCommittee", max_type)) %>% 
  mutate(max_type = ifelse(max_type == "Veto" | max_type == "BecameLaw", "PassedCongress", max_type)) %>% 
  mutate(max_type = ifelse(max_type == "ResolvingDifferences", "FloorConsideration", max_type)) %>% 
  mutate(max_type = factor(max_type, levels = c("Introduced", "CommitteeConsideration", "PassedCommittee", "FloorConsideration", "PassedCongress")))

#Subset for the desired predictors. Don't include actions count because it is basically a proxy for how far it gets
stage_predictors <- lobby_bills_multinom %>% 
  dplyr::select(max_type, Code_appearances, policyArea, subjects.count, cosponsors.count, introducedDate, relatedBills.count, sponsors.party, first_committee, committees.count, Chamber) %>% 
  mutate(subjects.count = log(subjects.count + 1)) %>% 
  mutate(cospsonsors.count = log(cosponsors.count + 1)) %>% 
  mutate(relatedBills.count = log(relatedBills.count + 1)) %>% 
  mutate(Code_appearances = log(Code_appearances)) %>% 
  dplyr::rename(Lobbying = Code_appearances) %>% 
  mutate(introducedDate = scale(introducedDate)[,1])

#Create empty data table to store results of bivariate analysis
comparison_table_stage <- data.frame(predictor = NULL, coefficient = NULL, se_coef = NULL, t_coef = NULL, p_coef = NULL, AIC = NULL)
#Store categorical variables indices
cat_index_ordinal <- NULL

#run through each variable for bivariate analysis
for(i in 2:ncol(stage_predictors)) {
  model <- MASS::polr(stage_predictors$max_type ~ stage_predictors[,i], Hess = T)
  modsum <- summary(model)
  AIC <- AIC(model)
  
  #get coefficients of non-categorical or categorical with two variables
  if(is.numeric(stage_predictors[,i]) | length(unique(stage_predictors[,i])) <= 2 | names(stage_predictors)[i] == "introducedDate") {
    coef <- as.character(round(modsum$coefficients[1, 1], 3))
    se_coef <- as.character(round(modsum$coefficients[1, 2], 3))
    t_coef <- modsum$coefficients[1, 3]
    tail <- t_coef < 0
    p_coef <- 2*pt(t_coef, df = 7903, lower.tail = tail)
    t_coef <- as.character(round(t_coef, 3))
    p_coef <- as.character(p_coef)
  } else {
    #save many category variables for another table
    coef <- "See Appendix E"
    se_coef <- "See Appendix E"
    t_coef <- "See Appendix E"
    p_coef <- "See Appendix E"
    cat_index_ordinal <- c(cat_index_ordinal, i)
  }
  
  comparison_table_stage <- rbind(comparison_table_stage, data.frame(predictor = names(stage_predictors)[i], coefficient = coef, se_coef = se_coef, t_coef = t_coef, p_coef = p_coef, AIC = AIC))
}

#Store results for final report
write.csv(comparison_table_stage, "ordinal_bivariate.csv")


#Get and store coefficients of categorical variables
for(i in cat_index_ordinal) {
  subset_df <- data.frame(max_type = stage_predictors$max_type, predictor = stage_predictors[,i])
  model <- MASS::polr(max_type ~ predictor, data = subset_df, Hess = T)
  modsum <- summary(model)
  
  coefs <- data.frame(modsum$coefficients)
  
  df <- nrow(subset_df) - 1 - length(unique(subset_df$predictor))
  
  coefs <- coefs %>% 
    tibble::rownames_to_column(var = "Predictor") %>% 
    mutate(Predictor = gsub("predictor", "", Predictor)) %>% 
    mutate(p.value = pt(t.value, df = df, lower.tail = (t.value < 0)))
  
  file_name <- paste0("Ordinal Bivariate Coefs ", names(stage_predictors)[i], ".csv")
  
  write.csv(coefs, file_name)
}


#Try with just the quantitative variables (didn't end up using this in final report)
olr_quant_model <- MASS::polr(max_type ~ Lobbying + subjects.count + cosponsors.count + introducedDate + relatedBills.count + committees.count,
                              data = stage_predictors,
                              Hess= T)

summary(olr_quant_model)

#Try with all the variables (didn't end up using this in final report)
olr_full_model <- MASS::polr(max_type ~ Lobbying + subjects.count + cosponsors.count + introducedDate + relatedBills.count + committees.count + policyArea + first_committee + sponsors.party,
                             data = stage_predictors,
                             Hess= T)

summary(olr_full_model)




#Prepare the LASSO matrices
stage_predictors <- stage_predictors %>% 
  dplyr::select(-Chamber) #chamber is not necessary when committees are in the model

#Create design matrices
X_stage <- model.matrix(max_type ~ . -1, data = stage_predictors)
X_stage_norm <- scale(X_stage)


y_stage <- na.omit(stage_predictors)$max_type

#Run the LASSO fit
library(ordinalNet)
ordinal_LASSO <- ordinalNetCV(X_stage_norm, y_stage, alpha = 1)

#it returns multiple best lambdas, choose the highest of them
lambda_index <- min(ordinal_LASSO$bestLambdaIndex)

#Find which coefficients went to zero
coefs <- ordinal_LASSO$fit$coefs[lambda_index,]

varkeep_ord <- names(coefs)[coefs != 0]
varzero_ord <- names(coefs)[coefs == 0]


#Refit model without variables that went to zero
X_stage_sparse <- X_stage[,colnames(X_stage) %in% varkeep_ord]

sparse_df <- data.frame(y_stage, X_stage_sparse)
sparse_model <- MASS::polr(y_stage ~ ., data = sparse_df, Hess = T)

ordinal_coefs_preclean <- summary(sparse_model)$coefficients

#There are some NA values in the Standard error which can be solved by rescaling the variables my mean and standard deviation
sparse_df$policyAreaAnimals <- scale(sparse_df$policyAreaAnimals)[,1]
sparse_df$introducedDate <- scale(sparse_df$introducedDate)[,1]
sparse_df$policyAreaLaw <- scale(sparse_df$policyAreaLaw)[,1]

#Refit model after scaling variables
model_scaled <- MASS::polr(y_stage ~ ., data = sparse_df, Hess = T)
ordinal_coefs <- summary(model_scaled)$coefficients

#Find Likelihood Ratio P-values with type 3 ANOVA (this is the recommended way to do this according to https://www.bookdown.org/rwnahhas/RMPH/blr-ordinal.html#blr-olr-adjusted)
anova_sparse <- car::Anova(model_scaled, type = 3)
LR_pval <- anova_sparse$`Pr(>Chisq)`
LR_pval <- c(LR_pval, rep(NA, nrow(ordinal_coefs) - length(LR_pval)))

ordinal_coefs <- data.frame(ordinal_coefs) %>% 
  mutate(p.val = LR_pval) %>% 
  tibble::rownames_to_column()

#ANOVA doesn't find coefficients for the intercepts so do this the traditional way
ordinal_coefs <- ordinal_coefs %>% 
  mutate(p.val = ifelse(is.na(p.val), pt(t.value, df = 7903, lower.tail = (t.value < 0)), p.val))

#Store results for final report
write.csv(ordinal_coefs, "ordinal model coefs.csv")



#Find out of sample classification error and log loss
set.seed(120)
k <- 5

n <- nrow(sparse_df)

folds <- rep(1:k, n/k)
folds <- sample(folds)



sparse_df$fold <- folds

sparse_df$Introduced_pred <- NA
sparse_df$CommitteeConsideration_pred <- NA
sparse_df$PassedCommittee_pred <- NA
sparse_df$FloorConsideration_pred <- NA
sparse_df$PassedCongress_pred <- NA

for(i in 1:k) {
  #separate training and test data
  training <- sparse_df %>% 
    filter(fold != i) %>% 
    dplyr::select(-c(Introduced_pred, 
                     CommitteeConsideration_pred, 
                     PassedCommittee_pred, 
                     FloorConsideration_pred, 
                     PassedCongress_pred,
                     fold))
  
  test <- sparse_df %>% 
    filter(fold == i) %>%
    dplyr::select(-c(Introduced_pred, 
                     CommitteeConsideration_pred, 
                     PassedCommittee_pred, 
                     FloorConsideration_pred, 
                     PassedCongress_pred,
                     fold))
  
  mod <- MASS::polr(y_stage ~ ., data = training, Hess = T)
  
  #Get predictions for test data in this fold
  preds <- predict(mod, newdata = test, type = "p")
  
  sparse_df$Introduced_pred[sparse_df$fold == i] <- preds[,1]
  sparse_df$CommitteeConsideration_pred[sparse_df$fold == i] <- preds[,2]
  sparse_df$PassedCommittee_pred[sparse_df$fold == i] <- preds[,3]
  sparse_df$FloorConsideration_pred[sparse_df$fold == i] <- preds[,4]
  sparse_df$PassedCongress_pred[sparse_df$fold == i] <- preds[,5]

}

#Find how it classified
max_prob = apply(cbind(sparse_df$Introduced_pred, sparse_df$CommitteeConsideration_pred, sparse_df$PassedCommittee_pred, sparse_df$FloorConsideration_pred, sparse_df$PassedCongress_pred), 1, max)

sparse_df <- sparse_df %>% 
  mutate(max_pred = max_prob) %>% 
  mutate(predict_state = case_when(max_pred == Introduced_pred ~ "Introduced",
                                   max_pred == CommitteeConsideration_pred ~ "CommitteeConsideration",
                                   max_pred == PassedCommittee_pred ~ "PassedCommittee",
                                   max_pred == FloorConsideration_pred ~ "FloorConsideration",
                                   max_pred == PassedCongress_pred ~ "PassedCongress"))

#Find accuracy
mean(sparse_df$y_stage == sparse_df$predict_state)

#See where it typically went wrong
table(sparse_df$y_stage, sparse_df$y_stage == sparse_df$predict_state)
preds_table <- table(sparse_df$y_stage, sparse_df$predict_state)

write.csv(preds_table, "ordinal predictions.csv")

#calculate log loss
sparse_df <- sparse_df %>% 
  mutate(prob_correct = case_when(y_stage == "Introduced" ~ Introduced_pred,
                                  y_stage == "CommitteeConsideration" ~ CommitteeConsideration_pred,
                                  y_stage == "PassedCommittee" ~ PassedCommittee_pred,
                                  y_stage == "FloorConsideration" ~ FloorConsideration_pred,
                                  y_stage == "PassedCongress" ~ PassedCongress_pred))


-mean(log(sparse_df$prob_correct))
