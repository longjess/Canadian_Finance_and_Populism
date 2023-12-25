library(tidyverse)
library(haven)
library(nnet)
library(lmtest)
library(DescTools)
library(DOS2)
library(RItools)
library(estimatr)
library(ggplot2)

setwd("C:/Jessica/Stanford/Fall 2023/STATS 209/Project")

load("data_cleaned.RData")

# Outcome and treatment groups
table(data_cleaned$finance)
ggplot(data_cleaned, aes(x=finance)) +
  geom_bar(fill="light blue") + 
  theme_minimal()
ggplot(data_cleaned, aes(x=populism, fill=factor(finance))) +
  geom_histogram(alpha = 0.5)+
  theme_minimal()

# Covariate imbalance
df_zx <- data_cleaned%>%select(-populism)
balance <- xBalance(finance ~ .,  data=df_zx)
balance
plot(balance)

x <- as.data.frame(RItools:::prepareXbalForPlot(balance))
x <- rownames_to_column(x)
x <- gather(x, strata, values, -rowname)
# x<- mutate(values = abs(values)) # enable for `abs = TRUE`.
ggplot(x, aes(y = rowname, x = values, color = strata, shape = strata)) + 
  geom_vline(xintercept = 0) +
  geom_point() +
  xlab("Difference") +
  ylab("covariate") +
  theme(legend.position = "bottom", axis.text.y=element_blank())

# Estimate propensity with log reg
data_cleaned$propensity <- glm(finance ~ ., family=binomial,data=df_zx)$fitted.values
min_prop <- min(data_cleaned$propensity)
max_prop <- max(data_cleaned$propensity)
min_prop
max_prop
ggplot(data_cleaned, aes(x=propensity, fill=factor(finance))) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = min_prop) +
  annotate(x = min_prop, y = 1000, label = "min", geom = "label") +
  geom_vline(xintercept = max_prop) +
  annotate(x = max_prop - 0.05, y = 1000, label = "max", geom = "label") +
  labs(title="Propensity Distribution", x = "propensity") +
  theme_minimal()

estimate <- function(data_cleaned){
  # Estimate mu
  data_1 <- data_cleaned %>%
    dplyr::filter(finance == 1) %>%
    select(-finance, -propensity)
  data_0 <- data_cleaned %>%
    dplyr::filter(finance == 0) %>%
    select(-finance, -propensity)
  mu1 <- lm_robust(populism ~ ., data=data_1)
  mu1_hat <- predict(mu1, data_cleaned)
  mu0 <- lm_robust(populism ~ ., data=data_0)
  mu0_hat <- predict(mu0, data_cleaned)
  
  # Peng Ding pg 158
  # Regression outcome
  tau_rg <- mean(mu1_hat - mu0_hat)
  # Horvitz-Thompson estimator
  tau_ht = mean(z*y/pscore - (1 - z)*y/(1 - pscore))
  # Hajek estimator
  tau_hj = mean(z*y/pscore)/mean(z/pscore) -
    mean((1 - z)*y/(1 - pscore))/ mean((1 - z)/(1 - pscore))
  # Doubly robust
  res1 = y - mu1_hat
  z <- data_cleaned$finance
  y <- data_cleaned$populism
  pscore <- data_cleaned$propensity 
  res0 = y - mu0_hat
  tau_dr = mean(z*res1/pscore + mu1_hat - (1 - z)* res0/(1 - pscore) - mu0_hat)
  
  return(c(tau_rg, tau_ht, tau_hj, tau_dr))
}

bootstrap <- function(data_cleaned, n_boot = 1000)
{
  est <- estimate(data_cleaned)
  
  ## nonparametric bootstrap
  n_sample <- nrow(data_cleaned)
  boot_est <- replicate(n_boot,
                        {id_boot = sample(1:n_sample, n_sample, replace = TRUE)
                        estimate(data_cleaned %>% slice(id_boot))})
  
  boot_se <- apply(boot_est, 1, sd)
  boot_CI_lb <- est - 1.96*sqrt(boot_se)
  boot_CI_ub <- est + 1.96*sqrt(boot_se)
  res <- rbind(est, boot_se, boot_CI_lb, boot_CI_ub)
  rownames(res) <- c("est", "se", "CI_lb", "CI_ub")
  colnames(res) <- c("reg", "HT", " Hajek ", "DR")
  return(res)
}
results <- bootstrap(data_cleaned, 1000)
results
save(results, file="results.RData")

# Different covariates
# Demographic and socioeconomic characteristics only
df <- data_cleaned %>% select(-c(attention, follow_pol, religion_importance, vote))
results_2 <- bootstrap(df, 1000)
results_2
save(results_2, file="results_2.RData")
# Regional and cultural covariates
df <- data_cleaned %>% select(c(region, rural_urban, native_anglophone,
                                native_francophone, religion, religion_importance,
                                bornin_canada, parents_born, 
                                finance, propensity, populism))
results_3 <- bootstrap(df, 1000)
results_3
save(results_3, file="results_3.RData")


# Alternate def for financial situation
# 1 if situation got better
load("data_cleaned_2.RData")
table(data_cleaned_2$finance)
df_zx <- data_cleaned_2 %>% select(-populism)
data_cleaned_2$propensity <- glm(finance ~ ., family=binomial,data=df_zx)$fitted.values
min(data_cleaned_2$propensity)
max(data_cleaned_2$propensity)
results_4 <- bootstrap(data_cleaned_2, 1000)
results_4
save(results_4, file="results_4.RData")

# Test different measures for populism
populism_alt <- function(question, data_cleaned){
  print(question)
  data_cleaned <- data_cleaned %>% 
    mutate(populism = !!as.name(question)) %>%
    select(-c(pes21_populism_2, pes21_populism_3, 
              pes21_populism_4, pes21_populism_7, pes21_populism_8))
  df_zx <- data_cleaned %>% select(-populism)
  data_cleaned$propensity <- glm(finance ~ ., family=binomial,data=df_zx)$fitted.values
  print(bootstrap(data_cleaned, 1000))
}

load("data_cleaned_3.RData")
sapply(c("pes21_populism_2", "pes21_populism_3", 
         "pes21_populism_4", "pes21_populism_7", "pes21_populism_8"),
       populism_alt, data_cleaned_3)


