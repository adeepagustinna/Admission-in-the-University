attach(adm_data)
Data <- adm_data

library(ggplot2)

###############################################################################
# Descriptive analysis
###############################################################################


###########################
#Training and  Testing data

## 75% of the sample size as training
smp_size <- floor(0.75 * nrow(Data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Data)), size = smp_size)

Train_Data <- Data[train_ind, ]
Test_Data <- Data[-train_ind, ]


##############################
# Plots
par(mfrow=c(2,2))
# Change line color and fill color
ggplot(Train_Data, aes(x=CGPA))+
  geom_histogram(color="darkblue", fill="lightblue")

# Change line color and fill color
ggplot(Train_Data, aes(x=GRE.Score))+
  geom_histogram(color="darkblue", fill="lightblue")

# Change line color and fill color
ggplot(Train_Data, aes(x=TOEFL.Score))+
  geom_histogram(color="darkblue", fill="lightblue")


###############################
# Numerical summary

#CGPA
summary(Train_Data$CGPA)
sd(Train_Data$CGPA)

#GRE Score
summary(Train_Data$GRE.Score)
sd(Train_Data$GRE.Score)

#TOEFL Score
summary(Train_Data$TOEFL.Score)
sd(Train_Data$TOEFL.Score)




#######################################
#Data Generative models, priors  and posteriors



#######
#CGPA
#######

# Consider data genarative model as normal with mean 8.5 and Variance 0.35

#Data-generating parameters
mu = 8.5
sigma_2 =0.35

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 2 and variance 1



#Hyperparameters for normal prior on mean 
mu0 = 2
tau0 = 1

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0*sigma_2) / (nsamp*tau0  + sigma_2)
sigman_2 = tau0* sigma_2/(nsamp * tau0  + sigma_2)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_CGPA <- tibble(x = Test_Data$CGPA, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_CGPA, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

CGPA_Test <- Test_Data$CGPA

# Count observations between bounds
count <- sum(CGPA_Test >= lowerbound & CGPA_Test <= upperbound)

print(count)



##############################################
#Prior Type 1

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_CGPA <- tibble(x = Test_Data$CGPA, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_CGPA, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

CGPA_Test <- Test_Data$CGPA

# Count observations between bounds
count <- sum(CGPA_Test >= lowerbound & CGPA_Test <= upperbound)

print(count)




#######
#GRE Score
#######

# Consider data genarative model as normal with mean 320 and variance 120

#Data-generating parameters
mu = 320
sigma_2 =120

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 250 and variance 100



#Hyperparameters for normal prior on mean 
mu0 = 250
tau0 = 100

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0) / (nsamp*tau0  + 1)
sigman_2 = tau0 /(nsamp * tau0  + 1)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_GRE <- tibble(x = Test_Data$GRE.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_GRE, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

GRE_Test <- Test_Data$GRE.Score

# Count observations between bounds
count <- sum(GRE_Test >= lowerbound & GRE_Test <= upperbound)

print(count)



##############################################
#Prior Type 2

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_GRE <- tibble(x = Test_Data$GRE.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_GRE, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

GRE_Test <- Test_Data$GRE.Score

# Count observations between bounds
count <- sum(GRE_Test >= lowerbound & GRE_Test <= upperbound)

print(count)





#######
#TOEFL Score
#######

# Consider data genarative model as normal with mean 320 and standard deviation 6

#Data-generating parameters
mu = 110
sigma_2 =36

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 100 and variance 25



#Hyperparameters for normal prior on mean 
mu0 = 100
tau0 = 25

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0) / (nsamp*tau0  + 1)
sigman_2 = tau0 /(nsamp * tau0  + 1)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_TOEFL <- tibble(x = Test_Data$TOEFL.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_TOEFL, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

TOEFL_Test <- Test_Data$TOEFL.Score

# Count observations between bounds
count <- sum(TOEFL_Test >= lowerbound & TOEFL_Test <= upperbound)

print(count)



##############################################
#Prior Type 2

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_TOEFL <- tibble(x = Test_Data$TOEFL.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_TOEFL, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

TOEFL_Test <- Test_Data$TOEFL.Score

# Count observations between bounds
count <- sum(TOEFL_Test >= lowerbound & TOEFL_Test <= upperbound)

print(count)


