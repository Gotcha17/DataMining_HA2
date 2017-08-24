# Part 1 ------------------------------------------------------------------
set.seed(1)
# Generating samples of size n = 100 of all random variables
u=c(rnorm(100, mean = 0, sd = 1))
x1=c(rnorm(100, mean = 0, sd = 1))
x2=c(rnorm(100, mean = 0, sd = 1))
# Calculating y with already generated r.v.
y=1+2*x1+u
linear_reg_data = data.frame(y, x1, x2)

# Part 2 ------------------------------------------------------------------

# Cross validation
# input "k" as numeric, "data" as dataframe and "regressor" as vector of dependent variable and regressors
# "regressor" can be left out or take the value "1" if only the intercept should be included in the model
CV_glm = function(k, data, regressors=1){
  data_range = length(data[,1])                            # Setting range of the whole sample
  k_fold_range = data_range/k                              # Setting the range of the k-folds
  data = data[sample(nrow(data)), ]                        # Shuffling data-set
  rownames(data) = NULL                                    # Disabling rownames
 
  # Regressing by leaving out the k-th fold out of the data
  sample = 1:data_range                               # Auxillary variable for indexing in the glm regression
  MSE = c()                                           # Initializing vector for storing result the MSEs
  for (l in 0:(k-1)){                                 # For-loop over all ks
    if(regressors == 1){                              # Case of regression with the intercept only
      glm.fit = glm(y ~ 1, data = data, subset = sample[-((l*k_fold_range+1):((l+1)*k_fold_range))])
    } 
    # Regression with intercept and at least one regressor besides the intercept
    if(regressors == 2){                              # Case of regression model of intercept+x1  
     glm.fit = glm(y ~ x1, data = data, subset = sample[-((l*k_fold_range+1):((l+1)*k_fold_range))])
    }
    if(regressors == 3){                              # Case of regression model of intercept+x2  
      glm.fit = glm(y ~ x2, data = data, subset = sample[-((l*k_fold_range+1):((l+1)*k_fold_range))])
    }
    if(regressors == 4){                              # Case of regression model of intercept+x1+x2  
      glm.fit = glm(y ~ x1+x2, data = data, subset = sample[-((l*k_fold_range+1):((l+1)*k_fold_range))])
    }
    # Predicting values for k-th fold values
    y_hat = predict(glm.fit, newdata = data[((l*k_fold_range+1):((l+1)*k_fold_range)),])
    # Calculating MSEs for k-th fold predicted values
    MSE[l+1] = mean((data[((l*k_fold_range+1):((l+1)*k_fold_range)),1]-y_hat)^2)
  }
  CV_MSE = mean(MSE)
  return(CV_MSE)
}
# Calcultaing MSEs for each model and K-fold, with k=5 and k=10
MSE=matrix(nrow = 4, ncol = 2, dimnames = list(c("1", "1,x1", "1,x2", "1,x1,x2"),c("K=5","K=10")))                        # Initializing matrix for MSEs for different models and ks
k_index=c(5,10)                                              # Setting parameters for k
for (j in 1:length(k_index)){                                # Outter for-loop over all ks
  for (i in 1:4){                                            # Inner for-loop over all regressor combinations for the model 
    # Calculation of the MSE matrix
    MSE[i,j]=CV_glm(k=k_index[j], data = linear_reg_data, regressors = i)
  }
}

# Display all combination of K and regressors included in the model to get an overview over the best performing models
MSE

# Testing our results with results with the cv.glm() function from the "boot" library
library(boot)
CV.glm.err=matrix(nrow = 4, ncol = 2, dimnames = list(c("1", "1,x1", "1,x2", "1,x1,x2"),c("K=5","K=10")))
for (j in 1:length(k_index)){
  glm.fit1 <- glm(y ~ 1, data = linear_reg_data)
  CV.glm.err[1,j] <- cv.glm(linear_reg_data, glm.fit1, K = k_index[j])$delta[1]
  glm.fit2 <- glm(y ~ 1 + x1, data = linear_reg_data)
  CV.glm.err[2,j] <- cv.glm(linear_reg_data, glm.fit2, K = k_index[j])$delta[1]
  glm.fit3 <- glm(y ~ 1 + x2, data = linear_reg_data)
  CV.glm.err[3,j] <- cv.glm(linear_reg_data, glm.fit3, K = k_index[j])$delta[1]
  glm.fit4 <- glm(y ~ 1 + x1 + x2, data = linear_reg_data)
  CV.glm.err[4,j] <- cv.glm(linear_reg_data, glm.fit4, K = k_index[j])$delta[1]
}
CV.glm.err
# Results are similar and therefore previously used procedure should be correct


# Models with included x1 variable perform much better than without. In general for best models with x1 included,
# fitting is somewhat better with K=5, but the difference in mean MSE is neglectibly small. For worse performing
# models the mean MSE is better with K=10.

# Part 3 ------------------------------------------------------------------

# Fitting the best models
best.model1 = glm(y ~ x1, data = linear_reg_data)     # Fitting first "best fit" model with intercept and x1
best.model2 = glm(y ~ x1+x2, data = linear_reg_data)  # Fitting first "best fit" model with intercept, x1 and x2

# Displaying model results
summary(best.model1)
summary(best.model2)

# First model with only the intercept and variable x1 is to be preffered, since in the second model x2
# is highly statistically insignificant. Since we are dealing with simulated data, there is also no
# benefit of taking x2 into the model because of more explanatory power of the model if the variable x2
# is included


# Part 4 ------------------------------------------------------------------

# First step to calculate average values for the estimators
n = 1000                                               # Setting n=1000 for 1000 repetitions
bs.estimate = matrix(nrow = n, ncol = 2)               # Initializing matrix for bs.estimates
for (i in 1:n){                                        # Outter for-loop over number of repetitions
  # Bootstrap sample is constructed from original data
  bs.sample = linear_reg_data[sample(1:100, replace = TRUE), ]
  rownames(bs.sample) = NULL                           # Disabling rownames
  glm.fit.bs = glm(y ~ x1, data = bs.sample)           # Regressing over the bootstrap sample for best model
  for (c in 1:length(coef(glm.fit.bs))){               # Inner for-loop over all coefficients
  bs.estimate[i,c] = (as.vector(coef(glm.fit.bs)))[c]  # Calculating and storing coefficient values
  }
}

# Second step to calculate average values for the estimators
bs.estimate.average = c()                              # Initializing vector to store the estimator averages
for (c in 1:length(coef(glm.fit.bs))){                 # For-loop over all coefficients
  bs.estimate.average[c] = mean(bs.estimate[ ,c])      # Calculating coefficient averages
}

# Calculation of the bootstrapped standard erros for the estimated coefficients
SE_hat_est = c()                                       # Initializing vector to store the standard errors
for (c in 1:length(coef(glm.fit.bs))){                 # For-loop over all coefficients
  # Calculation of the standard erros with formula (5.8) form the book "An Introduction to Statistical Learning"
  SE_hat_est[c] = sqrt(sum((bs.estimate[ ,c]-bs.estimate.average[c])^2)/(n-length(coef(glm.fit.bs))))
}

names(SE_hat_est) = c("Beta1.Err", "Beta2.Err")        # Setting names for values in the SE_hat_est vector

# Displaying the obtained results
SE_hat_est

# Testing our results with results with the boot() function from the "boot" library
coeff.est = function(data, index){
  
  return( coefficients(lm(formula(best.model1), 
                          data = data[index, ])) )
}

boot(linear_reg_data, coeff.est, 1000)
# We get similar standard errors and therefore our procedure should be correct


# Part 5 ------------------------------------------------------------------

# Displaying final results once again
print("Matrix of MSE values for each model with K=5 and K=10 folds")
MSE
print("Vector of bootstrapped standard errors for estimated cofficients")
SE_hat_est

# It is obviuos that a model that is generated with values of x1 our best fit models should naturally contain
# x1 as a regressor. 
# Standard errors of the coefficients and bootstrapped standard errors of coefficients are of the same
# magnitude and therefore it can be seen as a good procedure how to estimate standard errors of the coefficients.