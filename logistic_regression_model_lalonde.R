library(Matching)
library(arm)
# import lalonde dataset 
data("lalonde") 
# train model 
glm_lalonde = glm(treat~age+educ+hisp+re74+re75, family = "binomial", data = lalonde) 

get_pred_02 = function(coefs, person) {
  pred = coefs[1] + 
    person[1]*coefs[2] +
    person[2]*coefs[3] +
    person[3]*coefs[4] + 
    person[4]*coefs[5] +
    person[5]*coefs[6] 
  
  return(exp(pred) / (1 + exp(pred)))
}

# empty matrix for expected values
expected_value_matrix_02 = matrix(NA, nrow = 20, ncol = 14) 
for (j in 1:20){
  # simulate 500 models like lm_lalonde
  sim_lalonde_02 = sim(glm_lalonde, n.sims = 500) 
  # storage matrix for predicted values
  storage_matrix_lalonde_02 = matrix(NA, nrow = 500, ncol = 14) 
  
  for (educ in c(3:16)){
    for (i in 1:nrow(sim_lalonde_02@coef)){
      # set all predictors to their means except age
      avg_person = c(mean(lalonde$age), educ,
                     mean(lalonde$hisp),
                     mean(lalonde$re74), 
                     mean(lalonde$re75)) 
      # update matrix
      storage_matrix_lalonde_02[i, educ-2] = get_pred_02(sim_lalonde_02@coef[i, ], avg_person) 
    }
  }
  # take the mean of matrix column-wise
  mean_educ_02 <- apply(storage_matrix_lalonde_02, 2, mean) 
  # update expected value matrix
  expected_value_matrix_02[j, ] = mean_educ_02 
}
# find confidence interval of expected values
conf_int_exp_educ_02 <- apply(expected_value_matrix_02, 2, quantile, probs = c(0.025, 0.975)) 

# plot canvas for figure
plot(x = c(0:20), y = c(0:20), type = "n", xlim = c(3,16), ylim = c(0,1),
     main = "Expected Probability to get Treatment by Education Level", 
     xlab = "Education Level of Respondent (Years)", 
     ylab = "Probabilty of Receiving Treatment") 

# plot confidence intervals for all ages
for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = conf_int_exp_educ_02[1, educ - 2],
    x1 = educ,
    y1 = conf_int_exp_educ_02[2, educ - 2],
    lwd = 2)
}

# simulate 500 models like lm_lalonde
sim_lalonde_02 = sim(glm_lalonde, n.sims = 500) 
# storage matrix for predicted values  
storage_matrix_lalonde_02 = matrix(NA, nrow = 500, ncol = 14) 

for (educ in c(3:16)){
  for (i in 1:nrow(sim_lalonde_02@coef)){
    # set all predictors to their means except age
    avg_person = c(mean(lalonde$age), educ,
                   mean(lalonde$hisp),
                   mean(lalonde$re74), 
                   mean(lalonde$re75)) 
    # update matrix
    storage_matrix_lalonde_02[i, educ-2] = get_pred_02(sim_lalonde_02@coef[i, ], avg_person) 
  }
}
conf_int_educ_02 <- apply(storage_matrix_lalonde_02, 2, quantile, probs = c(0.025, 0.975))

# plot canvas for figure
plot(x = c(0:20), y = c(0:20), type = "n", xlim = c(3,16), ylim = c(0, 1),  
     main = "Predicted Probability to get Treatment by Education Level", 
     xlab = "Education Level of Respondent (Years)", 
     ylab = "Probabilty of Receiving Treatment")

# plot confidence intervals for all ages
for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = conf_int_educ_02[1, educ - 2],
    x1 = educ,
    y1 = conf_int_educ_02[2, educ - 2],
    lwd = 2)
}


