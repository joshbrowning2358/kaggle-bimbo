rmsle <- function(preds, actuals){
  n = length(preds)
  sqrt(1/n*sum((log(preds + 1) - log(actuals + 1))^2))
}

grad_rmsle <- function(preds, actuals){
  n = length(preds)
  rmsle_value = rmsle(preds, actuals)
  1/(2*rmsle_value) * 1/n * 2 * (log(preds + 1) - log(actuals + 1)) * 1/(preds + 1)
}

hess_rmsle <- function(preds, actuals){
  n = length(preds)
  rmsle_value = rmsle(preds, actuals)
  # rmsle_grad = grad_rmsle(preds, actuals)
  -1/(4*rmsle_value^3) * (1/n * 2 * (log(preds + 1) - log(actuals + 1)) * 1/(preds + 1))^2
    1/(2*rmsle_value) * 1/n * 2 * 1/(preds + 1) * 1/(preds + 1) +
    1/(2*rmsle_value) * 1/n * 2 * (log(preds + 1) - log(actuals + 1)) * (-1)/(preds + 1)^2
}




# Test
preds = 1:10
actuals = preds + 0.1*rnorm(10)
rmsle(preds, actuals)
grad_rmsle(preds, actuals)
numDeriv::grad(function(preds){rmsle(preds, actuals)}, x=preds)
hess_rmsle(preds, actuals)
diag(numDeriv::hessian(function(preds){rmsle(preds, actuals)}, x=preds))
