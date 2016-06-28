rmsle = function(pred, actual){
    n = length(pred)
    sqrt(1/n * sum((log(pred + 1) - log(actual + 1))^2))
}