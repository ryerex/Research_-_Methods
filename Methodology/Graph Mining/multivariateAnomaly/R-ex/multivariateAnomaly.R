### Name: multivariateAnomaly
### Title: Anomaly detection in time-series measurements
### Aliases: multivariateAnomaly


### ** Examples

# X is the predictor, having 2 variables whose values have been measured from
# t1 to t5 . Y is the target variable.
data(KernelPredictorSample2)
data(KernelTargetSample2)
X = as.matrix(KernelPredictorSample2)
Y = as.matrix(KernelTargetSample2)
X
Y
# multivariateAnomaly function takes in input target and predictor matrices, 
# along with the number of eigen values and the spread width of the RBF kernel.
# Damping factor is an optional parameter.
RandomWalkOutput = multivariateAnomaly(X, Y, 0, 93.14, 5)
RandomWalkOutput



