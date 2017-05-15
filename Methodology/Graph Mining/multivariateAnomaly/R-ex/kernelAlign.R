### Name: kernelAlign
### Title: KernelAlignment of time-series measurements
### Aliases: kernelAlign KAlign


### ** Examples

#X is the predictor, having 2 variables whose values have been measured from
#t1 to t5 . Y is the target variable.
data(KernelPredictorSample1)
data(KernelTargetSample1)
X = as.matrix(KernelPredictorSample1)
Y = as.matrix(KernelTargetSample1)
X
Y
#kernelAlign function takes in the target and predictor as input
#along with number of eigen values, the standard deviation
#damping factor is an optional parameter
RandomWalkOutput = kernelAlign(X, Y, nv = 2)
RandomWalkOutput



