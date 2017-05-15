### Name: matrixAlign
### Title: Alignment of matrices.
### Aliases: matrixAlign


### ** Examples

data(KernelPredictorSample1)
data(KernelTargetSample1)
X = as.matrix(KernelPredictorSample1)
Y = as.matrix(KernelTargetSample1)
X
KX = RBFMeasure(X, 1.414)
KY = RBFMeasure(Y, 0.44)
K_Aligned = matrixAlign(KX, KY)
K_Aligned



