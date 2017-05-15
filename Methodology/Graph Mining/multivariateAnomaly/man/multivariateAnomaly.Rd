% --- Source file: RBFMeasure.Rd ---
\name{RBFMeasure}
\Rdversion{1.1}
\alias{RBFMeasure}
\title{
Calculate cosine similarity distance for a give timeseries measurement.
}
\description{
Given a set of measurements and their timestamps, this function outputs a symmetric matrix that capture the pair wise similarity between every pair of timestamps.
}
\usage{
RBFMeasure(X, sig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
X is a matrix, consisting of $p$ real valued measurements in each column (timestamp).
}
  \item{sig}{
sigma, the spread width of the RBF kernel.
}
}
\value{
A non-negative symmetric matrix is returned.
}
\references{
Haibin Cheng, Pang-Ning Tan, Christopher Potter, Steven Klooster, "A Robust Graph-Based Algorithm for Detection and Characterization of Anomalies in Noisy Multivariate Time Series," icdmw, pp.349-358, 2008 IEEE International Conference on Data Mining Workshops, 2008
}
\author{
Sriram Lakshminarasimhan	
Siddarth Ramaswamy 
}

\seealso{
Other random walk algorithms 'outlierA', 'outlierB'
}
\examples{
data(KernelData)
TimeSeries = as.matrix(KernelData)

TimeSeries
RBFMeasure(TimeSeries, 1.414)
}
\concept{ RBFMeasure }
\concept{ Similarity }
\concept{ multivariateAnomaly }
\eof
% --- Source file: kernelAlign.Rd ---
\name{kernelAlign}
\Rdversion{1.1}
\alias{kernelAlign}
\alias{KAlign}
\title{
KernelAlignment of time-series measurements
}
\description{
Performs KernelAlignment algorithm described in the referenced paper, for a set of multivariate time series predictor values and a set of target values.
}
\usage{
kernelAlign(X, Y, r = 0, sig = 0.1, nv = 1)
}
\arguments{
  \item{X}{
The predictor time series variables, each row is a variable and each column is a timestamp.
}
  \item{Y}{
The target time series.
}
  \item{r}{
The autoregression lag.
}
  \item{sig}{
The spread width of the RBF kernel.
}
  \item{nv}{
Number of eigenvectors to be used.
}
}
\references{
	[1] Haibin Cheng, Pang-Ning Tan, Christopher Potter, and Steven Klooster. 
	A Robust Graph-based Algorithm for Detection and Characterization of Anomalies in Noisy
	Multivariate Time Series, to appear in Proc.of IEEE International Conference on Data Mining
	workshop on Spatial and Spatio-temporal Data Mining (ICDM/STDM 08)
}
\author{
Sriram Lakshminarasimhan
Siddarth Ramaswamy
}

\examples{
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
}
\concept{ anomaly }
\concept{ timeseries }
\concept{ multivariateAnomaly }
\eof
% --- Source file: matrixAlign.Rd ---
\name{matrixAlign}
\Rdversion{1.1}
\alias{matrixAlign}
\title{
Alignment of matrices.
}
\description{
The alignMatrix, takes in two kernel similarity matrices as input and aligns them i.e. it maximizes the correlation between the two matrices.
}
\usage{
matrixAlign(KX, KY)
}
\arguments{
  \item{X}{
Kernel alignment of the predictor time series variables.
}
  \item{Y}{
Kernel alignment of the target variables.
}
}
\references{
	[1] Haibin Cheng, Pang-Ning Tan, Christopher Potter, and Steven Klooster. 
	A Robust Graph-based Algorithm for Detection and Characterization of Anomalies in Noisy
	Multivariate Time Series, to appear in Proc.of IEEE International Conference on Data Mining
	workshop on Spatial and Spatio-temporal Data Mining (ICDM/STDM 08)
}
\author{
Sriram Lakshminarasimhan
}

\examples{
data(KernelPredictorSample1)
data(KernelTargetSample1)
X = as.matrix(KernelPredictorSample1)
Y = as.matrix(KernelTargetSample1)
X
KX = RBFMeasure(X, 1.414)
KY = RBFMeasure(Y, 0.44)
K_Aligned = matrixAlign(KX, KY)
K_Aligned
}
\concept{ anomaly }
\concept{ timeseries }
\concept{ multivariateAnomaly }
\eof
% --- Source file: multivariateAnomaly.Rd ---
\name{multivariateAnomaly}
\Rdversion{1.1}
\alias{multivariateAnomaly}
\title{
Anomaly detection in time-series measurements
}
\description{
Performs anomaly detection using algorithm descriped in the referenced paper, for a set of multivariate time series predictor values and a set of target values.
}
\usage{
multivariateAnomaly(X, Y, r, sig, nv, d = 0.1)
}
\arguments{
  \item{X}{
The predictor time series variables, each row is a variable and each column is a timestamp.
}
  \item{Y}{
The target time series.
}
  \item{r}{
The autoregression lag.
}
  \item{sig}{
The spread width of the RBF kernel.
}
  \item{nv}{
Number of eigenvectors to be used.
}
  \item{d}{
The damping factor to be passed to the anomaly detection function. Default value is 0.1
}
}
\value{
Intermediate outputs produced in this step passed to anomaly detection function

\item{w}{ 
The weight associated with each eigenvector.
}
\item{KX}{ 
The new aligned kernel matrix for X.
}
\item{KY}{ 
The kernel matrix for target Y.
}
}
\references{
	[1] Haibin Cheng, Pang-Ning Tan, Christopher Potter, and Steven Klooster. 
	A Robust Graph-based Algorithm for Detection and Characterization of Anomalies in Noisy
	Multivariate Time Series, to appear in Proc.of IEEE International Conference on Data Mining
	workshop on Spatial and Spatio-temporal Data Mining (ICDM/STDM 08)
}
\author{
Sriram Lakshminarasimhan
Siddarth Ramaswamy
}

\examples{
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
}
\concept{ anomaly }
\concept{ timeseries }
\concept{ multivariateAnomaly }
\eof
% --- Source file: repmat.Rd ---
\name{repmat}
\Rdversion{1.1}
\alias{repmat}
\alias{RepeatMatrix}
\title{
Replicate matrix.
}
\description{
Analogous to the repmat function in matlab. A = repmat(B, m, n) creates a new matrix A having m * n tiling of B.
The size of the matrix A will be [nrow(B) * m, ncol(B) * n]
}
\usage{
repmat(B, m, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{
The input matrix that has to be replicated.
}
  \item{m}{
Number of tilings along the row.
}
  \item{n}{
Number of tilings along the column.
}
}
\author{
Sriram Lakshminarasimhan
}

\examples{
B <- matrix(c(1:9), nrow=3)
B
A <- repmat(B, 2, 3)
A
}
\concept{ matrix }
\concept{ anomaly}
\concept{ multivariate }
\concept{ replicate }
\concept{ multivariateAnomaly }
\eof
