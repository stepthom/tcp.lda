\name{tcp.lda-package}
\alias{tcp.lda-package}
\alias{tcp.lda}
\docType{package}
\title{
Test case prioritization with LDA
}
\description{
This package uses latent Dirichlet allocation (LDA) to prioritize
a set of test cases. This package is an implementation of the paper "Static Test
Case Prioritization Using Topic Models" by Stephen W. Thomas et al. in Empirical
Software Engineering, 2012.
}
\details{
\tabular{ll}{
Package: \tab tcp.lda\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-11-23\cr
License: \tab GNU General Public License?\cr
LazyLoad: \tab yes\cr
}
}
\author{
Stephen W. Thomas

Maintainer: Stephen W. Thomas <sthomas@cs.queensu.ca>
}
\references{
[1] Stephen W. Thomas, Hadi Hemmati, Ahmed E. Hassan, and Dorothea Blostein.
Static Test Case Prioritization Using Topic Models.
Empirical Software Engineering, 2012.
}
\keyword{ test case prioritization, lda, latent dirichlet allocation, tcp }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
## NOT RUN
# Set up paths to the input data
testDir          <- "data/runs/derby/v1/preA.pruned0180"
testNamesMap     <- "data/runs/derby/v1/test_names.dat"
truthName        <- "data/runs/derby/v1/test_matrix.dat"

# Call the main function
l1 <- tcp.lda(testDir=testDir, 
            testNamesMap=testNamesMap, truthName=truthName,
            K=40)

# Other functions compute other TCP techniques:
# tcp.random(...)
# tcp.static(...)
# tcp.string(...)

}
