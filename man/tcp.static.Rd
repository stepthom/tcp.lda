\name{tcp.static}
\alias{tcp.static}
\title{
Use the static call-graph technique to prioritize test cases.
}
\description{
Use the static call-graph technique to prioritize test cases.
}
\usage{
tcp.static(s = NULL, methodVectorsDir, testDir, testNamesMap = NULL, truthName = NULL, verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
same as in tcp.lda()
}
  \item{methodVectorsDir}{
string: the path to the directory containing method vectors: one vector file per
original test case file. Each vector file contains one row and M columns, where M is the number of methods
in the system. A "1" represents that this test case file calls this method; "0"
otherwise..
}
  \item{testDir}{
same as in tcp.lda()
}
  \item{testNamesMap}{
same as in tcp.lda()
}
  \item{truthName}{
%%     ~~Describe \code{truthName} here~~
same as in tcp.lda()
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
same as in tcp.lda()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
same as in tcp.lda(), with the addition of:
\item{methods }{the read method vectors.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
