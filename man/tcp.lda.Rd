\name{tcp.lda}
\alias{tcp.lda}
\title{
Use LDA to prioritize test cases.
}
\description{
This function uses LDA to prioritize the given test cases.

If a testNamesMap file is specified, duplicate test cases will be removed. 

If a ground truth is also given (via truthName), the function will also evaluate the
performance of the generated prioritization.
}
\usage{
tcp.lda(s = NULL, testDir, testNamesMap = NULL, truthName = NULL, K = 20, iter = 200, alpha = 0.1, beta = 0.1, distance = "manhattan", maximization = "greedy", rerunLDA = FALSE, verbose = FALSE)
}

\arguments{
  \item{s}{
    environment: The object returned by this function. If calling this function for the first
time, omit this variable. However, if calling this function a second time (for
example, to re-prioritize the test cases with a new random number), then
including this object will elimate having to reread data, run LDA, etc. 
}
  \item{testDir}{
string: The path to the preprocessed test cases to prioritize.
}
  \item{testNamesMap}{
string: The path name to the file sed to map file names on disk (e.g.,
"0001.java" with their original file names (e.g.,
"jdbcapi/metadataJdbc20.java"). This is necessary because the order of the
s$files and s$truth must match during evaluation, but the Corpus() function
(which is used to read in the s$files) reads files from disk alphebetically. By
renaming the files on disk to 0001.java, 0002.java, such that 0001.java is the
test case in row 1 of the ground truth matrix, 0002.java is the test case in row
2, and so on, we can ensure that the order of s$files and s$truth are the same,
and evaluation can execute properly. In addition, the original file names are
used to find and eliminate duplicate test cases.
}
  \item{truthName}{
string: The path to the ground truth of the system under test, if it is available.
Should be specified as a TxB binary matrix, where T is the number of test cases and B
is the number of bugs. A "1" indicates that the test detects the bug. Note that
the order of rows in this file should be the same as the alphanumeric ordering
of files in testDir.
}
  \item{K}{
integer: the number of topics in LDA.
}
  \item{iter}{
integer: the number of LDA iterations to run.
}
  \item{alpha}{
real: the alpha parameter of LDA.
}
  \item{beta}{
real: the beta parameter of LDA.
}
  \item{distance}{
string: the distance function to use. Acceptable values are any that can be passed to
dist(): ‘"euclidean"’, ‘"maximum"’, ‘"manhattan"’, ‘"canberra"’,
          ‘"binary"’ or ‘"minkowski"’.
}
  \item{maximization}{
string: the maximization algorithm to use: "greedy" or "clustering"
}
  \item{rerunLDA}{
logical: Re-execute LDA (only makes sense if s!=NULL)
}
  \item{verbose}{
logical: display verbose output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{whichToTry }{Description of 'comp2'}
\item{apfd }{Description of 'comp2'}
\item{results }{Description of 'comp2'}
\item{files }{Description of 'comp1'}
\item{lda }{Description of 'comp2'}
\item{whichToTry }{Description of 'comp2'}
\item{files }{Description of 'comp2'}
\item{truth }{Description of 'comp2'}
\item{truth_inverse }{Description of 'comp2'}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Stephen W. Thomas
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
