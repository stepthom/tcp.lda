
library(tm)
library(plotrix)
library(reshape)
library(lda)
library(plyr)
library(cluster)
library(subselect)
library(FNN)
library(RecordLinkage)
library(bioDist)
library(e1071)

.computeTopics <-
function(f, K=100, ITER=200, ALPHA=0.1, BETA=0.1){
# Runs LDA (using tm package) on the given files)
#
# Args:
#   f: the set of files (a Corpus object) to run LDA  on
#   K: the number of topics
#   ITER: number of iterations
#   ALPHA: the alpha parameter of LDA
#   BETA: the beta parameter of LDA
#
# Results
#   The object returned by tm's lda.collapsed.gibbs.sampler


    a      <- sapply(f, function(x){paste(x, collapse=" ")})
    cora   <- lexicalize(as.vector(a))
    result <- 
         lda.collapsed.gibbs.sampler(cora$documents, K, 
            cora$vocab, ITER, ALPHA, BETA, compute.log.likelihood=FALSE) 

    # top.words <- top.topic.words(result$topics, 5, by.score=TRUE)

    # transform topic proportions
    result$topic.memberships <- t(result$document_sums) / colSums(result$document_sums)
    result$topic.memberships <- split(result$topic.memberships, row(result$topic.memberships))
    names(result$topic.memberships) = names(f)

    # normalize phi matrix
    for (i in 1:nrow(result$topics)){
        result$topics[i,] = result$topics[i,] / sum(result$topics[i,])
    }

    # Convienence method
    result$theta = do.call(rbind, result$topic.memberships)

    # Get rid of data we don't need
    result$assignments=NULL
    result$document_sums=NULL
    result
}

.doClustering <-
function(s, distMatrix, k=10){
# Clustering maximization algorithm, using PAM
#
# Args:
#  s: interal tcp-lda object
#  distMatrix: the distance matrix to cluster
#  k: the number of clusters
#
# Results
#  A list of test cases to execute 

    # because pam won't run this case where k=n, initialize the results for
    # maximal performance (i.e., we find all bugs); this would happen anyway
    # even if pam would run with k=n 
    clusters   <- pam(distMatrix, k=k, diss=T)

    # Now, need to do two things:
    # 1. Seperate the clusters
    c = list()
    for (j in 1:k){ # AKA, for each cluster, pick one
        c[[j]] = as.vector(which(clusters$clustering==j))
    }
    
    # and 2. Order the clusters (here, based on farthest average distance)
    ordered = list()
    for (j in 1:k){ 
        if (length(c[[j]]) > 1){
            averageDistances = apply(distMatrix[c[[j]],c[[j]]], 1, mean) 
            idx =  sort(averageDistances, decreasing=T, index.return=T)$ix
            ordered[[j]] = c[[j]][idx]
        } else { # only one element
            ordered[[j]] = c[[j]]
        }
    }
    

    # Now, we can iteratively select one from each cluster
    whichToTry <- vector(length=s$howManyTests)

    # The following algorithm is described in Carlson et al, 2011 ICSM, 
    # "A clustering approach to improving test case prioritization: an
    # industrial case study"
    # What we want to to is to visit each cluster, one by one, and select the 
    # top item in that cluster. and so on, until we've selected all the tests
    curJ = 1
    for (t in 1:s$howManyTests){ 
        # Select the current cluster (make sure it's not empty)
        while (length(ordered[[curJ]]) == 0){
            curJ = (curJ + 1) %% (k+1)
            if (curJ==0) curJ=1
        }
        #cat(sprintf("test case %d: selecting from cluster %d, which has %d elements\n", t, curJ, length(ordered[[curJ]])))

        # add to the list
        whichToTry[t] = ordered[[curJ]][1]
    
        # remove current element from cluster
        ordered[[curJ]] = setdiff(ordered[[curJ]], whichToTry[t])

        curJ = (curJ + 1) %% (k+1)
        if (curJ==0) curJ=1
    }
    whichToTry
}

.doGreedy <-
function(dd){
# Perform the greedy selection scheme
#
# Args:
#   dd: The distance matrix to maximizate
#
#
# Results
#  A list of test cases to execute 

    # First, find the min values for each row (each test case)
    # (This is precisely the greed scheme)
    mins           <- apply(dd, 1, min)
    
    # To break ties, reorder the mins (using sample() before sorting;
    # that way, if there happend to be a tie, then it might or might not be
    # broken by the sample() - sort() will always break ties the same way.
    tt             <- sample(length(mins))
    whichToTry     <- tt[sort(mins[tt], decreasing=T, index.return=T)$ix]
    whichToTry
}

.doStatic <-
function(s){
# TODO:
# numTests is the number of tests to select
# This function implements the JuptaT algorithm, from
# Zhang et al., ICSM 2009, "Prioritizing JUnit Test Cases in Absence of Coverage
# Information" pg. 23
#
# Args:
#  s: interal tcp-lda object
#
# Results
#  A list of test cases to execute 

    TS  = 1:nrow(s$methods) # Test set starts with all tests (indexes)
    PS  = c() # Prioritized Suite starts empty
    SCORES  = c() # holds the scores of the prioritized test case
    SS  = c() # Slide set, to indiciate which test cases have already been
              # chosen, also starts emtpy
    PSi = 0   # counter for PS
    SSi = 0   # counter for SS (may be different; see below)
    # select numTests tests, one at a time (highest to lowest priority)
    for (i in 1:s$howManyTests){
        SSi = SSi+1
        PSi = PSi+1
        if (length(TS) > 1){
            # adjust TA of each test case using EQN 3 in the paper
            if (length(SS) > 0){
                #all methods in slideset
                usedMethods = which(s$methods[SS,]>0, arr.ind=T)
                if (length(SS) > 1){
                    usedMethods = unique(usedMethods[,2])
                }

                if (length(usedMethods) == ncol(s$methods)){
                    # Thes cscore of all remaining tests will be 0
                    TA = rowSums(s$methods[TS,])
                    TA[1:length(TA)]=0
                } else if (length(usedMethods) == ncol(s$methods)-1){
                    TA = s$methods[TS,-usedMethods]
                } else {
                    TA = rowSums(s$methods[TS,-usedMethods])
                }
            } else {
                TA = rowSums(s$methods[TS,])
            }
            # select t with highest TA in TS
            t = which.max(TA)
            # if there's a tie, randomly break it
            idxs = which(TA==TA[t])
            if (length(idxs)>1){
                t = idxs[sample(length(idxs), 1)]    
            }
            # if TA(t) = 0, then no remaining tests cases cover new methods
            # in this case, clear the SS
            if (TA[t] == 0){
                SS  = c()
                SSi = 1
            }
            trowname= names(TA)[t]
            idx     = which(rownames(s$methods) == trowname)
        } else {
            idx      = TS
        }
        #  updated the sets
        TS      = setdiff(TS, idx)
        SS[SSi] = idx 
        PS[PSi] = idx 
        SCORES[PSi] = TA[t]
    }
    PS # this is the same as "whichToTry" in other functions
}

.evaluate <-
function(s){
# Evaluate the results of a given whichToTry
#
# Args:
#  s: interal tcp-lda object, which has already been executed (i.e., whichToTry
#  is popualted) and has truth data (i.e., truth and truth_inverse)
#
#
# Results
#  s$results and s$apfd will be now populated.

    # Now compute the results (i.e., how many bugs were found) for 
    # different number of test cases (1 to howManTests)
    s$results = array(0, s$howManyTests)
    for (n in 1:s$howManyTests){
        s$results[n]  = .scoreTests(s$truth_inverse[s$whichToTry[1:n],]) 
    }

    # Compute the APFD measure
    y <- s$results
    x <- 1:length(y)
    s$apfd = 100* .simp(y=y,x=x)/(s$howManyBugs*s$howManyTests)

    s
}

.loadAndCleanData <-
function(s, testDir, testNamesMap=NULL, truthName=NULL, verbose=FALSE){
# Load the data from disk.
# This will read the files in sorded order (so are the s$truth); 
# THIS IS IMPORTANT. If this function is chagned to something else, 
# make sure the files are sorted
#
# Args:
#  s: interal tcp-lda object
#
#
# Results
#   The following "member" variables of s will be populated:
#    s$files
#    s$howManyTests
#    s$names
#    s$removedIDs
#    s$truth (if truthName is given)

    doTruth = !is.null(truthName)
   
    # Read the files into a Corpus object, which will be needed for
    # the LDA computation later (if necessary) 
    s$files     <- Corpus(DirSource(testDir))

    if (doTruth==T){
        s$truth <- read.csv(truthName, sep=" ", header=F)
        s$truth <- s$truth[,-ncol(s$truth)] # Get rid of last col (NA)
        rownames(s$truth) <- s$names$V1
    }
  
    # Now read in the test name mapping file, which should have three
    # columns: code, short name, long name 
    if (!is.null(testNamesMap)){
        s$names <- read.csv(testNamesMap, sep=" ", header=F)
    } else {
        s$names <- data.frame(names(s$files), names(s$files),
                                names(s$files))
    }

    # Now remove duplicate test files (as discussed with Hadi) and only keep one; doesn't
    # matter which
    if (verbose==TRUE){
        cat(sprintf("Removing duplicate test files...\n"))
    }
    numRemoved <- 0
    removedIDs <- c()
    u <- unique(s$names$V3)
    for (i in 1:length(u)){
        thisFileName <- u[i]
        idx          <- which(s$names$V3 %in% thisFileName) # The duplicates
        if (length(idx)>1){
            removedIDs <- c(removedIDs, idx[2:length(idx)])

            if (doTruth==T){
                # First, average rows from truth
                avg              <- mean(s$truth[idx,])
                s$truth[idx[1],] <- avg
            }
        }
    }
    s$names <- s$names[-removedIDs,]
    s$files <- s$files[-removedIDs]
    s$removedIDs <- removedIDs

    if (doTruth==T){
        s$truth <- s$truth[-removedIDs,]
        s$howManyBugs = ncol(s$truth)
        # Following Hadi's TOSEM paper, the probability of finding a fault in a set
        # of test cases is the same as 1 minus the probability of not finding the
        # fault with any of the test cases; so, prepare
        s$truth_inverse  <- 1 - s$truth
    }
    
    if (verbose==TRUE){
        cat(sprintf("Removed %d duplicates.\n", length(removedIDs)))
    }
    s$howManyTests <- length(s$files)
    s
}

.preComputeStringEdit <-
function(s){
# Compute the string edit distance
#
# Args:
#  s: interal tcp-lda object
#
#
# Results
#  s$string.dists will be populated


    # Levenstine distnace
    #m       <- matrix(0, nrow=length(s$files.raw), ncol=length(s$files.raw))
    #for (i in 1:nrow(m)){
        #a <-  paste(as.character(s$files.raw[[i]]), collapse=" ")
        #for (j in 1:ncol(m)){
            #b      <-  paste(as.character(s$files.raw[[j]]), collapse=" ")
            #m[i,j] <- levenshteinDist(a, b)
        #}
    #}

    # Manhattan distance: to use the function man(), need to create
    # a matrix where each row is a test case, and each column is a value
    # padded with 0s for shorter strings
    a = lapply(s$files, function(x){ paste(as.character(x), collapse=" ") })
    nco = max(unlist(lapply(a, nchar))) # Get how many columns we need (max string length)

    charMatrix = array(0, c(length(s$files), nco))
    for (i in 1:length(s$files)){
        b = strtoi(charToRaw(a[[i]]), 16L)
        charMatrix[i,1:length(b)] = b
    }

    s$string.dists = as.matrix(man(charMatrix))
    s
}

.scoreTests <-
function(truth_inverse){
# Determine how many bugs a given set of tests has discoverd.
# Formala below is based on Hadi Hemmati's TOSEM paper
#
# Args:
#  truth_inverse: a matrixwho's rows are the executed test cases, and columsn
#  are bugs.
#
#
# Results
# The number of bugs that these test cases have detected.

    #sum(1 - (apply((1-truth[idx,]), 2, prod)))
    # Speed up, since reduce is faster than apply or for loop
    a = t(truth_inverse)
    sum(1 - Reduce(get("*"), as.data.frame(a), rep(1, nrow(a))))
}

.simp <-
function (y, a = NULL, b = NULL, x = NULL, n = 200){
# The Simpon's rule formulae for calculating the area under the curve.
# Borrowed from the R forums:
# https://stat.ethz.ch/pipermail/r-help/2008-July/166422.html
#
# Args:
#  y: the y points of the curve
#  x: the x points of the curve
#
# Results
#   The area under the curve (real number)

     if (is.null(a) | is.null(b)) {
         if (is.null(x))
             stop("No x values provided to integrate over.\n")
     }
     else {
         x <- c(a, b)
     }
     fff <- 1
     if (length(x) == 2) {
         if (x[1] == x[2])
             return(0)
         if (x[2] < x[1]) {
             fff <- -1
             x <- rev(x)
         }
         x <- seq(x[1], x[2], length = n)
         if (is.function(y))
             y <- y(x)
         else {
             cat("y must be a function when x is\n")
             cat("of length equal to 2.\n")
             stop("Bailing out.\n")
         }
         equisp <- TRUE
     }
     else {
         if (is.function(y))
             y <- y(x)
         else if (length(y) != length(x))
             stop("Mismatch in lengths of x and y.\n")
         s <- order(x)
         x <- x[s]
         ddd <- diff(x)
         if (any(ddd == 0))
             stop("Gridpoints must be distinct.\n")
         equisp <- isTRUE(all.equal(diff(ddd), rep(0, length(ddd) - 1)))
         y <- y[s]
     }
     n <- length(x) - 1
     if (equisp) {
         old.op <- options(warn = -1)
         on.exit(options(old.op))
         M <- matrix(y, nrow = n + 2, ncol = 4)[1:(n - 2), ]
         h <- x[2] - x[1]
         fc <- h * c(-1, 13, 13, -1)/24
         aa <- apply(t(M) * fc, 2, sum)
         a1 <- h * sum(y[1:3] * c(5, 8, -1))/12
         an <- h * sum(y[(n - 1):(n + 1)] * c(-1, 8, 5))/12
         return(fff * sum(c(a1, aa, an)))
     }
     m <- n%/%2
     i <- 1:(m + 1)
     a <- x[2 * i] - x[2 * i - 1]
     i <- 1:m
     b <- x[2 * i + 1] - x[2 * i]
     o <- (a[i] * b + 2 * a[i] * a[i] - b * b)/(6 * a[i])
     p <- (a[i] + b)^3/(6 * a[i] * b)
     q <- (a[i] * b + 2 * b * b - a[i] * a[i])/(6 * b)
     k <- numeric(n + 1)
     k[1] <- o[1]
     i <- 1:(m - 1)
     k[2 * i] <- p[i]
     k[2 * i + 1] <- q[i] + o[-1]
     if (n > 2 * m) {
         aa <- a[m + 1]
         bb <- b[m]
         den <- 6 * bb * (bb + aa)
         k[2 * m] <- p[m] - (aa^3)/den
         k[2 * m + 1] <- q[m] + (aa^3 + 4 * bb * aa^2 + 3 * aa *
             bb^2)/den
         k[2 * m + 2] <- (2 * bb * aa^2 + 3 * aa * bb^2)/den
     }
     else {
         k[2 * m] <- p[m]
         k[2 * m + 1] <- q[m]
     }
     fff * sum(k * y)
}

