#######################################################################
# tcp-lda - test case prioritization with LDA
# Copyright (C) 2012 Stephen W. Thomas
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


# Uses LDA to prioritize the given test cases.

tcp.lda <- function(   
            s=NULL,  # If calling this function a second time, this should be
                     # resulting object of the first
            testDir,  # Name of the directory with the preprocessed test files
            testNamesMap=NULL,  # testing map
            truthName=NULL, # ground truth (fault matrix)
            K=20,   # LDA parameter number of topics
            iter=200,   # LDA parameter number of iterations
            alpha=0.1,  # LDA parameter alpha
            beta=0.1,  #LDA parameter beta                        
            distance="manhattan",  # distance measure to use (passed to dist())
            maximization="greedy", # max. algorithm to use (greedy or clustering)
            rerunLDA=FALSE, # Should we rerun LDA, if it's already been run?
            verbose=FALSE){

    if (is.null(s)){
        s <- new.env()
    }

    if (is.null(s$files)){
        if (verbose==TRUE){
            cat(sprintf("Reading test directory...\n"))
        }
        .loadAndCleanData(s, testDir, testNamesMap, truthName, verbose)
    }

    # Compute LDA model, if we need to
    if (rerunLDA==T || is.null(s$lda)){
        if (verbose==TRUE){
            cat(sprintf("Computing the LDA model...\n"))
        }
        s$lda       <- .computeTopics(s$files, K=K, 
                        ITER=iter, ALPHA=alpha, BETA=beta)
        s$lda.dists <- as.matrix(dist(s$lda$theta, method="manhattan"))
        diag(s$lda.dists)  <- 10000 # Avoid the 0 difference to ourselves
    }

    # Now run the specified maximization algorithm
    if (maximization=="greedy"){
        if (verbose==TRUE){
            cat(sprintf("Running the greedy maximization algorithm...\n"))
        }
        s$whichToTry  <- .doGreedy(s$lda.dists)
    } else if (maximization=="cluster"){
        if (verbose==TRUE){
            cat(sprintf("Running the clustering maximization algorithm...\n"))
        }
        s$whichToTry <- .doClustering(s, s$lda.dists, k=10) 
    } else {
            cat(sprintf("Error: Unknown maximization algorithm: %s\n", maximization))
            return()
    }

    # Now, if we have ground truth available to use, let's evaluate our results
    if (!is.null(s$truth_inverse)){
        if (verbose==TRUE){
            cat(sprintf("Evaluating results using ground truth...\n"))
        }
        .evaluate(s)
    }
    
    if (verbose==TRUE){
        cat(sprintf("Done.\n"))
    }
    s
}

