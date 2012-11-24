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

# Uses the string-edit based approach to prioritize the given test cases.



tcp.string <- function(
                s=NULL, 
                testDir, 
                testNamesMap=NULL, 
                truthName=NULL,
                verbose=F){

    if (is.null(s)){
        s <- new.env()
    }

    if (is.null(s$files)){
        if (verbose==T){
            cat(sprintf("Reading test directory...\n"))
        }
        .loadAndCleanData(s, testDir, testNamesMap, truthName, verbose)
    }

    # Compute the string distances, if we need to
    if (is.null(s$string.dists)){
        if (verbose==T){
            cat(sprintf("Computing string distances...\n"))
        }
        s <- .preComputeStringEdit(s) 
        diag(s$string.dists)  <- 10000  # Avoid the 0 difference to ourselves
    }

    if (verbose==T){
        cat(sprintf("Running the greedy maximization algorithm...\n"))
    }
    s$whichToTry  <- .doGreedy(s$string.dists)

    if (!is.null(s$truth_inverse)){
        if (verbose==TRUE){
            cat(sprintf("Evaluating results using ground truth...\n"))
        }
        .evaluate(s)
    }

    if (verbose==T){
        cat(sprintf("Done.\n"))
    }
    s
}

