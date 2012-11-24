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

# Uses the static call-graph algorithm to prioritize the given test cases.

tcp.static <- function(
                s=NULL, 
                methodVectorsDir, 
                testDir, 
                testNamesMap=NULL,
                truthName=NULL, 
                verbose=F){

    if (is.null(s)){
        s <- new.env()
    }

    if (is.null(s$methods)){
        if (verbose==T){
            cat(sprintf("Reading method vectors...\n"))
        }
        # Now read in the method vectors for each test case
        tmp         <- Corpus(DirSource(methodVectorsDir))
        # Reformat the string vectors (in list format) into a matrix of integers
        tmp1        <- lapply(tmp, function(x){strsplit(x, " ")})
        tmp2        <- lapply(tmp1, function(x){as.numeric(unlist(x))})
        s$methods   <- t(do.call(cbind, tmp2))
    }

    if (is.null(s$files)){
        if (verbose==T){
            cat(sprintf("Reading test directory...\n"))
        }
        .loadAndCleanData(s, testDir, testNamesMap, truthName, verbose)
        # Now remove the duplicate methods, just like the duplicate files and
        # truth rows were removed in loadAnCleanData()
        s$methods = s$methods[-s$removedIDs,]
    }

    if (verbose==T){
        cat(sprintf("Running the greedy maximization algorithm...\n"))
    }
    s$whichToTry = .doStatic(s)

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

