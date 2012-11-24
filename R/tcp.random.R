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

# Randomly prioritizes test cases

tcp.random <- function(
                s=NULL,  
                testDir,  
                testNamesMap=NULL, 
                truthName=NULL,
                verbose=F){

    if (is.null(s)){
        s <- new.env()
    }
    if (is.null(s$files)){
        if (verbose==TRUE){
            cat(sprintf("Reading test directory...\n"))
        }
        .loadAndCleanData(s, testDir, testNamesMap, truthName, verbose)
    }


    if (verbose==T){
        cat(sprintf("Running the random maximization algorithm...\n"))
    }
    s$whichToTry  <- sample(1:s$howManyTests)

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

