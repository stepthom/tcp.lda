###################################################
# Stephen W. Thomas
# sthomas@cs.queensu.ca
#
# paper.R
#
# How to use the tcp-lda package to perfom the analysis in ur
# EMSE paper. Note that this will just perform the analyses
# for one system under test, Derby v1, although the other SUTs
# are exactly the same (with different input files).
#
# Each technique is run 900 random times. Results are then comared
# using Mann-Whitney and Vargha-Delaney statistics.
###################################################
 
# Source all the files in the "../R" directory
for(file in Sys.glob("../R/*.R")) source(file) 


# For a given system under test, run the four TCP approaches, each 900 times
doSystem <- function(testDir, testNamesMap, truthName, methodVectorsDir){

    # Run the random algorithm
    r1 <- tcp.random(testDir=testDir, testNamesMap=testNamesMap, truthName=truthName)
    set.seed(4566598) # Set a seed (which was draw at random) to make reproducible
    apfd.r = array(0, 900)
    for (iter in 1:900){
        r1 = tcp.random(r1)
        apfd.r[iter] =  r1$apfd
    }
    
    # Run the string-based algorithm
    s1 = tcp.string(testDir=testDir, testNamesMap=testNamesMap, truthName=truthName)
    set.seed(88898733) # Set a seed (which was draw at random) to make reproducible
    apfd.s = array(0, 900)
    for (iter in 1:900){
        s1 = tcp.string(s1)
        apfd.s[iter] =  s1$apfd
    }
    
    # Run the static call graph-based algorithm
    t1 = tcp.static(methodVectorsDir=methodVectorsDir, 
                        testDir=testDir, testNamesMap=testNamesMap, truthName=truthName)
    set.seed(3429587) # Set a seed (which was draw at random) to make reproducible
    apfd.t = array(0, 900)
    for (iter in 1:900){
    t1 = tcp.static(t1)
    apfd.t[iter] =  t1$apfd
    }
    
    # Run the LDA-based algorithm
    l1 = tcp.lda(testDir=testDir, testNamesMap=testNamesMap, truthName=truthName, K=39)
    set.seed(289591140)  # Set a seed (which was draw at random) to make reproducible
    apfd.l = array(0, 900)
    counter = 1
    for (iter1 in 1:30){
        cat(sprintf("lda iter %d of 30\n", iter1)) 
        l1 = tcp.lda(l1, K=39, rerunLDA=T)
        for (iter2 in 1:30){
            apfd.l[counter] =  l1$apfd
            counter = counter+1
        }
    }

    list(apfd.r=apfd.r, apfd.s=apfd.s, apfd.t=apfd.t, apfd.l=apfd.l)
}

# Output statistics for this SUT
# Mean APFD; Mann-Whiteny tests; and A measures
outputStatistics <- function(sut){
    
    # Look at descriptive statistics about the performance
    cat(sprintf("\nSummaries:\n"))
    cat(summary(sut$apfd.r))
    cat(summary(sut$apfd.s))
    cat(summary(sut$apfd.t))
    cat(summary(sut$apfd.l))
    
    # Are the results significant? Use the Mann-Whitney U test and check the p-value
    cat(sprintf("\nU test:\n"))
    cat(sprintf("RDM vs LDA: %f\n", wilcox.test(sut$apfd.r, sut$apfd.l)$p.value))
    cat(sprintf("STR vs LDA: %f\n", wilcox.test(sut$apfd.s, sut$apfd.l)$p.value))
    cat(sprintf("STA vs LDA: %f\n", wilcox.test(sut$apfd.t, sut$apfd.l)$p.value))
    
    # What is the effect size? Use the Vargha-Delaney A-measure
    cat(sprintf("\nA measure:\n"))
    cat(sprintf("RDM vs LDA: %f\n", measureA(sut$apfd.l, sut$apfd.r)))
    cat(sprintf("RDM vs LDA: %f\n", measureA(sut$apfd.l, sut$apfd.s)))
    cat(sprintf("RDM vs LDA: %f\n", measureA(sut$apfd.l, sut$apfd.t)))

}


testDir          <- "data/derby/v1/preA.pruned0180"
testNamesMap     <- "data/derby/v1/test_names.dat"
truthName        <- "data/derby/v1/test_matrix.dat"
methodVectorsDir <- "data/derby/v1/calledMethodVectors"

derby1 <- doSystem(testDir, testNamesMap, truthName, methodVectorsDir)
outputStatistics(derby1)
