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



analyzeFaultMatrix <- function(s, na="Ant v6"){
# Computes various statistics on the fault matrix
# out puts them to the screen

    a <- s$truth

    faults = ncol(a)
    tests  = nrow(a)
    fr     = 100*(sum(rowSums(a)>0) / tests)
    fpt    = mean(rowSums(a)) / faults
    tpf    = mean(colSums(a)) / tests
    t0     = 100*(sum(rowSums(a)==0) / tests)
    t50    = 100*(sum(rowSums(a)>=(faults/2)) / tests)
    t100   = 100*(sum(rowSums(a)==faults) / tests)

    cat(sprintf("%s & %d & %d & %.1f & %.2f & %.2f & %.1f & %.1f & %.1f \\\\\n",
                na, faults, tests, fr, fpt, tpf, t0, t50, t100)) 
}

