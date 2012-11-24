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

measureA <- function(a,b){
# Computes the Vargha-Delaney A measure for two populations a and b.
#
# Equation numbers below refer to the paper:
# @article{vargha2000critique,
#  title={A critique and improvement of the CL common language effect size
#               statistics of McGraw and Wong},
#  author={Vargha, A. and Delaney, H.D.},
#  journal={Journal of Educational and Behavioral Statistics},
#  volume={25},
#  number={2},
#  pages={101--132},
#  year={2000},
#  publisher={Sage Publications}
# }
# 
# Returns: A real number between 0 and 1 
# TODO:
#
# Args
#   a: a vector of real numbers
#   b: a vector of real numbers 
#
# Results
#   A real number between 0 and 1


    # Compute the rank sum (Eqn 13)
    r = rank(c(a,b))
    r1 = sum(r[seq_along(a)])
                      
    # Compute the measure (Eqn 14) 
    m = length(a)
    n = length(b)
    A = (r1/m - (m+1)/2)/n
       
    A
}

