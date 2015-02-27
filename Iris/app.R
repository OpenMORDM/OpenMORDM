# Copyright 2014 The Pennsylvania State University
#
# OpenMORDM was developed by Dr. David Hadka with guidance from Dr. Klaus
# Keller and Dr. Patrick Reed.  This work was supported by the National
# Science Foundation through the Network for Sustainable Climate Risk
# Management (SCRiM) under NSF cooperative agreement GEO-1240507.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

options(rgl.useNULL=TRUE)

library(functional)
library(OpenMORDM)
library(shiny)
library(shinyRGL)
library(rgl)
library(scales)
library(grid)
library(prim)
library(MASS)
library(animation)
library(sensitivity)
library(boot)
library(pracma)
library(emoa)
library(stringr)
library(dichromat)
library(rpart)
library(rpart.plot)
library(party)
library(stats)
library(corrgram)
library(lhs)
library(rdyncall)

#rgl.init()


data(iris)
explore(iris, runShinyApp=FALSE)