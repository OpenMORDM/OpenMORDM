# Copyright 2014-2015 The Pennsylvania State University
#
# OpenMORDM was developed by Dr. David Hadka with guidance from Dr. Klaus
# Keller and Dr. Patrick Reed.  This work was supported by the National
# Science Foundation through the Network for Sustainable Climate Risk
# Management (SCRiM) under NSF cooperative agreement GEO-1240507.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.onLoad <- function(libname, pkgname) {
	# Use null device to allow Shiny RGL to work
	if (is.null(getOption("rgl.useNULL"))) {
		options(rgl.useNULL=TRUE)
	}
	
	# Create a private environment for storing plot settings
	assign("mordm.globals", new.env(), envir=parent.env(environment()))
	
	options(mordm.palette=Curry(rainbow, start=0, end=2/6))
}