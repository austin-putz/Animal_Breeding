#==============================================================================#
# calc_intensity.R
#==============================================================================#

# Author: Austin Putz
# Created: May 11, 2017
# Modified: May 11, 2017
# License: GPLv2

#==============================================================================#
# function
#==============================================================================#

calc_intensity <- function(p, direction="pos") {
	
	# p = proportion selected (not percentage!)
	
	# direction = can be positive (think growth) or negative (think FCR)
	# direction = c("pos", "neg")
	
	if (!(direction %in% c("pos", "neg"))){
		warning("I don't recognize your direction option choose (\"pos\" or \"neg\"), will use \"pos\"")
	}
	
	# first get the quantile that that proportion selected corresponds to
	x = qnorm(1-p)
	
	# get the height of the standard normal 
	z = exp(-0.5*x^2) / (sqrt(2*3.14))
	
	# calculate i
	i = z / p
	
	# change direction if need be
	if (direction=="pos"){
		i = i
	} else if (direction=="neg") {
		i = -i
	} else {
		i = i
	}
	
	# return intensity
	return(i)
	
}





