.onAttach <- function(libname, pkgname) {
	
	c.config <- logitsgl.c.config()
	
	if(c.config$debugging) packageStartupMessage("logitsgl: Compiled with debugging on -- this may slow down runtime")
	
}