
rundssat <-function(i,path.to.extdata,TRT,AOI) {
	setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0")), sep = "/"))
	# Generate a DSSAT batch file using a tibble
	options(DSSAT.CSM="/opt/DSSAT/v4.8.1.40/dscsm048")
	tib <- tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.SBX'), TRTNO=TRT, RP=1, SQ=0, OP=0, CO=0)
	DSSAT::write_dssbatch(tib, file_name="DSSBatch.v48")
	# Run DSSAT-CSM
	DSSAT::run_dssat(file_name="DSSBatch.v48",suppress_output = TRUE)
	# Change output file name
	file.rename("Summary.OUT", paste0(path.to.extdata, '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((i)), flag = "0"), '.OUT'))
}


dssat.exec <- function(country, useCaseName, Crop, AOI = FALSE,TRT=1, cultivarType= "mediumDuration", 
	path="/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/") {	
	
	#require(doParallel)
	#require(foreach)
	# Set number of parallel workers
	#cls <- parallel::makePSOCKcluster(jobs)
	#doParallel::registerDoParallel(cls)
	#Set working directory to save the results
	path.to.extdata <- file.path(path, paste0("useCase_", country, "_", useCaseName, "/", Crop, "/transform/DSSAT"))
	setwd(path.to.extdata)	
	folders <- list.dirs(".", full.names = FALSE, recursive = FALSE)
	matching_folders <- folders[grepl("EXTE", folders, ignore.case = TRUE)]
	#foreach::foreach(i=seq_along(matching_folders), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
 
	print("Progress:")
	results <- purrr::map(seq_along(matching_folders), rundssat, path.to.extdata=path.to.extdata, TRT=TRT, AOI=AOI) 
	# %||% print("Progress:")
}

