
merge_DSSAT_output <- function(path) {
  
  # Set number of parallel workers
	#cls <- parallel::makePSOCKcluster(jobs)
	#doParallel::registerDoParallel(cls)

	path.to.extdata <- file.path(path, "/transform/DSSAT/")
	setwd(path.to.extdata)

	lf <- list.files()
	
	f_all <- NULL

	f_all <- lapply(1:length(lf), function(i) {		
		base <- lf[i]
		if (file.exists(paste0(base,"/", base, ".OUT"))) {
			a <- read_output(paste0(base,"/", base, ".OUT"))
			d <- a[,c("XLAT","LONG","TRNO","TNAM","PDAT", "HDAT","CWAM","HWAH","CNAM","GNAM","NDCH","TMAXA",
									"TMINA","SRADA","PRCP","ETCP","ESCP")]
			b <- utils::read.table(paste0(base,"/", base, ".OUT"), skip = 4, header = FALSE)
			b <- data.frame(b)
			d$XLAT <- b$V15
			d$base <- base
				# colnames(d) <- c('latitude','longitude','treatment.number','treatment.name','planting.date','harvesting.date','Total.aboveground.biomass(kg/ha)','WLY(kg/ha)', 'Total.aboveground.bio.N%(kg/ha)','GrainNMaturity(kg/ha)','crop.duration','Av.Tmax(°C)', 'Av.Tmin(°C)','A.Solar.rad(MJ/m2/d)','Total.Seasonal.Rainfall(mm)',							'Total.Seasonal.ETranspiration(mm)','Total.Seasonal.Soil.Evaporation(mm)')
			d$WUE <- d$HWAH / d$PRCP		
			d
		}})
	f_all <- do.call(rbind, f_all)	
		
	fn <- = file.path(path, "/result/DSSAT/AOI.rds")	
	saveRDS(f_all, fn)
}

