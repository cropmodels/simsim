

#crop = "Soybean"; filex="ESAD8501.SBX"; planting="2022-10-15"; harvest="2022-05-15"; plantingWindow=7; ingenoid="IB0058"; path ="d:/agwise/dssat/"

# Create multiple experimental files
dssat.expfile <- function(crop, filex, planting, harvesting, plantingWindow=1, ingenoid, path) {
		#	path <- "d:/agwise/dssat/"


#	fgps = file.path(path, "input/AOI_GPS.rds")
#	countryCoord <- readRDS(fgps)
#	countryCoord <- unique(countryCoord[, c("lon", "lat")])
#	countryCoord <- countryCoord[stats::complete.cases(countryCoord), ]		

#	fsoil <- paste(path, "input/SoilDEM_PointData_AOI_profile.RDS", sep="")  
#	Soil <- readRDS(fsoil)
#	names(Soil)[names(Soil)=="lat"] <- "latitude"
#	names(Soil)[names(Soil)=="lon"] <- "longitude"
#	Soil <- na.omit(Soil)

	frain = file.path(path, paste0("input/Rainfall_Season_1_PointData_AOI.RDS"))
	Rainfall <- readRDS(frain)

	path.to.extdata <- file.path(path, "transform")
	path.to.temdata <- file.path(path, "landing")
	dir.create(path.to.temdata, FALSE)
	dir.create(path.to.extdata, FALSE)
	

	#We need to add more codes
	crops <- c("Maize", "Potato", "Rice", "Soybean", "Wheat")
	cropcodes <- c("MZ","PT", "RI", "SB", "WH")
	cropid <- cropcodes[crops == crop]
		
	## check if both planting and harvest dates are in the same year
	planting <- as.Date(planting)
	harvest <- as.Date(harvest)
	
	py <- meteor::fromDate(planting, "year")
	hy <- meteor::fromDate(harvest, "year")
	planting_harvest_sameYear <- py == hy
	
	## set planting date one moth prior to the given Planting_month_date so that initial condition for the crop model could be set correctly
	starting <- as.Date(planting) - 30

	## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.	
	ending <- harvest + 7 * (plantingWindow-1)
		
#	countryCoord <- countryCoord[stats::complete.cases(countryCoord), ]
#	names(countryCoord) <- c("longitude", "latitude")

#	metaDataWeather <- as.data.frame(Rainfall[,1:7])
#	metaData_Soil <- Soil[,c("longitude", "latitude", "NAME_1", "NAME_2")]
#	metaData <- merge(metaDataWeather,metaData_Soil)
#	metaData <- unique(metaData[,1:4])
#	coords <- merge(metaData, countryCoord)
#	grid <- as.matrix(coords)

	coords <- unique(Rainfall[, c("ID", "longitude", "latitude")])
	
	sy <- meteor::fromDate(starting, "year")
	ey <- meteor::fromDate(ending, "year")
	number_years <- sy == ey
	

	#Read in original FileX
	file_x <- DSSAT::read_filex(file.path(path.to.temdata, filex))

	file_x$FIELDS$ID_SOIL <- "IB00000004"
    file_x$CULTIVARS$CR <- cropid
    file_x$CULTIVARS$INGENO <- ingenoid
    ex_profile <- suppressWarnings(DSSAT::read_sol(file.path(path.to.temdata, "SOIL.SOL"), id_soil = file_x$FIELDS$ID_SOIL))
    file_x$`INITIAL CONDITIONS`$SH2O<- ex_profile$SDUL #Assume field capacity as initial condition
    file_x$`INITIAL CONDITIONS`$ICBL <- ex_profile$SLB
	file_x$`SIMULATION CONTROLS`$NYERS <- number_years
	file_x$TREATMENTS$TNAME <- paste0("Initial planting")

	np <- rep(1, plantingWindow)
	j  <- 1:plantingWindow
	win <- (j-1) * 7	
	for (v in c("INITIAL CONDITIONS", "PLANTING DETAILS", "HARVEST DETAILS", "SIMULATION CONTROLS", "TREATMENTS")) {
		file_x[[v]] <- file_x[[v]][np, ]
		file_x[[v]][,1] <- j 
	}
	file_x$`INITIAL CONDITIONS`$ICDAT <- starting + win
	file_x$`SIMULATION CONTROLS`$SDATE <- starting + win
	file_x$`PLANTING DETAILS`$PDATE <- planting + win
	file_x$`HARVEST DETAILS`$HDATE <- harvest + win

	file_x$TREATMENTS$TNAME <- paste0("Planting + ", win ,"days")
	file_x$TREATMENTS$IC <- j
	file_x$TREATMENTS$MP <- j
	file_x$TREATMENTS$MH <- j
	file_x$TREATMENTS$SM <- j


	for (i in 1:nrow(coords)) {
		file_x$FIELDS$WSTA <- paste0("WHTE", formatC(width = 4, as.integer(i), flag = "0"))
		file_x$FIELDS$ID_SOIL <- paste0('TRAN', formatC(width = 5, as.integer(i), flag = "0"))
		fout <- file.path(path.to.extdata, 
			paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',cropid,'X'))
		DSSAT::write_filex(file_x, fout)
	}

}

