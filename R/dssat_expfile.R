
create_filex <-function(file_x, i, path.to.extdata) {
    file_x$FIELDS$WSTA <- paste0("WHTE", formatC(width = 4, as.integer(i), flag = "0"))
    file_x$FIELDS$ID_SOIL <- paste0('TRAN', formatC(width = 5, as.integer(i), flag = "0"))
	DSSAT::write_filex(file_x)
} 



crop = "Soybean"; filex="ESAD8501.SBX"; planting="2022-10-15"; harvest="2022-05-15"; plantingWindow=7; ingenoid="IB0058"; path ="d:/agwise/dssat/"

# Create multiple experimental files
dssat.expfile <- function(crop, filex, planting, harvesting, plantingWindow=1, ingenoid, 
		path) {
		#	path <- "d:/agwise/dssat/"

	fgps = file.path(path, "input/AOI_GPS.rds")
	frain = file.path(path, paste0("input/Rainfall_Season_1_PointData_AOI.RDS"))
	fsoil <- paste(path, "input/SoilDEM_PointData_AOI_profile.RDS", sep="")  

	countryCoord <- readRDS(fgps)
	countryCoord <- unique(countryCoord[, c("lon", "lat")])
	countryCoord <- countryCoord[stats::complete.cases(countryCoord), ]		

	Rainfall <- readRDS(frain)
	Soil <- readRDS(fsoil)
	names(Soil)[names(Soil)=="lat"] <- "latitude"
	names(Soil)[names(Soil)=="lon"] <- "longitude"
	Soil <- na.omit(Soil)

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
		
	countryCoord <- countryCoord[stats::complete.cases(countryCoord), ]
	names(countryCoord) <- c("longitude", "latitude")

	metaDataWeather <- as.data.frame(Rainfall[,1:7])
	metaData_Soil <- Soil[,c("longitude", "latitude", "NAME_1", "NAME_2")]
	metaData <- merge(metaDataWeather,metaData_Soil)
	metaData <- unique(metaData[,1:4])
	coords <- merge(metaData, countryCoord)
	grid <- as.matrix(coords)
	
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
	win <- j * 7	
	file_x$`INITIAL CONDITIONS` <- file_x$`INITIAL CONDITIONS`[np, ]
	file_x$`INITIAL CONDITIONS`$C <- j 
	
	file_x$`PLANTING DETAILS` <- file_x$`PLANTING DETAILS`[np,]
	file_x$`PLANTING DETAILS`$P <- j
		
	file_x$`HARVEST DETAILS` <- file_x$`HARVEST DETAILS`[np,]
	file_x$`HARVEST DETAILS`$H <- j
			
	file_x$`SIMULATION CONTROLS`<- file_x$`SIMULATION CONTROLS`[np,] 
	file_x$`SIMULATION CONTROLS`$N <- j

	file_x$TREATMENTS <- file_x$TREATMENTS[np, ]
	file_x$TREATMENTS$N <- j
	file_x$TREATMENTS$TNAME <- paste0("Planting + ", win ,"days")
	file_x$TREATMENTS$IC <- j
	file_x$TREATMENTS$MP <- j
	file_x$TREATMENTS$MH <- j
	file_x$TREATMENTS$SM <- j

	file_x$`HARVEST DETAILS`$HDATE <- as.Date(harvest) + win
	file_x$`PLANTING DETAILS`$PDATE <- as.Date(planting) + win
	file_x$`INITIAL CONDITIONS`$ICDAT <- as.Date(starting) + win
	file_x$`SIMULATION CONTROLS`$SDATE <- as.Date(starting) + win
	fout <- file.path(path.to.extdata, 
			paste0('EXTE', formatC(width = 4, as.integer((i)), flag = "0"),'.',cropid,'X'))


	for (i in seq_along(grid[,1])) {
		create_filex(file_x, i, fout)
	}

}

