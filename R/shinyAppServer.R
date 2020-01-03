#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Define server logic required to draw all plots and tables
shinyAppServer <- function(input, output) {

	input.var <- reactive({
		input$var
	})

	input.var.name <- reactive({
		if (input.var() == "dst") {
			input.var.name = "Dst"
		} else if (input.var() == "ae") {
			input.var.name = "AE"
		} else if (input.var() == "al") {
			input.var.name = "AL"
		} else if (input.var() == "au") {
			input.var.name = "AU"
		} else if (input.var() == "ap") {
			input.var.name = "AP"
		} else if (input.var() == "ey") {
			input.var.name = "Ey"
		} else if (input.var() == "bz") {
			input.var.name = "BZ (GSM)"
		} else if (input.var() == "sw") {
			input.var.name = "Solar Wind Speed"
		} else if (input.var() == "beta") {
			input.var.name = "Plasma Beta"
		} else if (input.var() == "sunspot") {
			input.var.name = "Sunspot Number"
		} else if (input.var() == "f107") {
			input.var.name = "Solar Index F10.7"
		} else if (input.var() == "cme") {
			input.var.name = "CME Speed"
		} else if (input.var() == "bn") {
			input.var.name = "IMF Bn"
		} else {
			input.var.name = "User Data"
		}
		input.var.name
	})

	dataInput <- reactive({
		if (input.var() == "dst") {
			#whDst <- as.numeric(input$whDst)
			dataInput <- dstData #dstData[[whDst]]
		}
		if (input.var() == "ae") {
			#whAe <-  as.numeric(input$whAe)
			dataInput <- aeData #aeData[[whAe]]
		}
		if (input.var() == "al") {
			dataInput <- alData
		}
		if (input.var() == "au") {
			dataInput <- auData
		}
		if (input.var() == "ap") {
			dataInput <- apData
		}
		if (input.var() == "ey") {
			dataInput <- eyData
		}
		if (input.var() == "bz") {
			dataInput <- bzData
		}
		if (input.var() == "sw") {
			dataInput <- swData
		}
		if (input.var() == "beta") {
			dataInput <- betaData
		}
		if (input.var() == "sunspot") {
			dataInput <- sunspotData
		}
		if (input.var() == "f107") {
			dataInput <- f107Data
		}
		if (input.var() == "cme") {
			dataInput <- cmeData
		}
		if (input.var() == "bn") {
			dataInput <- bnData
		}
		if (input.var() == "bz") {
			dataInput <- bzData
		}

		colnames(dataInput) <- c("evnt_time", "evnt")
		# remove any row with NA(s) 
		dataInput <- na.omit(dataInput)
		dataInput
	})


	dataGNR <- reactive({
		# Make sure requirement is met-User has selected an input file
		req(input$file1)
		inFile <- input$file1
		data <- read.csv(inFile$datapath, header = TRUE)
		ncol = dim(data)[2]
		nrow = dim(data)[1]
		month = day = rep(1, nrow)
		hour = rep(12, nrow)
		min = sec = rep(0, nrow)
		evnt <- data[, ncol]
		year <- data[, 1]
		if (ncol > 2) {
			month <- data[, 2]
		}
		if (ncol > 3) {
			day <- data[, 3]
		}
		if (ncol > 4) {
			hour <- data[, 4]
		}
		if (ncol > 5) {
			min = data[, 5]
		}
		if (ncol > 7) {
			sec = data[, 6]
		}

		evnt_time <- ISOdate(year, month, day, hour, min = min, sec = sec, tz = "GMT")

		dataGNR <- data.frame(evnt_time, evnt)
		# remove any row with NA(s) 
		dataGNR <- na.omit(dataGNR)
		dataGNR

	})
	## Load a small data set for Dst - for the purpose of plotting the  raw data only
	
	dataInputSml <- reactive({
		if (input.var() == "dst") {
			dataInputSml <- dstDataSml
		} else if (input.var() == "ae") {
			dataInputSml <- aeDataSml
		} else if (input.var() == "al") {
			dataInputSml <- alDataSml
		} else if (input.var() == "au") {
			dataInputSml <- auDataSml
		} else if (input.var() == "ap") {
			dataInputSml <- apDataSml
		} else if (input.var() == "ey") {
			dataInputSml <- eyDataSml
		} else if (input.var() == "bz") {
			dataInputSml <- bzDataSml
		} else if (input.var() == "sw") {
			dataInputSml <- swDataSml
		} else if (input.var() == "beta") {
			dataInputSml <- betaDataSml
		} else if (input.var() == "sunspot") {
			dataInputSml <- sunspotDataSml
		} else if (input.var() == "f107") {
			dataInputSml <- f107DataSml
		} else if (input.var() == "cme") {
			dataInputSml <- cmeData
		} else if (input.var() == "bn") {
			dataInputSml <- bnDataSml
		} else { ## input.var() == \gnr\""
			req(input$file1)
			dataInputSml <- dataGNR()
		}
		colnames(dataInputSml) <- c("evnt_time", "evnt")
		dataInputSml <- na.omit(dataInputSml)
		dataInputSml
	})


	## Select the proper time span
	finalOutput <- reactive({
		if (input.var() == "dst") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntDst[1] & evnt_time <= input$tIntDst[2])
		} else if (input.var() == "ae") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntAE[1] & evnt_time <= input$tIntAE[2])
		} else if (input.var() == "al") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntAL[1] & evnt_time <= input$tIntAL[2])
		} else if (input.var() == "au") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntAU[1] & evnt_time <= input$tIntAU[2])
		} else if (input.var() == "ap") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntAP[1] & evnt_time <= input$tIntAP[2])
		} else if (input.var() == "ey") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntEy[1] & evnt_time <= input$tIntEy[2])
		} else if (input.var() == "bz") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntBz[1] & evnt_time <= input$tIntBz[2])
		} else if (input.var() == "sw") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntSW[1] & evnt_time <= input$tIntSW[2])
		} else if (input.var() == "beta") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntBeta[1] & evnt_time <= input$tIntBeta[2])
		} else if (input.var() == "sunspot") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntSunSpot[1] & evnt_time <= input$tIntSunSpot[2])
		} else if (input.var() == "f107") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntF107[1] & evnt_time <= input$tIntF107[2])
		} else if (input.var() == "cme") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntCME[1] & evnt_time <= input$tIntCME[2])
		} else if (input.var() == "bn") {
			finalOutput <- dplyr::filter(dataInput(), evnt_time >= input$tIntBn[1] & evnt_time <= input$tIntBn[2])
		} else {
			req(input$file1)
			finalOutput <- dataGNR()
		}
		finalOutput
	})

	## Select the proper time span
	finalOutputSml <- reactive({
		if (input.var() == "dst") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntDst[1] & evnt_time <= input$tIntDst[2])
		} else if (input.var() == "ae") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntAE[1] & evnt_time <= input$tIntAE[2])
		} else if (input.var() == "al") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntAL[1] & evnt_time <= input$tIntAL[2])
		} else if (input.var() == "au") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntAU[1] & evnt_time <= input$tIntAU[2])
		} else if (input.var() == "ap") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntAP[1] & evnt_time <= input$tIntAP[2])
		} else if (input.var() == "ey") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntEy[1] & evnt_time <= input$tIntEy[2])
		} else if (input.var() == "bz") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntBz[1] & evnt_time <= input$tIntBz[2])
		} else if (input.var() == "sw") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntSW[1] & evnt_time <= input$tIntSW[2])
		} else if (input.var() == "beta") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntBeta[1] & evnt_time <= input$tIntBeta[2])
		} else if (input.var() == "sunspot") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntSunSpot[1] & evnt_time <= input$tIntSunSpot[2])
		} else if (input.var() == "f107") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntF107[1] & evnt_time <= input$tIntF107[2])
		} else if (input.var() == "cme") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntCME[1] & evnt_time <= input$tIntCME[2])
		} else if (input.var() == "bn") {
			finalOutputSml <- dplyr::filter(dataInputSml(), evnt_time >= input$tIntBn[1] & evnt_time <= input$tIntBn[2])
		} else {
			req(input$file1)
			finalOutputSml <- dataGNR()
		}
		finalOutputSml
	})


	evntMax <- reactive({
		if (input.var() == "dst") {
			evntMax = -100
		} else if (input.var() == "ae") {
			evntMax = 1000
		} else if (input.var() == "al") {
			evntMax = -600
		} else if (input.var() == "au") {
			evntMax = 400
		} else if (input.var() == "ap") {
			evntMax = 100
		} else if (input.var() == "ey") {
			evntMax = 8
		} else if (input.var() == "bz") {
			evntMax = -10
		} else if (input.var() == "sw") {
			evntMax = 600
		} else if (input.var() == "beta") {
			evntMax = 20
		} else if (input.var() == "sunspot") {
			evntMax = 150
		} else if (input.var() == "f107") {
			evntMax = 100
		} else if (input.var() == "bn") {
			evntMax = -10
		} else {
			evntMax = NULL
		}
		evntMax
	})

	evntThreshold <- reactive({
		if (input.var() == "dst") {
			evntThreshold = -20
		} else if (input.var() == "ae") {
			evntThreshold = 200
		} else if (input.var() == "al") {
			evntThreshold = -100
		} else if (input.var() == "au") {
			evntThreshold = 100
		} else if (input.var() == "ap") {
			evntThreshold = 20
		} else if (input.var() == "ey") {
			evntThreshold = 4
		} else if (input.var() == "bz") {
			evntThreshold = -2
		} else if (input.var() == "sw") {
			evntThreshold = 400
		} else if (input.var() == "beta") {
			evntThreshold = 5
		} else if (input.var() == "sunspot") {
			evntThreshold = 100
		} else if (input.var() == "f107") {
			evntThreshold = 100
		} else if (input.var() == "bn") {
			evntThreshold = -2
		} else {
			evntThreshold = NULL
		}
		evntThreshold
	})

	data <- reactive({
		if (input.var() == "cme" || input.var() == "gnr") {
			data = finalOutput()
		} else {
			data = calc_storms(dataRaw = finalOutput(), evntMax = evntMax(), evntThreshold = evntThreshold())
		}

		data
	})

	## For CMEs generate also a data set where speed is > 700 km/s
	startCME = 700
	dataCME <- reactive({
		if (input.var() == "cme") {
			dataCME <- dplyr::filter(finalOutput(), evnt >= startCME)
		}
	})

	continous <- reactive({
		TRUE
	})

	data_estimates <- reactive({
		data_estimates <- get_estimates(abs(data()$evnt), continous = continous())
		data_estimates
	})

	xmin <- reactive({
		if (input.var() == "dst") {
			xmin = as.numeric(input$xminDst)
		} else if (input.var() == "ae") {
			xmin = as.numeric(input$xminAE)
		} else if (input.var() == "al") {
			xmin = as.numeric(input$xminAL)
		} else if (input.var() == "au") {
			xmin = as.numeric(input$xminAU)
		} else if (input.var() == "ap") {
			xmin = as.numeric(input$xminAP)
		} else if (input.var() == "ey") {
			xmin = as.numeric(input$xminEy)
		} else if (input.var() == "bz") {
			xmin = as.numeric(input$xminBz)
		} else if (input.var() == "sw") {
			xmin = as.numeric(input$xminSW)
		} else if (input.var() == "beta") {
			xmin = as.numeric(input$xminBeta)
		} else if (input.var() == "sunspot") {
			xmin = as.numeric(input$xminSunSpot)
		} else if (input.var() == "f107") {
			xmin = as.numeric(input$xminF107)
		} else if (input.var() == "cme") {
			xmin = as.numeric(input$xminCME)
		} else if (input.var() == "bn") {
			xmin = as.numeric(input$xminBn)
		} else {
			xmin = 1
		}
		xmin
	})

	threshold <- reactive({
		if (input$tailmethod == "2") {
			threshold = xmin()
		} else {
			threshold = round(data_estimates()$estimates[2])
		}
		threshold
	})

	evntPOT <- reactive({
		if (input.var() == "dst") {
			evntPOT = -20
		} else if (input.var() == "ae") {
			evntPOT = 200
		} else if (input.var() == "al") {
			evntPOT = -100
		} else if (input.var() == "au") {
			evntPOT = 100
		} else if (input.var() == "ap") {
			evntPOT = 20
		} else if (input.var() == "ey") {
			evntPOT = 4
		} else if (input.var() == "bz") {
			evntPOT = -2
		} else if (input.var() == "sw") {
			evntPOT = 400
		} else if (input.var() == "beta") {
			evntPOT = 5
		} else if (input.var() == "sunspot") {
			evntPOT = 100
		} else if (input.var() == "f107") {
			evntPOT = 100
		} else if (input.var() == "cme") {
			evntPOT = 100
		} else if (input.var() == "bn") {
			evntPOT = -2
		} else {
			evntPOT = mean(finalOutput()$evnt)
		}
		evntPOT
	})

	# For Raw Data plot
	df = reactive({
		reshape::melt(finalOutputSml(), id.var = "evnt_time")
	})

	# For Processed Data/Storms Plot
	dfc = reactive({
		if (input.var() == "cme") {
			dfc <- reshape::melt(dataCME(), id.var = "evnt_time")
		} else if (input.var() == "gnr") {
			req(input$file1)
			tmp <- dplyr::filter(finalOutput(), evnt >= median(evnt, na.rm = TRUE))
			dfc <- reshape::melt(tmp, id.var = "evnt_time")
		} else {
			dfc <- reshape::melt(data(), id.var = "evnt_time")
		}
		dfc
	})

	## POT analysis - returns the return levels with confidence intervals
	
	dataPOT <- reactive({
		if (input.var() != "gnr") {
			dataPOT <- calc_storms(dataRaw = finalOutput(), evntMax = evntPOT(), evntThreshold = evntPOT())
		} else {
			dataPOT <- finalOutput()
		}
	})

	fitGP <- reactive({
		dst_extRemes(data = dataPOT(), threshold = threshold())
	})


	rlpoints <- reactive({
		getrlpoints(fitGP())
	})

	ci_df <- reactive({
		getcidf(fitGP())
	})

	## End of POT Analysis
	
	## Start Vuong's Statistic Test
	compDist <- reactive({
		compDistributions(data = abs(data()$evnt), continous = continous())
	})

	comp_pl_ln <- reactive({
		comp_pl_ln <- compDist()$comp_pl_ln
	})

	comp_pl_exp <- reactive({
		compDist()$comp_pl_exp
	})

	comp_ln_exp <- reactive({
		compDist()$comp_ln_exp
	})

	## End of Vuong's Statistic Test
	
	# Generate plots of the data ----
	# Also uses the inputs to build the plot label. Note that the
# dependencies on the inputs and the data reactive expression are
# both tracked, and all expressions are called in the sequence
# implied by the dependency graph.

	ytit1 <- reactive({
		if (input.var() == "dst") {
			ytit1 = "-Dst [nT]"
		} else if (input.var() == "ae") {
			ytit1 = "AE [nT]"
		} else if (input.var() == "al") {
			ytit1 = "-AL [nT]"
		} else if (input.var() == "au") {
			ytit1 = "AU [nT]"
		} else if (input.var() == "ap") {
			ytit1 = "AP [nT]"
		} else if (input.var() == "ey") {
			ytit1 = "Ey Electric Field mV/m"
		} else if (input.var() == "bz") {
			ytit1 = "-Bz (GSM) [nT]"
		} else if (input.var() == "sw") {
			ytit1 = "Solar Wind Speed [km/sec]"
		} else if (input.var() == "beta") {
			ytit1 = "Plasma Beta"
		} else if (input.var() == "sunspot") {
			ytit1 = "Sunspot Number [new version]"
		} else if (input.var() == "f107") {
			ytit1 = "Solar Index F10.7"
		} else if (input.var() == "cme") {
			ytit1 = "CME Speed [km/sec]"
		} else if (input.var() == "bn") {
			ytit1 = "IMF |Bn| [nT]"
		} else {
			ytit1 = "Magnitude"
		}
		ytit1
	})

	ytit11 <- reactive({
		if (input.var() == "dst") {
			ytit11 = "-Dst > 100 [nT] "
		} else if (input.var() == "ae") {
			ytit11 = "AE > 1000 [nT]"
		} else if (input.var() == "al") {
			ytit11 = "-AL > 600 [nT]"
		} else if (input.var() == "au") {
			ytit11 = "AU > 400 [nT]"
		} else if (input.var() == "ap") {
			ytit11 = "AP > 100 [nT]"
		} else if (input.var() == "ey") {
			ytit11 = "Ey Electric Field > 8 [mV/m]"
		} else if (input.var() == "bz") {
			ytit11 = "-Bz > 10 [nT]"
		} else if (input.var() == "sw") {
			ytit11 = "Solar Wind Speed > 600 [km/sec]"
		} else if (input.var() == "beta") {
			ytit11 = "Plasma Beta > 20"
		} else if (input.var() == "sunspot") {
			ytit11 = "Sunspot Number (new version) > 150"
		} else if (input.var() == "f107") {
			ytit11 = "Solar Index F10.7 > 100"
		} else if (input.var() == "cme") {
			ytit11 = "CME Speed > 700 [km/sec]"
		} else if (input.var() == "bn") {
			ytit11 = "IMF |Bn| > 10 [nT]"
		} else {
			ytit11 = "Magnitude > Median Magnitude"
		}
		ytit11
	})


	ytit2 = "P(X>xmax)"

	mtit <- reactive({
		if (input.var() == "dst") {
			mtit = paste0("Storms where Dst <", -threshold(), " nT")
		} else if (input.var() == "ae") {
			mtit = paste0("Storms where AE >", threshold(), " nT")
		} else if (input.var() == "al") {
			mtit = paste0("Storms where AL <", -threshold(), " nT")
		} else if (input.var() == "au") {
			mtit = paste0("Storms where AU >", threshold(), " nT")
		} else if (input.var() == "ap") {
			mtit = paste0("Storms where AP >", threshold(), " nT")
		} else if (input.var() == "ey") {
			mtit = paste0("Storms where Ey >", threshold(), " nT")
		} else if (input.var() == "bz") {
			mtit = paste0("Storms where Bz <", -threshold(), " nT")
		} else if (input.var() == "sw") {
			mtit = paste0("Storms where Solar Wind Speed >", threshold(), " nT")
		} else if (input.var() == "beta") {
			mtit = paste0("Storms where Plasma Beta >", threshold(), " nT")
		} else if (input.var() == "sunspot") {
			mtit = paste0("Storms where Sunspot Number (new version) >", threshold(), " nT")
		} else if (input.var() == "f107") {
			mtit = paste0("Storms where Solar Index F10.7 >", threshold(), " nT")
		} else if (input.var() == "cme") {
			mtit = paste0("Storms where CME Speed >", threshold(), " km/s")
		} else if (input.var() == "bn") {
			mtit = paste0("Storms where Bn <", -threshold(), " nT")
		} else {
			mtit = paste0("Storms where Magnitude >", threshold())
		}
		mtit
	})

	## Time Span in Years
	tSpan = reactive({
		(as.numeric(data()$evnt_time[length(data()$evnt_time)]) - as.numeric(data()$evnt_time[1]))/(86400 * 365)
	})

	date_breaks <- reactive({
		nyears <- tSpan()
		if (nyears >= 50) {
			date_breaks <- "10 years"
		} else if (nyears >= 10 & nyears < 50) {
			date_breaks <- "2 years"
		} else {
			date_breaks = "1 year"
		}
		date_breaks
	})


	pl1 <- reactive({
		if (is.null(df())) 
			return(NULL)
		pl1 <- ggplot(data = df(), aes(x = evnt_time, xend = evnt_time, y = 0, yend = abs(value), group = variable, color = variable)) + ggtitle(paste0("Raw ", input.var.name(), " Data ")) + theme(legend.position = "none") + 
			scale_x_datetime(name = "Year", date_breaks = date_breaks(), date_labels = "%Y") + scale_y_continuous(name = ytit1()) + geom_segment(colour = "darkblue")
		pl1
	})

	pl2 <- reactive({
		if (is.null(dfc())) 
			return(NULL)
		pl2 <- ggplot(data = dfc(), aes(x = evnt_time, xend = evnt_time, y = 0, yend = abs(value), group = variable, color = variable)) + geom_segment() + ggtitle(paste0("Selected ", input.var.name(), 
			" Data")) + theme(legend.position = "none") + scale_x_datetime(name = "Year", date_breaks = date_breaks(), date_labels = "%Y") + scale_y_continuous(name = ytit11())
		pl2
	})

	output$Plot1 <- renderPlotly({
		if (is.null(pl1())) 
			return(NULL)
		pl1()

	})

	output$Plot2 <- renderPlotly({
		if (is.null(pl2())) 
			return(NULL)

		pl2()
	})

	xaxmin <- reactive({
		if (input.var() == "dst") {
			xaxmin = 100
		} else if (input.var() == "ae") {
			xaxmin = 1000
		} else if (input.var() == "al") {
			xaxmin = 750
		} else if (input.var() == "au") {
			xaxmin = 385
		} else if (input.var() == "ap") {
			xaxmin = 100
		} else if (input.var() == "ey") {
			xaxmin = 8
		} else if (input.var() == "bz") {
			xaxmin = 12
		} else if (input.var() == "sw") {
			xaxmin = 600
		} else if (input.var() == "ae") {
			xaxmin = 1000
		} else if (input.var() == "beta") {
			xaxmin = 30
		} else if (input.var() == "sunspot") {
			xaxmin = 130
		} else if (input.var() == "f107") {
			xaxmin = 100
		} else if (input.var() == "cme") {
			xaxmin = 500
		} else {
			xaxmin = round(threshold() * 0.9)
		}
		xaxmin
	})

	yaxmax = 1
	boot = 200
	bootShow = 10

	inp.xcrit <- reactive({
		if (input$xcrit == "" || input$xcrit <= 0) {
			inp.xcrit = NULL
		} else {
			inp.xcrit = as.numeric(input$xcrit)
		}
	})

	dt <- reactive({
		if (input$dt == "" || input$dt <= 0) {
			dt = 10
		} else {
			dt = as.numeric(input$dt)
		}
	})


	clauset.list <- reactive({
		clauset(data_estimates(), boot = boot, bootShow = bootShow, xcrit = inp.xcrit(), xmin = NULL, xaxmin = NULL, yaxmax = yaxmax, dt = dt(), tSpan = tSpan(), continous = continous())
	})

	df_emp_ccdf <- reactive({
		clauset.list()$df_emp_ccdf
	})

	list_pl_ccdf <- reactive({
		clauset.list()$pl_ccdf
	})

	list_ln_ccdf <- reactive({
		clauset.list()$ln_ccdf
	})

	list_exp_ccdf <- reactive({
		clauset.list()$exp_ccdf
	})

	pHat_df <- reactive({
		clauset.list()$pHat_df
	})

	xcrit <- reactive({
		clauset.list()$xcrit
	})

	pEventXyears_df <- reactive({
		clauset.list()$pEventXyears
	})

	p1.clauset <- reactive({
		pl <- ggplot() + scale_y_continuous(name = ytit2, trans = "log", limits = c(min(df_emp_ccdf()$y), yaxmax), breaks = c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)) + scale_x_continuous(name = ytit1(), 
			trans = "log", limits = c(xaxmin(), to = max(df_emp_ccdf()$x)), breaks = round(seq(from = xaxmin(), to = max(df_emp_ccdf()$x), length.out = 5), digits = 0)) + ggtitle(mtit())
		for (i in 1:bootShow) {
			pl <- pl + geom_line(data = list_exp_ccdf()[[i]], aes(x = value, y = p_x, colour = "Exponential", alpha = "Exponential"))
			pl <- pl + geom_line(data = list_ln_ccdf()[[i]], aes(x = value, y = p_x, colour = "Log-Normal", alpha = "Log-Normal"))
			pl <- pl + geom_line(data = list_pl_ccdf()[[i]], aes(x = value, y = p_x, colour = "Power-Law", alpha = "Power-Law"))
		}
		pl <- pl + geom_point(data = df_emp_ccdf(), aes(x = x, y = y, colour = "Observations"), color = "black", alpha = 0.5, size = 2)
		pl <- pl + scale_colour_manual(values = c(`Power-Law` = "red", `Log-Normal` = "blue", Exponential = "green")) + scale_alpha_manual(values = c(`Power-Law` = 0.2, `Log-Normal` = 0.2, Exponential = 0.2))
		pl <- pl + theme(legend.position = "top", legend.title = element_blank(), legend.direction = "horizontal")
		pl <- pl + geom_vline(data = NULL, xintercept = xmin(), color = "grey2", size = 1, linetype = "dashed")
		pl <- ggplotly(pl) %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.3))
	})


	p2.clauset <- reactive({
		pl <- ggplot() + geom_density(data = pHat_df(), aes(x = prob, stat(scaled), colour = model, fill = model), adjust = 1.5, alpha = 0.2) + scale_x_continuous(name = ytit2, limits = c(0, 1)) + 
			scale_y_continuous(name = "Density") + scale_colour_manual(values = c(`Power-Law` = "red", `Log-Normal` = "blue", Exponential = "green")) + scale_alpha_manual(values = c(`Power-Law` = 0.2, 
			`Log-Normal` = 0.2, Exponential = 0.2)) + scale_fill_manual(values = c(`Power-Law` = "red", `Log-Normal` = "blue", Exponential = "green")) + theme(legend.position = "none", legend.title = element_blank())

	})

	output$ClausetPlot1 <- renderPlotly({
		p1.clauset()
	})

	output$ClausetPlot2 <- renderPlotly({
		p2.clauset()
	})

	pl.compDist <- reactive({

		df.compDist <- data.frame(x = comp_pl_ln()$ratio$x, `Power-Law_vs_Log-Normal` = comp_pl_ln()$ratio[, 2], `Power-Law_vs_Exponential` = comp_pl_exp()$ratio[, 2], `Log-Normal_vs_Exponential` = comp_ln_exp()$ratio[, 
			2])

		df.compDist <- reshape::melt(df.compDist, id.var = "x")
		xCoordLab = min(comp_pl_ln()$ratio$x):1.05
		yCoordLab = max(df.compDist$value) * 0.95
		pl.compDist <- ggplot() + geom_point(data = df.compDist, aes(x = x, y = value, color = variable), alpha = 0.6) + scale_x_continuous(name = ytit1(), limits = range(comp_pl_ln()$ratio$x)) + 
			scale_y_continuous(name = "Difference in log-likelihoods") + ggtitle("Comparing Distributions - Vuong Statistic") + theme(legend.position = "top", legend.title = element_blank(), legend.background = element_rect(fill = "transparent"))
		pl.compDist <- ggplotly(pl.compDist) %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.3))
		pl.compDist
	})

	output$compDistPlot <- renderPlotly({
		pl.compDist()
	})

	pl.rl <- reactive({
		ggplot() + geom_line(data = ci_df(), aes(x = rperiods, y = c2), color = "red", lwd = 2, alpha = 0.4) + geom_line(data = ci_df(), aes(x = rperiods, y = c1), color = "darkblue", linetype = 2, 
			lwd = 2, alpha = 0.4) + geom_line(data = ci_df(), aes(x = rperiods, y = c3), color = "darkblue", linetype = 2, lwd = 2, alpha = 0.4) + geom_point(data = rlpoints(), aes(x = rlpoints.x, 
			y = rlpoints.y), size = 2, alpha = 0.8) + geom_vline(data = NULL, xintercept = 100, color = "grey", size = 2) + scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100, 200, 500), name = "Return Period [Years]", 
			limits = c(2, 500)) + scale_y_continuous(name = ytit1(), limits = range(ci_df()[c(2, 5, 10, 50, 75, 100, 200, 500), ])) #, breaks = seq(from = 200, to = 2000, by = 400)

	})

	output$POTPlot <- renderPlotly({
		pl.rl()
	})

	# Comparison of Power-Law,  Log-Normal and Exponential Distributions ----
	
	output$compareModels <- renderTable({
		pl_v_ln = c(comp_pl_ln()$test_statistic, comp_pl_ln()$p_one_sided, comp_pl_ln()$p_two_sided)

		pl_v_exp = c(comp_pl_exp()$test_statistic, comp_pl_exp()$p_one_sided, comp_pl_exp()$p_two_sided)

		ln_v_exp = c(comp_ln_exp()$test_statistic, comp_ln_exp()$p_one_sided, comp_ln_exp()$p_two_sided)
		data <- data.frame(cbind(pl_v_ln, pl_v_exp, ln_v_exp), row.names = c("R<sub>V</sub>", "p<sub>1</sub>", "p<sub>2</sub>"))
		colnames(data) <- c("PL-LN", "PW-Exp", "LN-Exp")
		data
	}, rownames = T, colnames = F, digits = 4, sanitize.text.function = function(x) x)


	output$summaryPOT <- renderTable(({
		df = ci_df()
		df = df[, c(4, 2, 1, 3)]

		#select the return period we want
		df <- df[df$rperiods >= 5 & df$rperiods <= 1000, ]
		colnames(df) <- c("Return Period [Years]", "Median", "2.5% CI", "97.5% CI")
		#change to integers for output table
		df <- apply(df, 2, as.integer)

	}))

	output$pEventXyears <- renderTable({
		df <- pEventXyears_df()
		colnames(df) <- c("Model", "Median (%)", "2.5% (%)", "97.5% (%)")
		df

	})

	output$headerTableEvent <- renderText({
		if (input.var() == "dst") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", "|", str_to_title(input$var), "|", "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " nT. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "ae" || input.var() == "au" || input.var() == "ap") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", str_to_upper(input$var), "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " nT. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "al") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", "|", str_to_upper(input$var), "|", "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " nT. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "ey") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", str_to_title(input$var), "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " mV/m. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "sw") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of Solar Wind Speed", "<br/>", " Exceeding ", format(round(xcrit(), digits = 1), 
				scientific = TRUE), " km/sec. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "beta") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of Plasma Beta ", "<br/>", " Exceeding ", format(round(xcrit(), digits = 1), 
				scientific = TRUE), ". Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "sunspot") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of Sunspot Number (new version)", "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), ". Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "f107") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of Solar Index F10.7", "<br/>", " Exceeding ", format(round(xcrit(), digits = 1), 
				scientific = TRUE), ". Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "cme") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", str_to_upper(input$var), "<br/>", " Speed Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " km/sec. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "bn" || input.var() == "bz") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", "|", str_to_title(input$var), "|", "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), " nT. Assuming Power Law, Log-Normal and Exponential Distributions.")
		} else if (input.var() == "gnr") {
			paste("<b>", "Table 1:", "</b>", "Estimates and Confidence Intervals for ", dt(), " Year Probabilistic Forecasts of", "<i>", "User Provided Data", "</i>", "<br/>", " Exceeding ", format(round(xcrit(), 
				digits = 1), scientific = TRUE), ". Assuming Power Law, Log-Normal and Exponential Distributions.")
		}
	})

	output$headerTablePOT <- renderText({
		paste0("<b>", "Table 2:", "</b>", " Point Over Threshold Analysis Results.", "<br/>", "Median and 95% CI  are shown for increasing return periods (in years).")
	})

	output$headerTableEvents <- renderText({
		paste0("<b>", "List of Events", "<b>")
	})

	evntTable <- reactive({
		data <- data.frame(Time = as.character(data()$evnt_time), Magnitude = data()$evnt)
	})

	# Generate an HTML table view of the data ----
	output$table <- renderDataTable({
		DT::datatable(evntTable(), options = list(pageLength = 2100))
	})

	output$headerCompareModels <- renderText({
		paste0("<b>", "Table 3:", "</b>", " Vuong's test statistic and one- and two-sided ", "<i>", "p", "</i>", " for: Power-Law vs. Log-Normal (left column),", "<br/>", " Power-Law vs. Exponential (middle column) and Log-Normal vs. Exponential distribution  (right column). )")
	})

	output$footnoteCompareModels <- renderText({
		paste0("<b>", "Background: ", "</b>", "The Vuong's test statistic ", "R", "<sub>", "V", " compares two models under the hypothesis that both distributions are equally far from the true\n          distribution. If true, the log-likelihood ratio will have a mean value of zero.  If the first (or second) models are significantly\n          better than the other ", 
			"<i>", "R", "<sub>", "V", "</sub>", "</i>", " will move toward plus (or minus) infinity.", "<br/>", "The one-sided and\n          two-sided ", "<i>", "p", "</i>", " values estimate the significance of the ", 
			"<i>", "R", "<sub>", "V", "</sub>", "</i>", " statistic.\n          The one-sided approach tests the null hypothesis that both distributions are equally far from the true distribution against the\n          hypothesis that model A is closer to the true distribution. The two sided version tests the same null hypothesis\n           against the alternative that one of the distributions is closer.  The null hypothesis is rejected if ", 
			"<i>", "p", "</i>", " < 0.05.")
	})

}
