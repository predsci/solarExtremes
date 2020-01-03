#' Shiny app server object
#'
#' @import From ggplot
#' @import shiny

# create the shiny application user interface

shinyAppUI <- fluidPage(# App title ----
                tags$hr(),
                titlePanel("Estimating Space Weather Benchmarks"),
                tags$hr(),
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(

                    # Input: Select the random distribution type ----
                    selectInput(
                    inputId = "var",
                    label = "Select Data Type",
                    list("Geomagnetic Indices" = c("Dst" = 'dst', 'AE' = 'ae', "AL" = 'al', "AU" = 'au', "AP" = 'ap', 'Electric Field (Ey)' = 'ey'),
                    "Interplanetary Indices" = c("CME" = 'cme', "IMF Bn" = "bn", "Bz (GSM)" = "bz", "Solar Wind Speed" = 'sw', "Plasma Beta" = 'beta'),
                    "Solar Indices" = c("Sunspot Numbers "="sunspot", "F10.7" = 'f107'),
                    "User Data" = c('gnr')), selected = 'dst'),
                    tags$hr(),
                    conditionalPanel(
                      condition = "input.var == 'dst'",
                    dateRangeInput(inputId = "tIntDst",
                                   label = "Date range",
                                   start = "1957-01-01 00:00:00 GMT",
                                   end   = "2019-12-31 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'ae'",
                      dateRangeInput(inputId = "tIntAE",
                                     label = "Date range",
                                     start = "1964-1-01 00:00:00 GMT",
                                     end   = "2018-02-28 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'al'",
                      dateRangeInput(inputId = "tIntAL",
                                     label = "Date range",
                                     start = "1966-1-01 00:00:00 GMT",
                                     end   = "2018-02-28 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'au'",
                      dateRangeInput(inputId = "tIntAU",
                                     label = "Date range",
                                     start = "1966-1-01 00:00:00 GMT",
                                     end   = "2018-02-28 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'ap'",
                      dateRangeInput(inputId = "tIntAP",
                                     label = "Date range",
                                     start = "1964-1-01 00:00:00 GMT",
                                     end   = "2018-02-28 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'ey'",
                      dateRangeInput(inputId = "tIntEy",
                                     label = "Date range",
                                     start = "1964-1-01 17:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),
                   conditionalPanel(
                      condition = "input.var == 'bz'",
                      dateRangeInput(inputId = "tIntBz",
                                     label = "Date range",
                                     start = "1963-11-28 00:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'sw'",
                      dateRangeInput(inputId = "tIntSW",
                                     label = "Date range",
                                     start = "1964-01-01 12:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),
                     conditionalPanel(
                      condition = "input.var == 'beta'",
                      dateRangeInput(inputId = "tIntBeta",
                                     label = "Date range",
                                     start = "1964-1-01 00:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),
                   conditionalPanel(
                      condition = "input.var == 'sunspot'",
                      dateRangeInput(inputId = "tIntSunSpot",
                                     label = "Date range",
                                     start = "1964-01-01 00:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'f107'",
                      dateRangeInput(inputId = "tIntF107",
                                     label = "Date range",
                                     start = "1964-1-02 00:00:00 GMT",
                                     end   = "2019-01-30 23:00:00 GMT")),

                    conditionalPanel(
                      condition = "input.var == 'cme'",
                      dateRangeInput(inputId = "tIntCME",
                                     label = "Date range",
                                     start = "1996-01-11 00:14:36 GMT",
                                     end   = "2018-12-31 09:48:05 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'bn'",
                      dateRangeInput(inputId = "tIntBn",
                                     label = "Date range",
                                     start = "1963-11-27 19:00:00 GMT",
                                     end   = "2019-11-05 14:00:00 GMT")),
                    conditionalPanel(
                      condition = "input.var == 'gnr'",
                      fileInput(inputId = "file1",
                                     label = "Choose a CSV file",
                                     placeholder = 'Select File..Click Submit Button',
                                     accept = c("text/csv", "text/comma=separated values, text/plain",".csv"))),
                    tags$hr(),

                    radioButtons(
                      inputId = 'tailmethod',
                      label = 'Set Tail of Distribution',
                      choices = list('Kolmogorov-Smirnov Goodness of Fit' = 1,
                                     "Manually" = 2), selected = 1
                    ),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'dst'",
                      sliderInput(inputId = 'xminDst',
                                  label = 'Tail Value',
                                  min = 80, max = 140, step = 5, value = 110)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'ae'",
                      sliderInput(inputId = 'xminAE',
                                  label = 'Tail Value',
                                  min = 700, max = 1500, step = 50, value = 1150)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'al'",
                      sliderInput(inputId = 'xminAL',
                                  label = 'Tail Value',
                                  min = 400, max = 800, step = 25, value = 600)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'au'",
                      sliderInput(inputId = 'xminAU',
                                  label = 'Tail Value',
                                  min = 300, max = 500, step = 10, value = 400)),
                     conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'ap'",
                      sliderInput(inputId = 'xminAP',
                                  label = 'Tail Value',
                                  min = 60, max = 150, step = 10, value = 110)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'ey'",
                      sliderInput(inputId = 'xminEy',
                                  label = 'Tail Value',
                                  min = 6., max = 12, step = 0.5, value = 8.5)),
                   conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'bz'",
                      sliderInput(inputId = 'xminBz',
                                  label = 'Tail Value',
                                  min = 8., max = 20, step = 1., value = 10)),
                   conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'sw'",
                      sliderInput(inputId = 'xminSW',
                                  label = 'Tail Value',
                                  min = 400., max = 900., step = 25., value = 650)),
                   conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'beta'",
                      sliderInput(inputId = 'xminBeta',
                                  label = 'Tail Value',
                                  min = 25., max = 50, step = 1., value = 38)),
                   conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'sunspot'",
                      sliderInput(inputId = 'xminSunSpot',
                                  label = 'Tail Value',
                                  min = 100, max = 200, step = 5., value = 150)),
                   conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'f107'",
                      sliderInput(inputId = 'xminF107',
                                  label = 'Tail Value',
                                  min = 80., max = 150, step = 5., value = 110)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'cme'",
                      sliderInput(inputId = 'xminCME',
                                  label = 'Tail Value',
                                  min = 1000, max = 2000, step = 50, value = 1500)),
                    conditionalPanel(
                      condition = "input.tailmethod == '2' && input.var == 'bn'",
                      sliderInput(inputId = 'xminBn',
                                  label = 'Tail Value',
                                  min = 8., max = 20, step = 1, value = 10)),
                    helpText("Resetting the Tail of Distribution",
                             " affects the Models and POT Tabs"),
                    tags$hr(),
                     textInput("xcrit", label = "Abs. Magnitude of Critical Event:",
                               placeholder = 'Default is maximum value in dataset.', value = ""),
                    textInput('dt', label ="Time (years) for Critical Event",
                              placeholder =" Enter Time for Critical Event to Occur:",value = 10),
                    helpText("Resetting the Critical Event and Time ",
                             "only affects the Models Tab"),
                    submitButton("Submit", icon("refresh")),
                    verbatimTextOutput("value")
                    #verbatimTextOutput("value")
                    ),


                  # Main panel for displaying outputs ----
                  mainPanel(# Output: Tabset w/ plot, summary, and table ----
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                "Plot",
                                tags$hr(),
                                plotlyOutput("Plot1", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1"),
                                plotlyOutput("Plot2", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1")
                              ),
                              tabPanel(
                                "Models",
                                br(),
                                br(),
                                plotlyOutput("ClausetPlot1", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1"),
                                br(),
                                plotlyOutput("ClausetPlot2", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1"),
                                br(),
                                br(),
                                htmlOutput("headerTableEvent") %>% withSpinner(color ="#0dc5c1"),
                                br(),
                                br(),
                                tableOutput("pEventXyears") %>% withSpinner(color ="#0dc5c1")
                              ),
                              tabPanel("Model Comparison",
                              br(),
                              br(),
                              plotlyOutput("compDistPlot", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1"),
                              br(),
                              htmlOutput("headerCompareModels") %>% withSpinner(color ="#0dc5c1"),
                              br(),
                              tableOutput("compareModels") %>% withSpinner(color ="#0dc5c1"),
                              br(),
                              htmlOutput("footnoteCompareModels")
                              ),
                              tabPanel(
                                "POT",
                                br(),
                                br(),
                                plotlyOutput("POTPlot", height = "auto", width = "auto") %>% withSpinner(color ="#0dc5c1"),
                                br(),
                                htmlOutput("headerTablePOT"),
                                br(),
                                tableOutput("summaryPOT") %>% withSpinner(color ="#0dc5c1")
                              ),

                              tabPanel("About",
                              br(),
                              h3("Background"),
                              tags$p("Space weather refers to the state and future conditions of the space environment surrounding the Earth, including within the Earth's magentosphere. Extreme Space Weather refers to conditons that are so far removed from the norm, that they are rare.  This page provides a prototype tool for estimating the probability of an extreme solar event together with the uncertainties associated with that estimate."),
                              br(),
                              h3("Methods"),
                              tags$p("Below we briefly describe what each tab does. For more details about the models see our ",tags$a("2012",target="_blank",href=system.file("www/riley12a.pdf", package="solarExtremes")),",", tags$a("2017",target="_blank",href="rileyLove17a.pdf"), "and ", tags$a("2018",target="_blank",href="riley18a.pdf"), " manuscripts. For the Peak Over Threshold (POT) method we use the ",tags$a("extRemes 2.0",target="_blank",href="https://www.jstatsoft.org/article/view/v072i08") , " R package."),
                              br(),
							  tags$ul(tags$li("In the ", tags$b("Model"), " tab, we use three distributions: power-law, log-normal and exponential to fit the tail of the distibution of four different space weather benchmarks: the geomagnetic indices Dst and AE, CME speeds, and the north-south interplanetary magnetic field component, Bn.  For each disribution we estimate the best model parameters and generate an ensemble of possible realizations using nonparametric bootstrapping to compute the uncertainty associated with a particular forecast."),
							  	tags$li(" In the", tags$b("Compare Models"), " tab, we use the Vuong's test which relies on a likelihood ratio test to see if one model is more favorable than another.  We compare the Power-Law model to the Log-Normal and Exponentail models, and the Log-Normal model to the Expnential model."),
							  	tags$li("The ", tags$b("POT"), " (Peak Over Threshold) tab uses a Generalized Pareto distibution to fit the values which exceed a certain threshold. The threshold is set either manually or by minimizing the Kolmogotov-Smirnov (KS) goodness-of-fit statistic. The same threshold is used in the Models and POT tabs."),
							  	tags$li("The ", tags$b("Events Table"), "tab provides a searchable table of events.")),
							  	br(),
							  	h3("Data"),
							  	tags$ul(
							  	tags$li("Dst Data is obtained from the ", tags$a(href="http://wdc.kugi.kyoto-u.ac.jp/index.html", "World Data Center for Geomagnetism, Kyoto ", target="_blank")),
							  	tags$li("AE, AL, AU, AP, Ey, Bz, Solar Wind, Plasma Beta and F10.7 data is obtained from NASA's OMNI 2 ", tags$a(href="https://omniweb.gsfc.nasa.gov/form/dx1.html", "data web server", target="_blank")),
							  	tags$li("CME data is obtained from SOHO's ", tags$a(href="https://cdaw.gsfc.nasa.gov/CME_list/","LASCO CME database", target="_blank")),
							  	tags$li("IMF Bn Data is obtained from: (a) Historic data from the early 1970's to the end of 2016 was obtained from NASA's OMNI_M dataset,
                  using the ",tags$a(href="ftp://ftp.swpc.noaa.gov/pub/lists/ace2/", "COHOWeb data server",target="_blank"), " (b) Past, 1-minute high resolution data for 2017, is obtained from NOAA's DSCOVR Space Weather  ", tags$a(href="https://www.ngdc.noaa.gov/dscovr/portal/index.html#/","Data Portal",target="_blank"), " and (c) Real time data is obtained from NOAA's Space Weather Prediction Center",
                  tags$a(href="http://services.swpc.noaa.gov/products/solar-wind/","(SWPC)",target="_blank"),". The high-resolution 1-minute datasets are averaged to 1-hour.")
							  	),
							  	br(),
							  	h3("User Uploaded Data"),
							  	tags$p("The user can upload a table of events for analysis.  Currently we only support 'csv' format.  The csv file should have columns of: year, month, day, hour, minute, second and event magnitude. Only the first (year) and last (event magnitude) columns are required. All other columns are optional.  The magnitude of all the events in the table must be positive. Please note that not all extereme event data can be modeled properly with exponential, log-normal and power-law distributions.  To upload your data:"),
							  	 tags$ul(
							  	 tags$li("Select the ", tags$b("User Data"), " button and click on the ", tags$b("Submit"), " button."),
							  	tags$li("Use the", tags$b("Browse")," window to select and upload your csv file. Once the file is loaded you should see an ", tags$b("upload complete")," bar and shortly after the data will be displayed. "),
							  	tags$li("If the data was correcltly uploaded it can be modeled as described above")),
							  	br(),
       							h3("Technology"),
        						tags$p("All the data and model analysis are done with R codes and displayed using R, shinyApp and plotly."),
        						br(),
        						br()
							  ),
                              tabPanel(
                                "Events Table",
                                tags$hr(),
                                htmlOutput("headerTableEvents"),
                                br(),
                                dataTableOutput("table", width = "70%", height = "auto")
                              ),
       				tabPanel("Contact",
                 	column(width=7,
                 	br(),
                 	br(),
                 	tags$p(tags$em("For questions about this web site please contact:")),
                 	br(),
                 	tags$p("Dr. Pete Riley: pete@predsci.com"),
                 	tags$p("Dr. M. Ben-Nun: mbennun@predsci.com"),
                 	br(),
                 	br(),
                 	includeHTML("address.html")))
                            ))
                ))
