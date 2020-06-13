
#tags$head(title = "qpcrXpress")

# put a message in console or server log; note this happens only when the app is started!
cat("HT Atlas v1.0 application started...\n")

fluidPage(useShinyjs(),
#headcode[1]
uiOutput("uihousekeeping")                                # single-output stub ui

)

