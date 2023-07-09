# Set options here
setwd("/mnt/d/Repository_ubuntu/Housekeeping/HRT_Atlas/hrt.v1.1")
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.autoreload = TRUE)

# Comment this if you don't want the app to be served on a random port
#options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application # To be redefined before deployment
opt = list(
  host = "0.0.0.0",
  port = 1985
)


run_app(options = opt)
