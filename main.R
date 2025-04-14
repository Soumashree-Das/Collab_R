# main.R
# Load required utilities and modules
source("utils.R")
source("ui.R")
source("server.R")
source("preprocessing.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)