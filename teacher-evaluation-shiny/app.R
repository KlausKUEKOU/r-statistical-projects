# Contenu du fichier app.R
library(shiny)

# Charger les fichiers UI et server
source("ui.R")
source("server.R")

# Créer l'application Shiny
shinyApp(ui = ui, server = server)