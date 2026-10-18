##################################################################################################################################################################################
################################################################ Importation des library #########################################################################################
##################################################################################################################################################################################

packages=c('shiny','shinydashboard','shinybusy','shinymanager','magrittr','plyr','questionr',
           'shinyalert','shinycssloaders','shinybrowser','shinyjs','shinyWidgets','highcharter',
           'RMySQL','DBI','rolap','dashboardthemes',"DT") 
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}


##################################################################################################################################################################################
#############################################  Connection aux bases de données et exploitation ###################################################################################
##################################################################################################################################################################################

##################################################################################################################################################################################
# Conection a la base

connect_to_database <- function() {
  dbConnect(RMySQL::MySQL(), 
            host = "localhost",
            dbname = "suivi",
            user = "root",
            password = "")
}




shinyServer(function(input, output, session) {
  
  shinyalert("WELCOME", "Bienvenue sur l'application pour l'evaluation des enseignants de l' ISSEA", type = "info")
  
  connection <- connect_to_database()
  
##################################################################################################################################################################################
  # Partie select input des box 
  observe({
    
    updateSelectInput(
      session,
      "input4",
      choices = switch(input$input3,
                       "AS" = c("AS1", "AS2", "AS3"),
                       "ISE" = c("ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3"),
                       "ISEL" = c("ISEL1", "ISEL2", "ISEL3"),
                       "TOUT CYCLE" = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3")
      )
    )
    updateSelectInput(
      session,
      "input6",
      choices = switch(input$input5,
                       "AS" = c("AS1", "AS2", "AS3"),
                       "ISE" = c("ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3"),
                       "ISEL" = c("ISEL1", "ISEL2", "ISEL3"),
                       "TOUT CYCLE" = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3")
      )
    )
    updateSelectInput(
      session,
      "input6",
      choices = switch(input$input5,
                       "AS" = c("AS1", "AS2", "AS3"),
                       "ISE" = c("ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3"),
                       "ISEL" = c("ISEL1", "ISEL2", "ISEL3"),
                       "TOUT CYCLE" = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3")
      )
    )
    
    
  })
  
  observe({
    # Vérifiez si input$CYCLE n'est pas nul
    if (!is.null(input$CYCLE)) {
      # Mettez ici votre logique pour déterminer les classes en fonction du cycle
      if (input$CYCLE == "AS") {
        classes <- c("AS1", "AS2", "AS3")
      } else if (input$CYCLE == "ISE") {
        classes <- c("ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3")
      } else if (input$CYCLE == "ISEL") {
        classes <- c("ISEL1", "ISEL2", "ISEL3")
      } else {
        classes <- c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3")
      }
      
      # Mettez à jour les choix de classe
      updateSelectInput(session, "CLASSE", choices = classes)
    }
  })
  
  observe({
    # Obtenez les valeurs sélectionnées dans les selectInput
    selected_classe1 <- input$input2
    selected_classe2 <- input$input4
    selected_classe3 <- input$input6
    
    
    # Réétablissez la connexion dans l'observe
    connection <- connect_to_database()
    
    
    # Exécution de la requête SQL et obtenez les données filtrées

    enseignants1 <- dbGetQuery(connection, "SELECT * FROM enseignant")
    
    # Calcul des indicateurs
    Nombre_ens_perm <- sum(enseignants1$TYPE == "PERMANENT") 
    Nombre_ens_as <- sum(enseignants1$TYPE == "ASSOCIE")
    Nombre_ens_vac <- sum(enseignants1$TYPE == "VACATAIRE")
    
    sql_query2 <- paste0("SELECT * FROM etudiant WHERE CLASSE = '", selected_classe2, "'")
    
    # Exécutez la requête SQL et obtenez les données filtrées
    etudiant <- dbGetQuery(connection, sql_query2)
    etudiant1 <- dbGetQuery(connection, "SELECT * FROM etudiant")
    
    Nombre_et_h <- sum(etudiant$SEXE == "MASCULIN")  # Utilisez le bon nom de colonne
    Nombre_et_f <- sum(etudiant$SEXE == "FEMININ")
    Nombre_et_ens <- Nombre_et_f + Nombre_et_h
    
    sql_query3 <- paste0("SELECT * FROM matiere WHERE CLASSE = '", selected_classe3, "'")
    
    # Exécutez la requête SQL et obtenez les données filtrées
    matiere <- dbGetQuery(connection, sql_query3)
    matiere1 <- dbGetQuery(connection, "SELECT * FROM matiere")
    
    maths1 <- sum(matiere$SEMESTRE == "SEMESTRE 1")  # Utilisez le bon nom de colonne
    maths2 <- sum(etudiant$SEMESTRE == "SEMESTRE 2")
    maths <- maths1 + maths2
    
    ##################################################################################################################################################################################
    # Partie : subitem = enseignant_statistique  
    
    output$Nombre1 <- renderValueBox({
      valueBox(
        value = Nombre_ens_perm,
        subtitle = "Nombre d'enseignants permanents",
        icon=icon("person-chalkboard"),
        color="blue")})
    
    output$Nombre_ens_asso <- renderValueBox({
      valueBox(
        value = Nombre_ens_as,
        subtitle = "Nombre total d'enseignants associé",
        icon=icon("chalkboard-user"),
        color="purple")})
    
    output$Nombre_mat <- renderValueBox({
      valueBox(
        value = Nombre_ens_vac,
        subtitle = "Nombre total d'enseignants vacataires",
        icon=icon("user"),
        color="yellow")})
    
    
    proportions <- data.frame(prop.table(table(enseignants1$NATIONALITE)) * 100)
    proportions1 <- data.frame(prop.table(table(enseignants1$SEXE)) * 100)
    
    output$gr1 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Répartition des enseignants par nationalité") %>%
        hc_subtitle(text = "Source: Notre base de données") %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.percentage:.1f}")
        )) %>%
        hc_add_series(
          name = "Nationalité",
          data = lapply(seq_len(nrow(proportions)), function(i) {
            list(
              name = proportions$Var1[i],
              y = proportions$Freq[i]
            )
          })
        )
    })
    
    output$gr2 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Répartition des enseignants par sexe") %>%
        hc_subtitle(text = "Source: Votre base de données") %>%
        hc_xAxis(categories = proportions1$Var1) %>%
        hc_yAxis(title = list(text = "Pourcentage")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%")
        )) %>%
        hc_add_series(
          name = "Sexe",
          data = lapply(seq_len(nrow(proportions1)), function(i) {
            list(
              name = proportions1$Var1[i],
              y = proportions1$Freq[i]
            )
          })
        )
    })
    
    
    output$tab1=renderDataTable(
      enseignants1%>%datatable(rownames = FALSE, 
                               extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                               selection = "none",
                               options = list(
                                 dom = "Bfrtip", 
                                 # scroll :
                                 scrollY = 500, scrollX = 400, scroller = TRUE,
                                 # fixer les colonnes : 
                                 fixedColumns = list(leftColumns = 1),
                                 # selection :
                                 select = list(style = 'os', items = 'row'),
                                 buttons = c(
                                   # enregistrements
                                   'copy', 'csv', 'excel', 'pdf', 'print'
                                   
                                 )
                               ))
      
    )
    
    ##################################################################################################################################################################################
    # Partie : subItem = etudiant_statistique 
    
    output$Nombre_et <- renderValueBox({
      valueBox(
        value = Nombre_et_ens,
        subtitle = "Nombre Total d'etudiants",
        icon=icon("users"),
        color="aqua")})
    
    output$Nombre_eth <- renderValueBox({
      valueBox(
        value = Nombre_et_h,
        subtitle = "Nombre d'hommes",
        icon=icon("male"),
        color="orange")})
    
    output$Nombre_etf <- renderValueBox({
      valueBox(
        value = Nombre_et_f,
        subtitle = "Nombre de femmes",
        icon=icon("female"),
        color="green")})
    
    proportions4 <- data.frame(prop.table(table(etudiant$NATIONALITE)) * 100)
    proportions3 <- data.frame(prop.table(table(etudiant$SEXE)) * 100)
    
    output$gr4 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Répartition des etudiantspar nationalité") %>%
        hc_subtitle(text = "Source: Notre base de données") %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.percentage:.1f}")
        )) %>%
        hc_add_series(
          name = "Nationalité",
          data = lapply(seq_len(nrow(proportions4)), function(i) {
            list(
              name = proportions4$Var1[i],
              y = proportions4$Freq[i]
            )
          })
        )
    })
    
    output$gr3 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Répartition des etudiants par sexe") %>%
        hc_subtitle(text = "Source: Votre base de données") %>%
        hc_xAxis(categories = proportions3$Var1) %>%
        hc_yAxis(title = list(text = "Pourcentage")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%")
        )) %>%
        hc_add_series(
          name = "Sexe",
          data = lapply(seq_len(nrow(proportions3)), function(i) {
            list(
              name = proportions3$Var1[i],
              y = proportions3$Freq[i]
            )
          })
        )
    })
    
    output$tab2=renderDataTable(
      etudiant1%>%datatable(rownames = FALSE, 
                            extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                            selection = "none",
                            options = list(
                              dom = "Bfrtip", 
                              # scroll :
                              scrollY = 500, scrollX = 400, scroller = TRUE,
                              # fixer les colonnes : 
                              fixedColumns = list(leftColumns = 1),
                              # selection :
                              select = list(style = 'os', items = 'row'),
                              buttons = c(
                                # enregistrements
                                'copy', 'csv', 'excel', 'pdf', 'print'
                                
                              )
                            )))
    
    
  
  ##################################################################################################################################################################################
  # Partie : subitem = matiere_statistique  
  
  output$A <- renderValueBox({
    valueBox(
      value = maths1,
      subtitle = "Nombre de matière du semestre 1",
      icon=icon("person-chalkboard"),
      color="light-blue")})
  
  output$B <- renderValueBox({
    valueBox(
      value = maths2,
      subtitle = "Nombre de matière du semestre 2",
      icon=icon("chalkboard-user"),
      color="lime")})
  
  output$C <- renderValueBox({
    valueBox(
      value = maths,
      subtitle = "Nombre total de matière",
      icon=icon("user"),
      color="teal")})
  
  
  proportions5 <- data.frame(prop.table(table(matiere$CYCLE)) * 100)
  proportions6 <- data.frame(table(matiere$CYCLE))
  
  output$graph1 <- renderHighchart({ 
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Répartition des matières par cycle") %>%
      hc_subtitle(text = "Source: Notre base de données") %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE, format = "{point.name}: {point.percentage:.1f}")
      )) %>%
      hc_add_series(
        name = "cycle",
        data = lapply(seq_len(nrow(proportions5)), function(i) {
          list(
            name = proportions5$Var1[i],
            y = proportions5$Freq[i]
          )
        })
      )
  })
  
  output$graph2 <- renderHighchart({ 
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Répartition des matières par cycle") %>%
      hc_subtitle(text = "Source: Votre base de données") %>%
      hc_xAxis(categories = proportions6$Var1) %>%
      hc_yAxis(title = list(text = "Pourcentage")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%")
      )) %>%
      hc_add_series(
        name = "Cycle",
        data = lapply(seq_len(nrow(proportions6)), function(i) {
          list(
            name = proportions6$Var1[i],
            y = proportions6$Freq[i]
          )
        })
      )
  })
  
  
  output$tableau1=renderDataTable(
    matiere%>%datatable(rownames = FALSE, 
                        extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                        selection = "none",
                        options = list(
                          dom = "Bfrtip", 
                          # scroll :
                          scrollY = 500, scrollX = 400, scroller = TRUE,
                          # fixer les colonnes : 
                          fixedColumns = list(leftColumns = 1),
                          # selection :
                          select = list(style = 'os', items = 'row'),
                          buttons = c(
                            # enregistrements
                            'copy', 'csv', 'excel', 'pdf', 'print'
                            
                          )
                        ))
    
  )
  dbDisconnect(connection)
  })
  
  ##################################################################################################################################################################################
  # Partie Reactivite fonction de l'ajout dans le formulaire : TabItem = enseignant_ajout
  
  observeEvent(input$submit_btn1, {
    
    # Construisez votre requête SQL en fonction des valeurs sélectionnées
    
    new_entry <- data.frame(
      
      NOM = as.character(input$nom),
      PRENOM = as.character(input$prenom),
      SEXE = as.character(input$sexe),
      TYPE = as.character(input$type),
      SPECIALITE1 = as.character(input$specialite1),
      SPECIALITE2 = as.character(input$specialite2),
      DATE_ENTREE = as.character(input$date_entree),
      NATIONALITE = as.character(input$nationalite)
    )
    
    connection <<- connect_to_database()
    
    tryCatch({
      # Votre code d'enregistrement ici
      dbWriteTable(connection, "enseignant", value = new_entry, append = TRUE, row.names = FALSE)
      
      # Réinitialiser les champs du formulaire
      updateTextInput(session, "nom", value = "")
      updateTextInput(session, "prenom", value = "")
      updateRadioButtons(session, "sexe", selected = "Homme")
      updateSelectInput(session, "type", selected = "PERMANENT")
      updateTextInput(session, "specialite1", value = "")
      updateTextInput(session, "specialite2", value = "")
      updateDateInput(session, "date_entre", "")
      
      # Notification de succès
      shinyalert::shinyalert(
        title = "Succès",
        text = "Enseignant enregistré avec succès!",
        type = "success"
      )
    }, error = function(e) {
      # En cas d'erreur, affichez une notification d'échec
      shinyalert::shinyalert(
        title = "Erreur",
        text = "Échec de l'enregistrement de l'étudiant. Veuillez réessayer.",
        type = "error"
      )
    })
  
    enseignants1 <- dbGetQuery(connection, "SELECT * FROM enseignant")
    
    Nombre_ens_perm=sum(enseignants1$TYPE == "PERMANENT") 
    Nombre_ens_as=sum(enseignants1$TYPE == "ASSOCIE")
    Nombre_ens_vac=sum(enseignants1$TYPE == "VACATAIRE")
    
    output$Nombre1 <- renderValueBox({
      valueBox(
        value = Nombre_ens_perm,
        subtitle = "Nombre d'enseignants permanents",
        icon=icon("person-chalkboard"),
        color="blue")})
    
    output$Nombre_ens_asso <- renderValueBox({
      valueBox(
        value = Nombre_ens_as,
        subtitle = "Nombre total d'enseignants associé",
        icon=icon("chalkboard-user"),
        color="purple")})
    
    output$Nombre_mat <- renderValueBox({
      valueBox(
        value = Nombre_ens_vac,
        subtitle = "Nombre total d'enseignants vacataires",
        icon=icon("user"),
        color="yellow")})
    
    output$tab1=renderDataTable(
      enseignants1%>%datatable(rownames = FALSE, 
                               extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                               selection = "none",
                               options = list(
                                 dom = "Bfrtip", 
                                 # scroll :
                                 scrollY = 500, scrollX = 400, scroller = TRUE,
                                 # fixer les colonnes : 
                                 fixedColumns = list(leftColumns = 1),
                                 # selection :
                                 select = list(style = 'os', items = 'row'),
                                 buttons = c(
                                   # enregistrements
                                   'copy', 'csv', 'excel', 'pdf', 'print'
                                   
                                 )
                               ))
      
    )
    
    dbDisconnect(connection)
  })
  
  
  
  
  
  ##################################################################################################################################################################################
  # Partie Reactivite fonction de l'ajout dans le formulaire : TabItem = etudiant_ajout
 
  
  # Observer pour traiter le bouton d'enregistrement
  observeEvent(input$submit_btn2, {
    
    if (input$NOM == "" || input$PRENOM == "" || input$CLASSE == "" || input$DATE_NAISSANCE == "" || input$SEXE == "" || input$MATRICULE == "" || input$DATE_ENTRE == "" || input$NATIONALITE == "") {
      # Afficher une alerte d'erreur si au moins un champ est vide
      shinyalert(
        title = "Erreur",
        text = "Veuillez remplir tous les champs avant d'enregistrer.",
        type = "error"
      )
      return(NULL)
    }
    else{
    
    selected_classe <- input$input4
    
    new_entry2 <- data.frame(
      NOM = as.character(input$NOM),
      PRENOM = as.character(input$PRENOM),
      CLASSE = as.character(input$CLASSE),
      DATE_NAISSANCE = as.character(input$DATE_NAISSANCE),
      SEXE = as.character(input$SEXE),
      MATRICULE = as.character(input$MATRICULE),
      DATE_ENTRE = as.character(input$DATE_ENTRE),
      NATIONALITE = as.character(input$NATIONALITE)
    )
    
    connection <- connect_to_database()
    
    tryCatch({
      # Votre code d'enregistrement ici
      dbWriteTable(connection, "etudiant", value = new_entry2, append = TRUE, row.names = FALSE)
      
      # Réinitialiser les champs du formulaire
      updateTextInput(session, "NOM", value = "")
      updateTextInput(session, "PRENOM", value = "")
      updateSelectInput(session, "CLASSE", selected  = "AS1")
      updateDateInput(session, "DATE_NAISSANCE", value = "2000-01-01")
      updateRadioButtons(session, "SEXE", selected = "MASCULIN")
      updateTextInput(session, "MATRICULE", value = "")
      updateDateInput(session, "DATE_ENTRE", value = "2023-09-25")
      updateSelectInput(session, "NATIONALITE", selected = "CAMEROUNAISE")
      
      # Notification de succès
      shinyalert::shinyalert(
        title = "Succès",
        text = "Étudiant enregistré avec succès!",
        type = "success"
      )
    }, error = function(e) {
      # En cas d'erreur, affichez une notification d'échec
      shinyalert::shinyalert(
        title = "Erreur",
        text = "Échec de l'enregistrement de l'étudiant. Veuillez réessayer.",
        type = "error"
      )
    })
    
    sql_query <- paste0("SELECT * FROM etudiant WHERE CLASSE = '", selected_classe, "'")
    
    # Exécutez la requête SQL et obtenez les données filtrées
    etudiant <- dbGetQuery(connection, sql_query)
    etudiant1 <- dbGetQuery(connection, "SELECT * FROM etudiant")
    
    Nombre_et_h=sum(etudiant$SEXE == "MASCULIN")
    Nombre_et_f=sum(etudiant$SEXE == "FEMININ")
    Nombre_et_ens=Nombre_et_f+Nombre_et_h
    
    ##################################################################################################################################################################################
    # Partie valueBox 
    
    output$Nombre_et <- renderValueBox({
      valueBox(
        value = Nombre_et_ens,
        subtitle = "Nombre Total d'etudiants",
        icon=icon("users"),
        color="aqua")})
    
    output$Nombre_eth <- renderValueBox({
      valueBox(
        value = Nombre_et_h,
        subtitle = "Nombre d'hommes",
        icon=icon("male"),
        color="orange")})
    
    output$Nombre_etf <- renderValueBox({
      valueBox(
        value = Nombre_et_f,
        subtitle = "Nombre de femmes",
        icon=icon("female"),
        color="green")})
    
    
    ##################################################################################################################################################################################
    # Partie Base de données etudiants   
    
    output$tab2=renderDataTable(
      etudiant1%>%datatable(rownames = FALSE, 
                            extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                            selection = "none",
                            options = list(
                              dom = "Bfrtip", 
                              # scroll :
                              scrollY = 500, scrollX = 400, scroller = TRUE,
                              # fixer les colonnes : 
                              fixedColumns = list(leftColumns = 1),
                              # selection :
                              select = list(style = 'os', items = 'row'),
                              buttons = c(
                                # enregistrements
                                'copy', 'csv', 'excel', 'pdf', 'print'
                                
                              )
                            )))
    
    dbDisconnect(connection)}
  })
  
  
  
  ##################################################################################################################################################################################
  # Partie Base de données Évaluation : TabItem = Stat1 
  
  output$tab3=renderDataTable(
    questionnaire%>%datatable(rownames = FALSE, 
                              extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                              selection = "none",
                              options = list(
                                dom = "Bfrtip", 
                                # scroll :
                                scrollY = 500, scrollX = 400, scroller = TRUE,
                                # fixer les colonnes : 
                                fixedColumns = list(leftColumns = 1),
                                # selection :
                                select = list(style = 'os', items = 'row'),
                                buttons = c(
                                  # enregistrements
                                  'copy', 'csv', 'excel', 'pdf', 'print'
                                  
                                )
                              )))
  
  
  ##################################################################################################################################################################################
  # # Partie : subItem = matiere_ajout
  
  
  connection <- connect_to_database()
  enseignants1 <- dbGetQuery(connection, "SELECT * FROM enseignant")
  dbDisconnect(connection)
  
  output$matiereUI <- renderUI({fluidRow(box(title = "AJOUT D'UN NOUVEAU ETUDIANT", width = 12,  status = "primary", 
                                   solidHeader = TRUE, collapsible = TRUE,
                                   #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
                                   
                     
                                    selectizeInput("NOM1", "Nom de l'Enseignant", choices = enseignants1$NOM, multiple = FALSE),
                                   textInput("MAT", "Nom de la matiere", ""),
                                   selectInput("CYCLE", "Cycle", choices = c("AS","ISE","ISEL","TOUT CYCLE")),
                                   selectInput("CLASSE", "Classe", choices = c("AS1","AS2","AS3","ISE1-ECO","ISE1-MATH","ISE2","ISE3","ISEL1","ISEL2","ISEL3")),
                                   selectInput("SEMESTRE", "Semestre", choices = c("S1", "S2")),
                                   actionButton("submit5", "Ajouter Matière")
    ))})
  
  id_enseignant_selected <- reactive({
    connection <- connect_to_database()
    selected_nom <- input$NOM1
    enseignants1 <- dbGetQuery(connection, "SELECT * FROM enseignant")
    ID_ENSEIGNANT <- enseignants1$ID_ENSEIGNANT[enseignants1$NOM == selected_nom]
    dbDisconnect(connection)
    return(ID_ENSEIGNANT)
  })
  
  observeEvent(input$submit5, {
    
    selected_classe <- input$CLASSE
    
    new_ent <- data.frame(
      ID_ENSEIGNANT = as.character(id_enseignant_selected()),
      MAT = as.character(input$MAT),
      CYCLE = as.character(input$CYCLE),
      CLASSE = as.character(input$CLASSE),
      SEMESTRE = as.character(input$SEMESTRE)
    )
    
    connection <- connect_to_database()
    
    tryCatch({
      # Votre code d'enregistrement ici
      dbWriteTable(connection, "matiere", value = new_ent, append = TRUE, row.names = FALSE)
      
      # Réinitialiser les champs du formulaire
      updateSelectizeInput(session, "NOM1", selected = enseignants1$NOM[1])
      updateTextInput(session, "MAT", value = "")
      updateSelectInput(session, "CYCLE", selected = "AS")
      updateSelectInput(session, "CLASSE", selected = "AS1")
      updateSelectInput(session, "SEMESTRE", selected = "S1")
      
      # Notification de succès
      shinyalert::shinyalert(
        title = "Succès",
        text = "Matière enregistré avec succès!",
        type = "success"
      )
    }, error = function(e) {
      # En cas d'erreur, affichez une notification d'échec
      shinyalert::shinyalert(
        title = "Erreur",
        text = "Échec de l'enregistrement de la matière. Veuillez réessayer !!!",
        type = "error"
      )
    })

    sql_query3 <- paste0("SELECT * FROM matiere WHERE CLASSE = '", selected_classe, "'")
    
    # Exécutez la requête SQL et obtenez les données filtrées
    matiere <- dbGetQuery(connection, sql_query3)
    matiere1 <- dbGetQuery(connection, "SELECT * FROM matiere")
    
    maths1 <- sum(matiere$SEMESTRE == "S1")  
    maths2 <- sum(matiere$SEMESTRE == "S2")
    maths <- maths1 + maths2
    
    ##################################################################################################################################################################################
    # Partie : subitem = enseignant_statistique  
    
    output$A <- renderValueBox({
      valueBox(
        value = maths1,
        subtitle = "Nombre de matière du semestre 1",
        icon=icon("person-chalkboard"),
        color="light-blue")})
    
    output$B <- renderValueBox({
      valueBox(
        value = maths2,
        subtitle = "Nombre de matière du semestre 2",
        icon=icon("chalkboard-user"),
        color="lime")})
    
    output$C <- renderValueBox({
      valueBox(
        value = maths,
        subtitle = "Nombre total de matière",
        icon=icon("user"),
        color="teal")})
    
    
    proportions5 <- data.frame(prop.table(table(matiere$CYCLE)) * 100)
    proportions6 <- data.frame(table(matiere$CYCLE))
    
    output$graph1 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Répartition des matières par cycle") %>%
        hc_subtitle(text = "Source: Notre base de données") %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.percentage:.1f}")
        )) %>%
        hc_add_series(
          name = "cycle",
          data = lapply(seq_len(nrow(proportions5)), function(i) {
            list(
              name = proportions5$Var1[i],
              y = proportions5$Freq[i]
            )
          })
        )
    })
    
    output$graph2 <- renderHighchart({ 
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Répartition des matières par cycle") %>%
        hc_subtitle(text = "Source: Votre base de données") %>%
        hc_xAxis(categories = proportions6$Var1) %>%
        hc_yAxis(title = list(text = "Pourcentage")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%")
        )) %>%
        hc_add_series(
          name = "Cycle",
          data = lapply(seq_len(nrow(proportions6)), function(i) {
            list(
              name = proportions6$Var1[i],
              y = proportions6$Freq[i]
            )
          })
        )
    })
    
    
    output$tableau1=renderDataTable(
      matiere%>%datatable(rownames = FALSE, 
                          extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
                          selection = "none",
                          options = list(
                            dom = "Bfrtip", 
                            # scroll :
                            scrollY = 500, scrollX = 400, scroller = TRUE,
                            # fixer les colonnes : 
                            fixedColumns = list(leftColumns = 1),
                            # selection :
                            select = list(style = 'os', items = 'row'),
                            buttons = c(
                              # enregistrements
                              'copy', 'csv', 'excel', 'pdf', 'print'
                              
                            )
                          ))
      
    )
    
    dbDisconnect(connection)
  })
  
  ##################################################################################################################################################################################
  # # Partie : subItem = questionnaire
  
  
  insert_reponses_questionnaire <- function(connection, reponses) {
    for (reponse in reponses) {
      query <- paste0("INSERT INTO questionnaire (MAT, NOM, ENONCE_OBJECTIF_COURS, CONTENU_COURS, ...)")
      query <- paste0(query, " VALUES ('", reponse$MAT, "', '", reponse$NOM, "', '", reponse$ENONCE_OBJECTIF_COURS, "', '", reponse$CONTENU_COURS, "', '", ... , "')")
      
      # Exécutez la requête SQL
      dbExecute(connection, query)
    }
  }
  
  
  
  matricule <- reactive({
    connection <- connect_to_database()
    selected_nom <- input$nomEvaluation
    etd <- dbGetQuery(connection, "SELECT * FROM etudiant")
    MATRICULE <- etd$MATRICULE[etd$NOM == selected_nom]
    dbDisconnect(connection)
    return(MATRICULE)
  })
  
  sexe <- reactive({
    connection <- connect_to_database()
    selected_nom <- input$nomEvaluation
    etd <- dbGetQuery(connection, "SELECT * FROM etudiant")
    SEXE <- etd$SEXE[etd$NOM == selected_nom]
    dbDisconnect(connection)
    return(SEXE)
  })
  
  observe({
    connection <- connect_to_database()
    etude <- dbGetQuery(connection, "SELECT * FROM etudiant")
    
    if (!is.null(input$classeEvaluation)) {
      base_filtree1 <- subset(etude, CLASSE == input$classeEvaluation)
    } else {
      base_filtree1 <- etude
    }
    
    output$questionnaireUI1 <- renderUI({
      fluidRow(
        box(
          title = "QUESTIONNAIRE D'EVALUATION",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectInput("classeEvaluation", "Classe", choices = unique(etude$CLASSE), selected = input$classeEvaluation),
          selectInput("nomEvaluation", "Nom", choices = base_filtree1$NOM, selected = input$nomEvaluation),
          selectInput("matriculeEvaluation", "Matricule", choices = matricule(), selected = matricule()),
          selectizeInput("anneeEvaluation", "Année", choices = c("2023-2024", "2024-2025", "2025-2026", "2026-2027", "2027-2028", "2028-2029", "2030-2031", "2031-2032", "2033-2034", "2034-2035", "2035-2036", "2036-2037", "2037-2038", "2038-2039", "2039-2040")),
          radioButtons("sexeEvaluation", "Sexe", choices = sexe(), selected = sexe()),
          selectInput("semestreEvaluation", "Semestre", choices = c("S1", "S2"), selected = "S1"),
          actionButton("nextBtn1", "Suivant")
        )
      )
    })
    
    dbDisconnect(connection)
  })
  
  observeEvent(input$classeEvaluation, {
    connection <- connect_to_database()
    etude <- dbGetQuery(connection, "SELECT * FROM etudiant")
    
    if (!is.null(input$classeEvaluation)) {
      base_filtree1 <- subset(etude, CLASSE == input$classeEvaluation)
    } else {
      base_filtree1 <- etude
    }
    
    updateSelectInput(session, "nomEvaluation", choices = base_filtree1$NOM)
    updateSelectInput(session, "matriculeEvaluation", choices = matricule())
    
    dbDisconnect(connection)
  })
    
  nombre_de_pages_reactive <- reactiveVal(NULL)
  
  observe({
    connection <- connect_to_database()
    
    sql <- "SELECT enseignant.NOM, matiere.MAT, matiere.CLASSE, matiere.SEMESTRE as SM FROM enseignant JOIN matiere ON enseignant.ID_ENSEIGNANT = matiere.ID_ENSEIGNANT"
    
    base <- dbGetQuery(connection, sql)
    
    # Filtre la base en fonction de la classe et du semestre choisis
    base_filtree2 <- subset(base, CLASSE == input$classeEvaluation & SM == input$semestreEvaluation)
    
    nombre_de_pages_reactive(nrow(base_filtree2))
    
    # Affiche le questionnaire pour chaque matière et enseignant
    output$questionnaireUI2 <- renderUI({
      nombre_de_pages <- nombre_de_pages_reactive()
      pages <- lapply(1:nombre_de_pages, function(page_num) {
        # Obtenir les informations pour la matière et l'enseignant actuels
        matiere <- base_filtree2$MAT[page_num]
        enseignant <- base_filtree2$NOM[page_num]
        
        pageName <- paste0("page", page_num)
        conditionalPanel(
          condition = sprintf("input.nextBtn1 > 0"),
          fluidPage(
            box(
              title = paste("Questionnaire -", matiere, "-", enseignant),
              width = 12,
              status = "primary",
              collapsed = TRUE,
              solidHeader = TRUE, collapsible = TRUE,
              # ... (Ajoutez ici les éléments spécifiques à chaque page du questionnaire)
              # Ajoutez ici les éléments du questionnaire pour chaque matière
              radioButtons(paste("ENONCE_OBJECTIF_COURS_",page_num), "Énoncé Objectif Cours", choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("CONTENU_COURS_",page_num),"Contenu du cours",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("TAUX_COUVERTURE_PROGRAMME_",page_num), "Taux de couverture du programme",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("CONNAISSANCE_THEORIQUES_ACQUISES_",page_num), "Connaissance theoriques apprises", choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("CONNAISSANCES_PRATIQUES_",page_num), "Connaissance pratiques acquises", choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("CONFORMITE_EVALUATIONS_CONTENU_",page_num), "Conformité evaluation et contenu",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("RAPPORT_DUREE_CONTENU_EPREUVE_",page_num), "Rapport duree et contenu de l'epreuve",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("ASSIDUITE_",page_num), "Assiduité",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("PONCTUALITE_",page_num), "Ponctualité",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("TENUE_VESTIMENTAIRE_",page_num), "Tenu Vestimentaire",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("UTILISATION_OUTILS_MATERIAUX_DIDACTIQUES_",page_num), "Utilisation des outils ou materiaux didactiques",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("DISPONIBILITE_ECOUTE_DES_ETUDIANTS_",page_num), "Disponibilité à ecouter les etudiants",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("MAITRISE_SALLE_COURS_",page_num), "Maitrise de la salle de cours",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("INTERACTION_ENSEIGNANTS_ETUDIANTS_",page_num), "Interaction entre l'enseignant et les etudiants",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("INTEGRATION_TIC_",page_num), "Integration des TIC",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("ORGANISATION_SUIVI_TP_",page_num), "Organisation et suivi des TP, TPE, TD ...",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              radioButtons(paste("CAPACITE_TRANSMISSION_COURS_",page_num), "Capacite à transmettre le cours",choices = c("Tres satisfait", "Satisfait", "Moyen", "Mauvais"),selected = ""),
              textInput(paste("COMMENTAIRE_ASPECT_POSITIF_",page_num), "Commenter les aspects positifs",""),
              textInput(paste("COMMENTAIRE_ASPECT_NEGATIF_",page_num), "Commenter les aspects negatifs",""),
              textInput(paste("SUGGESTIONS_",page_num), "Suggestions","")
            )
          )
        )
      })
      
      pages <- append(pages, fluidPage(
        box(
          title = "Soumission",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          actionButton("submitBtn", "Soumettre")
        )
      ))
      
      tagList(pages)
    })
    
    
    
    dbDisconnect(connection)
  })
  
  id_enseignant_selected <- reactive({
    connection <- connect_to_database()
    selected_nom <- input$NOM1
    enseignants1 <- dbGetQuery(connection, "SELECT * FROM enseignant")
    ID_ENSEIGNANT <- enseignants1$ID_ENSEIGNANT[enseignants1$NOM == selected_nom]
    dbDisconnect(connection)
    return(ID_ENSEIGNANT)
  })
  
  id_etudiant_selected <- reactive({
    connection <- connect_to_database()
    selected_nom <- input$NOM
    enseignants1 <- dbGetQuery(connection, "SELECT * FROM etudiant")
    ID_ETUDIANT <- etudiant$ID_ETUDIANT[etudiant$NOM == selected_nom]
    dbDisconnect(connection)
    return(ID_ETUDIANT)
  })
  
  observeEvent(input$submitBtn, {
    nouvelles_reponses <- lapply(1:nombre_de_pages, function(page_num) {
      list(
        ID_ENSEIGNANT = id_enseignant_selected(),
        ID_ETUDIANT = id_etudiant_selected(),
        NOM = base_filtree2$NOM[page_num],
        ANNEE = input$anneeEvaluation,
        ENONCE_OBJECTIF_COURS = input[[paste("ENONCE_OBJECTIF_COURS_", page_num)]],
        CONTENU_COURS = input[[paste("CONTENU_COURS_", page_num)]],
        TAUX_COUVERTURE_PROGRAMME = input[[paste("TAUX_COUVERTURE_PROGRAMME_", page_num)]],
        CONNAISSANCE_THEORIQUES_ACQUISES = input[[paste("CONNAISSANCE_THEORIQUES_ACQUISES_", page_num)]],
        CONNAISSANCES_PRATIQUES = input[[paste("CONNAISSANCES_PRATIQUES_", page_num)]],
        CONFORMITE_EVALUATIONS_CONTENU = input[[paste("CONFORMITE_EVALUATIONS_CONTENU_", page_num)]],
        RAPPORT_DUREE_CONTENU_EPREUVE = input[[paste("RAPPORT_DUREE_CONTENU_EPREUVE_", page_num)]],
        ASSIDUITE = input[[paste("ASSIDUITE_", page_num)]],
        PONCTUALITE = input[[paste("PONCTUALITE_", page_num)]],
        TENUE_VESTIMENTAIRE = input[[paste("TENUE_VESTIMENTAIRE_", page_num)]],
        UTILISATION_OUTILS_MATERIAUX_DIDACTIQUES = input[[paste("UTILISATION_OUTILS_MATERIAUX_DIDACTIQUES_", page_num)]],
        DISPONIBILITE_ECOUTE_DES_ETUDIANTS = input[[paste("DISPONIBILITE_ECOUTE_DES_ETUDIANTS_", page_num)]],
        MAITRISE_SALLE_COURS = input[[paste("MAITRISE_SALLE_COURS_", page_num)]],
        INTERACTION_ENSEIGNANTS_ETUDIANTS = input[[paste("INTERACTION_ENSEIGNANTS_ETUDIANTS_", page_num)]],
        INTEGRATION_TIC = input[[paste("INTEGRATION_TIC_", page_num)]],
        ORGANISATION_SUIVI_TP = input[[paste("ORGANISATION_SUIVI_TP_", page_num)]],
        CAPACITE_TRANSMISSION_COURS = input[[paste("CAPACITE_TRANSMISSION_COURS_", page_num)]],
        COMMENTAIRE_ASPECT_POSITIF = input[[paste("COMMENTAIRE_ASPECT_POSITIF_", page_num)]],
        COMMENTAIRE_ASPECT_NEGATIF = input[[paste("COMMENTAIRE_ASPECT_NEGATIF_", page_num)]],
        SUGGESTIONS	= input[[paste("SUGGESTIONS_", page_num)]],
        # ... (ajoutez les autres champs du questionnaire)
      )
    })
    
    # Ajout des nouvelles réponses à la liste globale
    liste_reponses_questionnaire <<- append(liste_reponses_questionnaire, nouvelles_reponses)
    
    # Insérer les réponses dans la table questionnaire
    insert_reponses_questionnaire(connection, nouvelles_reponses)
  })
  
####################################################################################################################################
################################################ Partie telechargement #############################################################
  
  
  })

  


##################################################################################################################################################################################
########################################################################## Lancement de l'Application ###############################################################################
##################################################################################################################################################################################
