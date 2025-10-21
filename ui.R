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
################################################# Definition de chaque partie de l'UI #######################################################################################
##################################################################################################################################################################################

title <- tags$p(href='https://ceeac-eccas.org/#intranet',
                tags$img(src="issea.png", height = '50', width = '50'),
                'EVALUATION DES ENSEIGNANTS', target="_blank",colors= "blue")

##################################################################################################################################################################################
# Partie header
##################################################################################################################################################################################

header = shinydashboard::dashboardHeader(title = title,titleWidth = 600,
                                         tags$li(class="dropdown",tags$a(href="https://web.facebook.com/IsseaCemac", icon("facebook"), "ISSEA", target="_blank")),
                                         tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/school/issea-cemac/" ,icon("linkedin"), "ISSEA", target="_blank")),
                                         tags$li(class="dropdown",tags$a(href="https://github.com/aagarw30/R-Shinyapp-Tutorial", icon("github"), "Source Code", target="_blank"))
)

##################################################################################################################################################################################
# Partie Body
##################################################################################################################################################################################
body = shinydashboard::dashboardBody(theme_blue_gradient,tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tabItems(
    
    
    ###################################################################################################################################################################################
    ############################################################### Creation des tabitems #############################################################################################
    
    
    ############################################################### TabItem = Acceuil ################################################################################################
    
    tabItem(tabName = "acceuil",
            
              ## inclusion du fichier css
              tags$style(HTML("
      #header { background-image: url('IMG-20240202-WA0116.JPG'); background-size: cover; color: blue; padding: 10px; text-align: center; width:1270px;height:650px;font-size: 14px;
              border: 2px solid blue; /* Couleur et épaisseur de la bordure */
                border-radius: 10px; /* Coins arrondis */
                padding: 15px; /* Espace intérieur de la bordure */ }
      
      #sub {
          color: black;
          
      }
      
      #org {
          color: black;
      }
      
      #h1 {
      font-size: 20px;
                font-weight: bold;
                margin-top: 20px;
                border: 2px solid aqua; /* Couleur et épaisseur de la bordure */
                border-radius: 10px; /* Coins arrondis */
                padding: 15px; /* Espace intérieur de la bordure */ 
                color: pink;
      }
      

      #logo-left, #logo-right {
        width: 50px; /* Ajustez la taille selon vos besoins */
        height: 50px;
        margin: 0 10px;
        display: inline-block;
      }

      #main-text {
                font-size: 30px;
                font-weight: bold;
                margin-top: 20px;
                color: blue;
                border: 2px solid blue; /* Couleur et épaisseur de la bordure */
                border-radius: 10px; /* Coins arrondis */
                padding: 15px; /* Espace intérieur de la bordure */
              }

      #sub-text {
        font-size: 18px;
        margin-top: 10px;
        color: black
      }

      #footer {
        display: flex;
        justify-content: space-between;
        font-size: 14px;
        margin-top: 20px;
        text-align: left;
      }
      
      .tab-buttons-custom .nav-tabs > li > a {
      border: 1px solid #ddd; /* Bordure */
      border-radius: 5px; /* Coins arrondis */
      margin-right: 5px; /* Marge entre les boutons */
      padding: 8px 15px; /* Espacement interne */
      color: #333; /* Couleur du texte */
      background-color: #fff; /* Couleur de fond */
    }

    .tab-buttons-custom .nav-tabs > li.active > a,
    .tab-buttons-custom .nav-tabs > li.active > a:hover,
    .tab-buttons-custom .nav-tabs > li.active > a:focus {
      background-color: #3c8dbc; /* Couleur de fond pour l'onglet actif */
      color: #ffffff; /* Couleur du texte pour l'onglet actif */
    }
    ")),
              
              div(id = "header",
                  div(id = "sub", "Communauté Économique et Monétaire de l'Afrique Centrale"),
                  div(id = "sub", "(CEMAC)"),
                  h1("INSTITUT SOUS REGIONAL DE STATISTIQUE ET D'ECONOMIE APPLIQUÉE"),
                  div(id = "org", "Organisation Internationale"),
                  img(src = "issea.png", alt = "Logo Gauche", id = "logo-left",with= "60",height= "60"),
                  img(src = "issea.png", alt = "Logo Droite", id = "logo-right",with= "60",height= "60")
                  ,br(),br(),br(),br(),br(),
                  
                  
                  div(id = "main-text", "APPLICATION DE SUIVI DES ENSEIGNANTS DE L'ISSEA"),br(),br(),br(),
                  div(id = "sub-text", "Réalisé par : "),
                  div(id = "sub-text", "- DINGA DJOBLAO Leonard"),
                  div(id = "footer", "Supervisé par : M. MVONDO")
              ),width = 50, height = 680
            )
           ,
    
    ###################################################################### tabitem = enseignant ###########################################################################################
    ######################################################################
    
    ###################################################################### Subitem = enseignant_statistique ###########################################################################################
    
    tabItem(tabName = "enseignant_statistique",
            
            fluidRow(width = 12,height = 120,
                     valueBoxOutput("Nombre1", width = 4),
                     valueBoxOutput("Nombre_ens_asso", width = 4),
                     valueBoxOutput("Nombre_mat", width = 4)),
            
            box(height = 400, column(width = 6,fluidRow(withSpinner(highchartOutput("gr1",width = 600))))),
            box(height = 400, column(width = 6, fluidRow(withSpinner(highchartOutput("gr2",width = 600))))),
            
            fluidRow(box(title = "base de donnée enseignants", width = 12,  status = "primary", collapsed = TRUE,
                         solidHeader = TRUE, collapsible = TRUE,
                         #tags$style(".skin-blue .sidebar a { color: purple; }"), # higlting tabs)     
                         dataTableOutput("tab1")))),
    
    #################################################################### subItem = enseignant_ajout ############################################################################################
    
    tabItem(tabName = "enseignant_ajout",
            fluidRow(box(title = "base de donnée enseignants", width = 12,  status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE,
                         #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)     
                         textInput("nom", "Nom", ""),
                         textInput("prenom", "Prénom", ""),
                         radioButtons("sexe", "Sexe", choices = c("Masculin", "Féminin"), selected = ""),
                         selectInput("type", "type", choices = c("PERMANENT","ASSOCIE","VACATAIRE"),selected = "PERMANENT"),
                         textInput("specialite1", "Spécialité 1", ""),
                         textInput("specialite2", "Spécialité 2", ""),
                         dateInput("date_entree", "Date d'Entrée", ""),
                         selectInput("nationalite","Nationalité",choices = c("CAMEROUNAISE","TCHADIENNE","CENTRAFRICAINE","CONGOLAISE","GABONNAISE","GUINNEENNE","NIGERIENNE","MALGACHE","TOGOLAISE","BENINOISE","AUTRES"),selected = "CAMEROUNAISE"),
                         actionButton("submit_btn1", "Enregistrer")))),
    
    ###################################################################### tabitem = etudiant ###########################################################################################
    ######################################################################
    
    #################################################################### subItem = "etudiant_statistique" ############################################################################################
    
    tabItem(tabName = "etudiant_statistique",
            box(width = 12,height = 90,
                column(width=2,fluidRow(selectInput("input3","Filtrer le cycle",choices = c("AS","ISE","ISEL","TOUT CYCLE"),selected = "AS"))),
                column(width=2,fluidRow(selectInput("input4","Filtrer la classe",choices = c("AS1","AS2","AS3","ISE1-ECO","ISE1-MATH","ISE2","ISE3","ISEL1","ISEL2","ISEL3"))))),
            
            fluidRow(width = 12,height = 120,
                     valueBoxOutput("Nombre_et", width = 4),
                     valueBoxOutput("Nombre_eth", width = 4),
                     valueBoxOutput("Nombre_etf", width = 4)),
            
            
            box(height = 400, column(width = 6,fluidRow(withSpinner(highchartOutput("gr3",width = 600))))),
            box(height = 400, column(width = 6, fluidRow(withSpinner(highchartOutput("gr4",width = 600))))),
            
            
            fluidRow(box(title = "base de donnée étudiants", width = 12,  status = "primary", collapsed = TRUE,
                         solidHeader = TRUE, collapsible = TRUE,
                         #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)     
                         dataTableOutput("tab2")))),
    
    #################################################################### subItem = "etudiant_ajout" ############################################################################################
    
    tabItem(tabName = "etudiant_ajout",
            fluidRow(box(title = "AJOUT D'UN NOUVEAU ETUDIANT", width = 12,  status = "primary", 
                         solidHeader = TRUE, collapsible = TRUE,
                         #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)     
                         textInput("NOM", "Nom", ""),
                         textInput("PRENOM", "Prénom", ""),
                         selectInput("CLASSE", "Classe",choices = c("AS1","AS2","AS3","ISE1-ECO","ISE1-MATH","ISE2","ISE3","ISEL1","ISEL2","ISEL3"),selected = "AS1"),
                         dateInput("DATE_NAISSANCE", "Date de Naissance", ""),
                         radioButtons("SEXE", "Sexe", choices = c("MASCULIN", "FEMININ"), selected = "MASCULIN"),
                         textInput("MATRICULE", "Matricule", ""),
                         dateInput("DATE_ENTRE", "Date d'Entrée", ""),
                         selectInput("NATIONALITE", "Nationalité",choices = c("CAMEROUNAISE","TCHADIENNE","CENTRAFRICAINE","CONGOLAISE","GABONNAISE","GUINNEENNE","NIGERIENNE","MALGACHE","TOGOLAISE","BENINOISE","AUTRES"),selected = "CAMEROUNAISE"),
                         actionButton("submit_btn2", "Enregistrer", onclick = "Shiny.onInputChange('submit_btn2', Math.random()")))),
    
    ###################################################################### tabitem = matiere ###########################################################################################
    ######################################################################
    
    ############################################################### SubItem = "matiere_statistique" #######################################################################
            
    tabItem(tabName = "matiere_statistique",
                    
                      box(width = 12, height = 90,
                          column(width = 4, selectInput("input5", "Cycle", choices = c("AS", "ISE", "ISEL", "TOUT CYCLE"), selected = "AS")),
                          column(width = 4, selectInput("input6", "Classe", choices = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3")))
                      ),
                      
                      fluidRow(width = 12,height = 120,
                               valueBoxOutput("A", width = 4),
                               valueBoxOutput("B", width = 4),
                               valueBoxOutput("C", width = 4)),
                      
                      box(height = 400, column(width = 6,fluidRow(withSpinner(highchartOutput("graph1",width = 600))))),
                      box(height = 400, column(width = 6, fluidRow(withSpinner(highchartOutput("graph2",width = 600))))),
                      
                      fluidRow(box(title = "base de donnée matiere", width = 12,  status = "primary", collapsed = TRUE,
                                   solidHeader = TRUE, collapsible = TRUE,
                                   #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)     
                                   dataTableOutput("tableau1")))
                    ),
    
    #################################################################### subiItem = matiere_ajout ############################################################################################         ,
    tabItem(tabName = "matiere_ajout",
            uiOutput("matiereUI")),
            
    
    ###################################################################### tabitem = evaluation ###########################################################################################
    ######################################################################
    
    #################################################################### subItem = evaluation_database ############################################################################################
    
    tabItem(tabName = "evaluation_database",fluidRow(box(title = "base de donnée de l'enquete d'evaluation", width = 12,  status = "primary", collapsed = TRUE,
                                          solidHeader = TRUE, collapsible = TRUE,
                                          #tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)     
                                          dataTableOutput("tableau2")))),
    
    #################################################################### subItem = Tableau ############################################################################################
    
    tabItem(tabName = "evaluation_tableau_de_bord",
            fluidRow(
              box(width = 12, height = 90,
              column(width = 3, selectInput("critere1", "Cycle", choices = c("AS", "ISE", "ISEL", "TOUT CYCLE"), selected = "AS")),
              column(width = 3, selectInput("critere2", "Classe", choices = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3"))),
              column(width = 3, selectInput("critere2", "Enseignant", choices = "")),
              column(width = 3, selectInput("critere2", "Semestre", choices = c("S1","S2"), selected = "S1")))),
            box(width = 15, tabsetPanel( 
              id = "tabset",
              
              tabPanel("Couverture de la collecte",class = "tab-buttons-custom"),  # Ajoutez des classes personnalisées si nécessaire
              tabPanel("Analyse 1",class = "tab-buttons-custom",
                       box(width = 3,height = 90,
                           column(width =4, highchartOutput("anal_graph1")),
                           column(width =4, highchartOutput("anal_graph2")),
                           column(width =4,highchartOutput("anal_graph3")),
                           column(width =6, highchartOutput("anal_graph4")),
                           column(width =6, highchartOutput("anal_graph5"))),
                      ),
              tabPanel("Analyse 2",class = "tab-buttons-custom"),
              tabPanel("Analyse 3",class = "tab-buttons-custom"),
              tabPanel("Analyse 4",class = "tab-buttons-custom"),
              tabPanel("Analyse 5",class = "tab-buttons-custom")
              
            ))),
    
    
    
    #################################################################### TabItem = Questionnaire ############################################################################################
    ################################################### Première Page (Identification) ##############################################################  
    tabItem(tabName = "questionnaire",
            uiOutput("questionnaireUI1"),
            uiOutput("questionnaireUI2")
      ),
    #################################################################### TabItem = Telechargement ############################################################################################
    tabItem(
      tabName = "telechargement",
      fluidRow(
        box(
          title = "TÉLÉCHARGEMENT",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectInput("cycleSelector", "Cycle", choices = c("AS", "ISEL", "ISE"), selected = "AS"),
          selectInput("classeSelector", "Classe", choices = c("AS1", "AS2", "AS3", "ISE1-ECO", "ISE1-MATH", "ISE2", "ISE3", "ISEL1", "ISEL2", "ISEL3"), selected = "AS1"),
          selectInput("semestreSelector", "Semestre", choices = c("S1", "S2"), selected = "S1"),
          selectInput("enseignantSelector", "Enseignant", choices = c("Enseignant1", "Enseignant2", "Enseignant3"), selected = "Enseignant1"),
          selectInput("formatSelector", "Format du fichier", choices = c("PDF", "WORD", "HTML"), selected = "PDF"),
          downloadButton("downloadReport", label = "Télécharger le rapport")
        )
      )
    )
    ,
    #################################################################### TabItem = Read ############################################################################################
    
    tabItem(tabName = "Read")
  )
)


##################################################################################################################################################################################
# Partie siderMenu
##################################################################################################################################################################################

siderbar = shinydashboard::dashboardSidebar(
  sidebarMenu(
    menuItem("Accueil", tabName = "acceuil", icon = icon("home")),
    menuItem("Enseignant", tabName = "enseignant", icon = icon("chalkboard-teacher"),
             menuSubItem("Statistique", tabName = "enseignant_statistique"),
             menuSubItem("Ajout", tabName = "enseignant_ajout")
    ),
    menuItem("Etudiant", tabName = "etudiant", icon = icon("user-graduate"),
             menuSubItem("Statistique", tabName = "etudiant_statistique"),
             menuSubItem("Ajout", tabName = "etudiant_ajout")
    ),
    menuItem("Matière", tabName = "matiere", icon = icon("book"),
             menuSubItem("Statistique", tabName = "matiere_statistique"),
             menuSubItem("Ajout", tabName = "matiere_ajout")
    ),
    menuItem("Évaluation", tabName = "evaluation", icon = icon("clipboard-list"),
             menuSubItem("Base de Données", tabName = "evaluation_database"),
             menuSubItem("Tableaux de Bord", tabName = "evaluation_tableau_de_bord")
    ),
    menuItem("Questionnaire", tabName = "questionnaire", icon = icon("file-alt")
    
  ),
  menuItem("Téléchargement", tabName = "telechargement", icon = icon("download")
  ),
  menuItem("A propos", tabName = "Read",icon = icon("info-circle"),badgeLabel = "new", badgeColor = "blue")))








##################################################################################################################################################################################
############################################################ Definition de l'interface UI ########################################################################################
##################################################################################################################################################################################

shinyUI(dashboardPage(header,siderbar,body,title= "Dashboard"))


