## Load dependencies
# load('environement') # à faire avant de lancer l'app et mettre en commentaire?
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('dependencies.R')


header <- dashboardHeader(title = 'ML Meet up 2018',
                          titleWidth = 300)

sidebar <- dashboardSidebar(
    h4('Menu'),
    sidebarMenu(
        menuItem(
            'Choix des features',
            tabName = 'data_analysis',
            icon = icon('database')
        ),
        menuItem(
            'Validation du GLM',
            tabName = 'regression_lineaire',
            icon = icon('coffee')
        )
    ),
    h4('Choix critique'),
    sliderInput("choix_variance", label = "Seuil de variance (d'importance)", value = 0.9, min = 0, max = 1, step = 0.01),
    sliderInput("choix_seuil", label = 'Seuil prédiction (treshold)', value = 0.3, min = 0, max = 1, step = 0.01),
    
    
    
    
    icon('github'),
    a('Lien vers Code Source',
      href = 'https://github.com/gabrielcrepeault/predict-anything-R')
)


body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "data_analysis",
            box(title = 'Méthodologie', width = 12, collapsible = T, collapsed = T,
                h3("Analyse des données"),
                p("Trop de variable"),
                h3("Il faut manipuler les données..."),
                p("analyse PCA"),
                h3("Choix important : choix des variables, seuil d'important, treshold"),
                h3("Validation par dataset d'entrainement et de validation")),
            box(title = "Analyse en composantes principales",
                width = 12, collapsible = T, collapsed = T,
                plotOutput('custom_plot'))
            
        ),
        tabItem(
            tabName = 'regression_lineaire',
            fluidRow( # avec les verbatim, il faut wrap dans fluidrow
                box(title = "Optimisation du score F1", width = 12, status = 'success',
                    solidHeader = T,collapsed = T, collapsible = T,
                    img(src = 'f1_score.png')),
                box(title = 'Matrice de confusion et score F1',
                    status = 'success', solidHeader = T,
                    collapsible = T, collapsed = F,
                    width = 12,
                    verbatimTextOutput(
                        'matrice'
                    ),
                    h3("Treshold et seuil d'importance optimaux"),
                    p("Treshold : 0.17"),
                    p("Seuil d'importance : 0.8"),
                    verbatimTextOutput('f1')
                    
                ))
        )
    )
)

ui <- dashboardPage(
    skin = 'green',
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    ## DATA_ANALYSIS
    mat <- reactive({
        
    })
    
    output$custom_plot <- renderPlot({
        plot(pca.summary$importance[3,], ylab = 'Variance', xlab = 'Nombre de variable')
        abline(input$choix_variance,0, col = 'red')
    })
    
    output$matrice <- renderPrint({
        treshold_final <- input$choix_seuil
        importance_final <- input$choix_variance
        f1_final <- 0.387931
        dat.pca_final <- data.frame(Ind = toitvert$green_roof_ind,
                                    pca.x[, which(pca.summary$importance[3,] <= importance_final)])
        mod_final <- glm(Ind~., data = dat.pca_final, family = binomial(link = 'logit'))
        
        predictions_final <- ifelse(mod_final$fitted.values>= treshold_final, 1, 0)
        conf_mat_final <- confusionMatrixFor_Neg1_0_1(dat.pca_final$Ind,
                                                      predictions_final)
        conf_mat_final[-1,-1] / length(mod_final$fitted.values)
    })
    
    
    
    
    
    
}




## Lancer l'application
shinyApp(ui, server)