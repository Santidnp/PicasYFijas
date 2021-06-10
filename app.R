library(shinydashboard)
library(shiny)
library(tidyverse)
library(colourpicker)
library(plotly)

header <- dashboardHeader(
    
    
)
sidebar <- dashboardSidebar(
    textInput("Numero","Tu Jugada: ","0000"),
    actionButton("go","A Jugar!!")
)


body <- dashboardBody(
    
    infoBoxOutput("PICAS"),
    infoBoxOutput("FIJAS"),
    infoBoxOutput("GANASTE"),
    tableOutput("Tabla")
)
ui <- dashboardPage(skin = "purple",header,sidebar,body)



server <-  function(input, output){
    
    Lista_secreta <- function(){
        return( as.vector(sample(c(0,1,2,3,4,5,6,7,8,9),4, replace = FALSE, prob = NULL)))
    }
    Lista <- Lista_secreta() 
    
    
    Numero_picas <- function(lista_jugadas,lista_adivinar){
        
        Picas = 0
        for(j in lista_jugadas){
            if(j %in% lista_adivinar){
                if(lista_jugadas[match(j,lista_jugadas)] != lista_adivinar[match(j,lista_jugadas)]){
                    Picas <- Picas + 1
                }
                
            }
        }
        
        return(Picas)
        
    }
    Numero_fijas <- function(lista_jugadas,lista_adivinar){
        Fijas = 0
        
        for(j in c(1:4)){
            if(lista_jugadas[j]==lista_adivinar[j]){
                Fijas <-  Fijas + 1
            }
            
        }
        return(Fijas)
    }
    
    
    Jugadas <- reactive(as.numeric(strsplit(as.character(input$Numero), "")[[1]]))
    
    output$txt <- renderText({Jugadas()})
    output$txt1 <- renderText({Numero_picas(Jugadas(),Lista)})
    output$txt2 <- renderText(N_picas())
    output$txt3 <- renderText(Lista)
    
    N_picas <- eventReactive(input$go,{
        Numero_picas(Jugadas(),Lista)
    })
    
    N_fijas<- eventReactive(input$go,{
        Numero_fijas(Jugadas(),Lista)
    })
    
    mydata <- data.frame()
    

    df <- eventReactive(input$go, {
        
        newrow = data.frame('Jugada' = input$Numero,
                            'Picas'= N_picas(),
                            'Fijas' = N_fijas())
        mydata <<- rbind(mydata, newrow)
        
        
    })
    #FilaNueva <- c(input$Numero,N_picas(),N_fijas())
    #df1 <- eventReactive(input$go,{rbind(df,FilaNueva())})
    
    output$Tabla <- renderTable(df())
    
    output$PICAS <- renderInfoBox({
        infoBox("Picas",
                N_picas(),icon = icon("list"),
                color = "purple", fill = TRUE
                
        )
    })
    
    output$FIJAS <- renderInfoBox({
        infoBox("Fijas",
                N_fijas(),icon = icon("list"),
                color = "red", fill = TRUE
                
        )
    })
    
    output$GANASTE <-  renderInfoBox({
        
        if(input$Numero==paste(Lista,collapse="")){
            infoBox(
               "GANASTE: el numero era:",
               paste(Lista,collapse=""),
                icon = icon("list"),
                color = "green", fill = TRUE
            )
        }
        else if(input$Numero=='0000'){
            infoBox(
                "Listo Para Jugar ?",
                '0000',
                icon = icon("list"),
                color = "green",
                fill = T
            )
            
        }
        else{
            infoBox(
                "Sigue intentando",
                ":(",
                icon = icon("list"),
                color = "green", fill = TRUE
            )
            
        }
    })
}

shinyApp(ui = ui, server = server)


