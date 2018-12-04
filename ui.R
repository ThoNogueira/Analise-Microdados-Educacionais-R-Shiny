############################################################################################################
# Ferramenta web de análise de microdados educacionais desenvolvida utilizando o Shiny do RStudio.
# 
# Essa ferramenta foi desenvolvida por Thiago Eugênio Ramos Nogueira como estudo de caso do trabalho de conclusão de curso intitulado "UMA ANÁLISE EXPLORATÓRIA DE MICRODADOS EDUCACIONAIS UTILIZANDO MODELAGEM MULTIDIMENSIONAL E LINGUAGEM R" do curso de Ciência da Computação da FBUni.
# A ferramenta foi publicada com a configuração necessária para analise dos microdados dos ENADEs 2015, 2016 e 2017. Porém sua lógica foi desenvolvida de forma que, aplicando as configurações corretas, a ferramenta seja capaz de processar microdados educacionais variados.
############################################################################################################

library(DT)
library(shinythemes)

# Define a interface
ui <- fluidPage(theme = shinytheme("paper"),
    uiOutput("AnoEscolhido"),
    # Layout da página. (Definições de entradas e saídas de dados)
    sidebarLayout(
        # Barra lateral de entrada de dados
        sidebarPanel(
            selectizeInput(
                "Ano",
                "Escolha o ano que deseja avaliar:",
                choices = c(2015, 2016, 2017),
                options = list(
                    onInitialize = I('function() { this.setValue(""); }')
                ),
                selected = NULL,
                multiple = FALSE
            ),
            actionButton("CarregarAno", "Carregar Ano", icon("refresh")),
            #Parametros
            uiOutput("Parametros"),
            #Botão Gerar
            uiOutput("Gerar")
        ),
        # Painel principal que exibe os gráficos
        mainPanel(
            tabsetPanel(
                tabPanel("Graficos", uiOutput("graficos"))
            )
        )
    )
)