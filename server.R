############################################################################################################
# Ferramenta web de análise de microdados educacionais desenvolvida utilizando o Shiny do RStudio.
#
# Essa ferramenta foi desenvolvida por Thiago Eugênio Ramos Nogueira como estudo de caso do trabalho de conclusão de curso intitulado "UMA ANÁLISE EXPLORATÓRIA DE MICRODADOS EDUCACIONAIS UTILIZANDO MODELAGEM MULTIDIMENSIONAL E LINGUAGEM R" do curso de Ciência da Computação da FBUni.
# A ferramenta foi publicada com a configuração necessária para analise dos microdados dos ENADEs 2015, 2016 e 2017. Porém sua lógica foi desenvolvida de forma que, aplicando as configurações corretas, a ferramenta seja capaz de processar microdados educacionais variados.
############################################################################################################

# Aumenta a memória destinada a execução do programa
options(java.parameters = "- Xmx1024m")

#Importa a bibliotecas
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)

# Define a função que será executada no servidor.
server <- function(input, output) {

    #Ano Escolhido
    output$AnoEscolhido <- renderUI({
        h4("Microdados ENADE:")
    })

    CarregarDados <- function(ano)
    {
        ###################################################### CONFIGURAÇÃO: ####################################################

        # Definir o diretório dos dados:
        diretorioDosDados <- "C:/Users/Thiago/Documents/Projetos/FFB-TCCII/AnaliseDeMicrodadosEducacionais NOVO/Dados/"
        # Definir o nome da Fato:
        NOME_DA_FATO <- "MICRODADOS_ENADE"
        COLUNA_DE_CODIGO_DA_INSTITUICAO <<- "CO_IES"
        # Define as colunas da Fato que possuem as escolhas das questões objetivas
        COLUNAS_DE_ESCOLHAS <<- c("DS_VT_ESC_OFG", "DS_VT_ESC_OCE")
        # Define as colunas da Fato que possuem os gabaritos das questões objetivas
        # IMPORTANTE: É NECESSÁRIO que as colunas de gabarito estejam na mesma ordem das colunas correspondentes às escolhas
        COLUNAS_DE_GABARITOS <<- c("DS_VT_GAB_OFG_FIN", "DS_VT_GAB_OCE_FIN")
        # Definir a lista de nomes das Dimensoes:
        # É necessário que as dimensões sejam definidas conforme sua ordem de abrangência da mais abrangênte para a menos abrangênte
        # Ex.: Uma unidade federativa é mais abrangente do que uma categoria administrativa 
        NOMES_DAS_DIMENSOES <<- c(
            "GRUPOS",
            "UNIDADES_FEDERATIVAS",
            "CATEGORIAS_ADMINISTRATIVAS",
            # "ORGANIZACOES_ACADEMICAS",
            "INSTITUICOES"
            );
        NOMES_DAS_DIMENSOES_DE_QUESTOES_DISCURSIVAS <<- c(
            "QUESTOES_DISCURSIVAS"
        )
        NOMES_DAS_DIMENSOES_DE_QUESTOES_OBJETIVAS <<- c(
            "QUESTOES_OBJETIVAS"
        )
        # Mapeamento das colunas:
        # Todas as dimensões devem OBRIGATORIAMENTE ter uma coluna ID
        # Todas as dimensões devem OBRIGATORIAMENTE ter uma coluna NOME
        # A coluna SIGLA das dimensões é importante para ser utilizada na exibição dos gráficos, para reduzir as informações. Mas são opcionais.
        # Caso não haja sigla, remover da lista, não deixar vazio
        COLUNAS_POR_DIMENSAO <<- c(
            "GRUPOS" = c(ID = "CO_GRUPO", NOME = "NO_GRUPO"),
            "UNIDADES_FEDERATIVAS" = c(ID = "CO_UF_CURSO", NOME = "NO_UF", SIGLA = "SIGLA"),
            "CATEGORIAS_ADMINISTRATIVAS" = c(ID = "CO_CATEGAD", NOME = "NO_CATEGAD", SIGLA = "SG_CATEGAD"),
            # "ORGANIZACOES_ACADEMICAS" = c(ID = "CO_ORGACAD", NOME = "NO_ORGACAD", SIGLA = "SG_ORGACAD"),
            "INSTITUICOES" = c(ID = "CO_IES", NOME = "NO_IES", SIGLA = "SG_IES", UF ="SIGLA"),
            "QUESTOES_DISCURSIVAS" = c(ID = "COL_QUESDIS", NOME = "NO_QUESDIS", CATEGORIA = "CTG_QUESDIS"),
            "QUESTOES_OBJETIVAS" = c(ID = "CO_QUESOBJ", NOME = "NO_QUESOBJ", CATEGORIA = "CTG_QUESOBJ")
        )

        ################################################## FIM DA CONFIGURAÇÃO: #################################################

        NOMES_DAS_DIMENSOES_E_MEDIDAS <<- c(NOMES_DAS_DIMENSOES, NOMES_DAS_DIMENSOES_DE_QUESTOES_DISCURSIVAS, NOMES_DAS_DIMENSOES_DE_QUESTOES_OBJETIVAS)
        
        QUANTIDADE_DE_DIMENSOES <<- length(NOMES_DAS_DIMENSOES)
        QUANTIDADE_DE_MEDIDAS_DISCURSIVAS <<- length(NOMES_DAS_DIMENSOES_DE_QUESTOES_DISCURSIVAS)
        QUANTIDADE_DE_MEDIDAS_OBJETIVAS <<- length(NOMES_DAS_DIMENSOES_DE_QUESTOES_OBJETIVAS)
        QUANTIDADE_DE_DIMENSOES_E_MEDIDAS <<- (QUANTIDADE_DE_DIMENSOES + QUANTIDADE_DE_MEDIDAS_DISCURSIVAS + QUANTIDADE_DE_MEDIDAS_OBJETIVAS)

        ####################################################### LEITURA: ########################################################
        
        # Função que irá carregar as informações necessárias para o funcionamento da aplicação
        CarregarInformacoesBase <- function(EstadoDoProgresso = NULL){
            if (is.function(EstadoDoProgresso)) {
                EstadoDoProgresso(estado = "Fato...")
            }

            # Carrega os microdados
            MICRODADOS <<- fread(paste0(diretorioDosDados, ano, "/", NOME_DA_FATO, ".txt"), header = T)
            
            if (is.function(EstadoDoProgresso)) {
                EstadoDoProgresso(estado = "Dimensões...")
            }

            dimensoes <<- list()

            # Para cada dimensão
            for(nomeDaDimensaoOuMedida in NOMES_DAS_DIMENSOES_E_MEDIDAS)
            {
                # Lê o arquivo e guarda o data frame na memória
                dimensoes[[nomeDaDimensaoOuMedida]] <<- fread(paste0(diretorioDosDados, ano, "/", nomeDaDimensaoOuMedida, ".txt"), header = T, encoding = 'UTF-8')
            }
        }

        # Cria o controle de progresso
        progresso <- shiny::Progress$new(style = "notification")
        progresso$set(message = "Aguarde, carregando aplicação.", value = 0)
        on.exit(progresso$close())
        EstadoDoProgresso <- function(valor = NULL, estado = "") {
            if (is.null(valor)) {
                valor <- progresso$getValue()
                valor <- valor + (progresso$getMax() - valor) / 7
            }
            progresso$set(value = valor, detail = paste("Carregando", estado))
        }
        
        CarregarInformacoesBase(EstadoDoProgresso)

        ################################################### FIM DA LEITURA: #####################################################

        ################################################### INICIALIZAÇÃO: ######################################################
        opcoesDosParametros <- list()
        # Para cada dimensão
        for(nomeDaDimensaoOuMedida in NOMES_DAS_DIMENSOES_E_MEDIDAS)
        {
            # Resgata a coluna que é o ID da dimensão corrente, se existir
            ID <- NULL
            if(paste0(nomeDaDimensaoOuMedida,".ID") %in% names(COLUNAS_POR_DIMENSAO))
            {
                ID <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensaoOuMedida,".ID")]]
            }
            else
            {
                stop(paste("Todas as dimensões devem OBRIGATORIAMENTE ter uma coluna ID:", nomeDaDimensaoOuMedida))
            }
              # Resgata a coluna que é o SIGLA da dimensão corrente, se existir
            SIGLA <- ""
            if(paste0(nomeDaDimensaoOuMedida,".SIGLA") %in% names(COLUNAS_POR_DIMENSAO))
            {
                colunaSigla <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensaoOuMedida,".SIGLA")]]
                SIGLA <- dimensoes[[nomeDaDimensaoOuMedida]][[colunaSigla]]
            }
            # Resgata a coluna que é o NOME da dimensão corrente, se existir
            NOME <- ""
            if(paste0(nomeDaDimensaoOuMedida,".NOME") %in% names(COLUNAS_POR_DIMENSAO))
            {
                colunaNome <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensaoOuMedida,".NOME")]]
                NOME <- paste0(" - ", dimensoes[[nomeDaDimensaoOuMedida]][[colunaNome]])
            }
            # Resgata a coluna que é a UF da dimensão corrente, se existir
            UF <- ""
            if(paste0(nomeDaDimensaoOuMedida,".UF") %in% names(COLUNAS_POR_DIMENSAO))
            {
                colunaUF <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensaoOuMedida,".UF")]]
                UF <- paste0(" - ", dimensoes[[nomeDaDimensaoOuMedida]][[colunaUF]])
            }
            
            # Resgata os IDs dos registros da dimensão corrente. Estes serão os valores das opções do parâmetro correspondente a esta dimensão. 
            valoresDasOpcoes <- dimensoes[[nomeDaDimensaoOuMedida]][[ID]]

            # Resgata o código e monta a lista de nomes que serão exibidos nos campos da barra lateral.
            opcoes <- as.list(valoresDasOpcoes)
            names(opcoes) <- paste0("[", valoresDasOpcoes, "]", SIGLA, NOME, UF)

            opcoesDosParametros[[nomeDaDimensaoOuMedida]] <- opcoes
        }

        #Ano Escolhido
        output$AnoEscolhido <- renderUI({
            h4(paste0("Microdados: ", ano))
        })
        #Parametros
        output$Parametros <- renderUI({
            camposParametros <- vector("list", QUANTIDADE_DE_DIMENSOES_E_MEDIDAS)
            
            contador <- 1
            for(nomeDaDimensaoOuMedida in NOMES_DAS_DIMENSOES_E_MEDIDAS)
            {
                campo <- selectizeInput(
                    nomeDaDimensaoOuMedida,
                    paste0("Escolha as opções da dimensão: ", nomeDaDimensaoOuMedida),
                    choices = opcoesDosParametros[[nomeDaDimensaoOuMedida]],
                    selected = NULL,
                    multiple = TRUE
                )

                camposParametros[[contador]] <- campo

                contador <- contador + 1 
            }

            do.call(tagList, camposParametros)
        })
        output$Gerar <- renderUI({actionButton(
            "Gerar",
            "Gerar gráficos",
            icon("refresh")
        )})
        ############################################### FIM DA INICIALIZAÇÃO: ###################################################
    }
    observeEvent(input$CarregarAno, {
        if(isTruthy(input$Ano))
        {
            output$graficos <- renderUI({})
            CarregarDados(input$Ano)
        }
    })
    observeEvent(input$Gerar, {
        output$graficos <- renderUI({

        isolate({
            
            ############################################ DIMENSÕES/CÁLCULOS: ####################################################

            # É possível melhorar a performance caso consiga saber exatamente a quandidade de de medias
            mediasDiscursivas <- list()

            rotulosDasBarrasDosGraficos <- list()
            contextosDasBarrasDosGraficosDiscursivas <- list()
            contextosDasBarrasDosGraficosObjetivas <- list()
            contextoEstaDefinido <- FALSE

            # Considera os primeiros microdadosFiltrados como sendo todos os  MICRODADOS
            microdadosFiltrados <- MICRODADOS

            gabarito <- NULL
            quantidadeDeQuestoes <- NULL 

            listaDeAcertosObjetivas <- list()
            listaDeErrosObjetivas <- list()

            totalDeAlunosNaInstituicaoPrincipal <- NULL

            gabaritoPorQuestao <- list()

            # Para cada dimensão configurada
            for(nomeDaDimensao in NOMES_DAS_DIMENSOES)
            {
                # Seleciona as opções escolhidas no parâmetro correspondente à dimensão corrente
                opcoesEscolhidasDaDimensaoCorrente <- input[[nomeDaDimensao]]
                
                # Resgata a coluna que é o ID da dimensão corrente
                nomeDaColunaDaDimensaoID <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensao,".ID")]]

                # Resgata a coluna que é a SIGLA da dimensão corrente
                dimensaoSIGLA <- ""
                if(paste0(nomeDaDimensao,".SIGLA") %in% names(COLUNAS_POR_DIMENSAO))
                {
                    dimensaoSIGLA <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaDimensao,".SIGLA")]]
                }

                # Para cada opção escolhida para a dimensão corrente
                for(opcaoEscolhidaDaDimensaoCorrente in opcoesEscolhidasDaDimensaoCorrente)
                {
                    # Filtra os dados considerando a opção corrente da dimensão corrente
                    # Este será o contexto de cálculo das médias para cada medida
                    microdadosFiltradosPelaOpcaoDaDimensaoCorrente <- microdadosFiltrados %>% filter(UQ(as.name(nomeDaColunaDaDimensaoID)) == opcaoEscolhidaDaDimensaoCorrente)
                    
                    # Configura os nomes dos rótulos dos gráficos
                    quantidadeDeRegistrosNoContextoCorrente <- nrow(microdadosFiltradosPelaOpcaoDaDimensaoCorrente)
                    
                    siglaDaOpcaoDaDimensaoParaRotulo <- NULL
                    if(dimensaoSIGLA != "")
                    {
                        opcaoDaDimensaoParaRotulo <- dimensoes[[nomeDaDimensao]] %>% filter(UQ(as.name(nomeDaColunaDaDimensaoID)) == opcaoEscolhidaDaDimensaoCorrente)
                        siglaDaOpcaoDaDimensaoParaRotulo <- opcaoDaDimensaoParaRotulo[[dimensaoSIGLA]]
                    }

                    # Se não possuir rotulo ainda
                    if(length(rotulosDasBarrasDosGraficos) == 0)
                    {
                        siglaDaOpcaoDaDimensaoParaRotulo <- "BRASIL"
                        rotulosDasBarrasDosGraficos <- c(paste0("BRASIL", " (", quantidadeDeRegistrosNoContextoCorrente, ")"))

                        for(colunaDeGabarito in COLUNAS_DE_GABARITOS)
                        {
                            gabarito <- paste0(gabarito, microdadosFiltradosPelaOpcaoDaDimensaoCorrente[1, colunaDeGabarito]) #junção dos gabaritos
                        }

                        quantidadeDeQuestoes <- stri_length(gabarito) #total de questões
                    }
                    else
                    {
                        rotulosDasBarrasDosGraficos <- c(rotulosDasBarrasDosGraficos, paste0(siglaDaOpcaoDaDimensaoParaRotulo, " (", quantidadeDeRegistrosNoContextoCorrente, ")"))
                    }

                    # Para cada medida DISCURSIVA configurada
                    for(nomeDaMedidaDiscursiva in NOMES_DAS_DIMENSOES_DE_QUESTOES_DISCURSIVAS)
                    {
                        # Seleciona as opções escolhidas no parâmetro correspondente à medida corrente
                        opcoesEscolhidasDaMedidaCorrente <- input[[nomeDaMedidaDiscursiva]]

                        # Para cada opção escolhida da medida corrente
                        for(opcaoEscolhidaDaMedidaCorrente in opcoesEscolhidasDaMedidaCorrente)
                        {
                            contextosDasBarrasDosGraficosDiscursivas[[opcaoEscolhidaDaMedidaCorrente]] <- c(contextosDasBarrasDosGraficosDiscursivas[[opcaoEscolhidaDaMedidaCorrente]], siglaDaOpcaoDaDimensaoParaRotulo)

                            # Calcula a média das notas da questão correspondente à opção escolhida corrente
                            # considerando como contexto os dados filtrados pela opção corrente da dimensão corrente
                            mediaComNotaDiscursiva <- c(mediasDiscursivas[[opcaoEscolhidaDaMedidaCorrente]], round(mean(microdadosFiltradosPelaOpcaoDaDimensaoCorrente[[opcaoEscolhidaDaMedidaCorrente]], na.rm=T), digits=0))
                            notasSemNota = replace(microdadosFiltradosPelaOpcaoDaDimensaoCorrente[[opcaoEscolhidaDaMedidaCorrente]], is.na(microdadosFiltradosPelaOpcaoDaDimensaoCorrente[[opcaoEscolhidaDaMedidaCorrente]]), 0)
                            mediaSemNotaDiscursiva <- c(mediasDiscursivas[[opcaoEscolhidaDaMedidaCorrente]], round(mean(notasSemNota, digits=0)))

                            mediasDiscursivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]] <- c(mediaComNotaDiscursiva, mediaSemNotaDiscursiva)
                        }
                    }

                    unicasInstituicoesDoContexto <- unique(microdadosFiltradosPelaOpcaoDaDimensaoCorrente[[COLUNA_DE_CODIGO_DA_INSTITUICAO]])
                    ehContextoDaInstituicaoPrincial <- (length(unicasInstituicoesDoContexto) == 1 && unicasInstituicoesDoContexto == opcoesEscolhidasDaDimensaoCorrente[1])

                    listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]] <- NULL

                    # Para cada medida OBJETIVA configurada
                    for(nomeDaMedidaObjetiva in NOMES_DAS_DIMENSOES_DE_QUESTOES_OBJETIVAS)
                    {
                        # Seleciona as opções escolhidas no parâmetro correspondente à medida corrente
                        opcoesEscolhidasDaMedidaCorrente <- input[[nomeDaMedidaObjetiva]]

                        contagemDeAcertosPorQuestao <- list()

                        # Para cada opção escolhida da medida corrente
                        for(opcaoEscolhidaDaMedidaCorrente in opcoesEscolhidasDaMedidaCorrente)
                        {
                           
                            contextosDasBarrasDosGraficosObjetivas[[opcaoEscolhidaDaMedidaCorrente]] <- c(contextosDasBarrasDosGraficosObjetivas[[opcaoEscolhidaDaMedidaCorrente]], siglaDaOpcaoDaDimensaoParaRotulo)

                            if(ehContextoDaInstituicaoPrincial)
                            {
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["A"]] <- 0
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["B"]] <- 0
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["C"]] <- 0
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["D"]] <- 0
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["E"]] <- 0
                                listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["N"]] <- 0
                            }

                            listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]] <- 0

                            # Calcula a média das notas da questão correspondente à opção escolhida corrente
                            # considerando como contexto os dados filtrados pela opção corrente da dimensão corrente

                            item_correto <- substr(gabarito, opcaoEscolhidaDaMedidaCorrente, opcaoEscolhidaDaMedidaCorrente)
                            gabaritoPorQuestao[[opcaoEscolhidaDaMedidaCorrente]] <- item_correto

                            if (item_correto %in% c("A", "B", "C", "D", "E"))
                            {
                                totalDeAlunosNoContexto <- nrow(microdadosFiltradosPelaOpcaoDaDimensaoCorrente)

                                #Itera sobre alunos
                                for (indiceDoRegistro in 1:totalDeAlunosNoContexto)
                                {
                                    registro <- microdadosFiltradosPelaOpcaoDaDimensaoCorrente[indiceDoRegistro,]

                                    escolhas <- NULL
                                    for(colunaDeEscolha in COLUNAS_DE_ESCOLHAS)
                                    {
                                        escolhas <- paste0(escolhas, registro[[colunaDeEscolha]])
                                    } 

                                    # Item marcado na questão corrente
                                    item_marcado <- substr(escolhas, opcaoEscolhidaDaMedidaCorrente, opcaoEscolhidaDaMedidaCorrente)

                                    # Verifica se está no contexto de uma instituição e se a instituição corrente é a princial
                                    if(ehContextoDaInstituicaoPrincial)
                                    {
                                        #Contabiliza respostas da Instituição principal
                                        if (item_marcado == "A")
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["A"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["A"]] + 1
                                        }
                                        else if (item_marcado == "B")
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["B"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["B"]] + 1
                                        }
                                        else if (item_marcado == "C")
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["C"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["C"]] + 1
                                        }
                                        else if (item_marcado == "D")
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["D"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["D"]] + 1
                                        }
                                        else if (item_marcado == "E")
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["E"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["E"]] + 1
                                        }
                                        else
                                        {
                                            listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["N"]] <- listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["N"]] + 1
                                        }

                                        totalDeAlunosNaInstituicaoPrincipal <- totalDeAlunosNoContexto
                                    }
                                    if (item_marcado == item_correto)
                                    {
                                        # Conta na lista de questões
                                        listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]] <- listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]] + 1
                                    }
                                }

                                listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]] <- round(listaDeAcertosObjetivas[[siglaDaOpcaoDaDimensaoParaRotulo]][[opcaoEscolhidaDaMedidaCorrente]]/totalDeAlunosNoContexto, digits=2)*100

                                if(ehContextoDaInstituicaoPrincial)
                                {
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["A"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["A"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["B"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["B"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["C"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["C"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["D"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["D"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["E"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["E"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                    listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["N"]] <- round(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][["N"]]/totalDeAlunosNaInstituicaoPrincipal, digits=2)*100
                                }
                            }
                        }
                    }
                }

                # Aplica o filtro da dimensão corrente aos microdadosFiltrados
                if(length(opcoesEscolhidasDaDimensaoCorrente) > 0)
                {
                    microdadosFiltrados <- microdadosFiltrados %>% filter(UQ(as.name(nomeDaColunaDaDimensaoID)) %in% opcoesEscolhidasDaDimensaoCorrente)
                }
            }

            ############################################ FIMA DA DIMENSÕES/CÁLCULOS: ############################################
            
            ###################################################### ESTRUTURAÇÃO: ################################################

            nomes_grafico_base_discursivas <- unlist(lapply(rotulosDasBarrasDosGraficos, function(nome) {
                rep(nome, 2)
            }))

            plot_output_list_discursivas <- list()
            for(nomeDaMedidaDiscursiva in NOMES_DAS_DIMENSOES_DE_QUESTOES_DISCURSIVAS)
            {
                nomeDaColunaDaDimensaoDiscursivaID <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaDiscursiva,".ID")]]
                dimensaoDiscursivaCATEGORIA <- ""
                if(paste0(nomeDaMedidaDiscursiva,".CATEGORIA") %in% names(COLUNAS_POR_DIMENSAO))
                {
                    dimensaoDiscursivaCATEGORIA <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaDiscursiva,".CATEGORIA")]]
                }

                dimensaoDiscursivaNOME <- ""
                if(paste0(nomeDaMedidaDiscursiva,".NOME") %in% names(COLUNAS_POR_DIMENSAO))
                {
                    dimensaoDiscursivaNOME <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaDiscursiva,".NOME")]]
                }

                # Seleciona as opções escolhidas no parâmetro correspondente à medida corrente
                opcoesEscolhidasDaMedidaDiscursivaCorrente <- input[[nomeDaMedidaDiscursiva]]
                # Para cada opção escolhida da medida corrente
                plot_output_list_discursivas <- lapply(opcoesEscolhidasDaMedidaDiscursivaCorrente, function(opcaoEscolhidaDaMedidaDiscursivaCorrente) {

                    plotname <- paste0("plot", opcaoEscolhidaDaMedidaDiscursivaCorrente)

                    plot_output_object_discursiva <- plotOutput(plotname)

                    plot_output_object_discursiva <- renderPlot({
                        mediasDaQuestaoDiscursivaCorrente <- list()

                        for(contextoDiscursiva in contextosDasBarrasDosGraficosDiscursivas[[opcaoEscolhidaDaMedidaDiscursivaCorrente]])
                        {
                            mediasDaQuestaoDiscursivaCorrente <- c(mediasDaQuestaoDiscursivaCorrente, mediasDiscursivas[[contextoDiscursiva]][[opcaoEscolhidaDaMedidaDiscursivaCorrente]])
                        }
                        
                        dadosDoGraficoDiscursiva <- data.frame(
                            Categoria = nomes_grafico_base_discursivas,
                            Subcategoria = c(rep(c("Com Nota", "Sem Nota"), (length(nomes_grafico_base_discursivas)/2))),
                            valores = as.numeric(c(paste(mediasDaQuestaoDiscursivaCorrente)))
                        )

                        dadosDoGraficoDiscursiva$Categoria <- factor(dadosDoGraficoDiscursiva$Categoria, levels = unique(dadosDoGraficoDiscursiva$Categoria))

                        graficoDiscursivas <<- ggplot(dadosDoGraficoDiscursiva, aes(x=Categoria, y=valores, fill=Subcategoria)) 
                        categoriaDaQuestaoDiscursiva <- ""
                        if(dimensaoCATEGORIA != "")
                        {
                            opcaoEscolhidaDaMedidaDiscursivaCorrenteObjeto <- dimensoes[[nomeDaMedidaDiscursiva]] %>% filter(UQ(as.name(nomeDaColunaDaDimensaoDiscursivaID)) == opcaoEscolhidaDaMedidaDiscursivaCorrente)
                            categoriaDaQuestaoDiscursiva <- paste(opcaoEscolhidaDaMedidaDiscursivaCorrenteObjeto[[dimensaoDiscursivaNOME]], opcaoEscolhidaDaMedidaDiscursivaCorrenteObjeto[[dimensaoDiscursivaCATEGORIA]]) 
                        }

                        graficoDiscursivas + 
                        ggtitle(categoriaDaQuestaoDiscursiva) +
                        geom_bar(stat="identity", width=0.5, colour="black", position=position_dodge()) +
                        geom_text(aes(label=valores), vjust=-1, position = position_dodge(0.5), size=3.5) +
                        scale_fill_brewer(palette="Paired") +
                        theme_minimal() +
                        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                        xlab("") +
                        scale_y_continuous(name="Nota Média", limits=c(0, 100)) +
                        guides(fill=guide_legend(title="Legenda"))
                    })

                })
            }
            plot_output_list_objetivas <- list()
            
            for(nomeDaMedidaObjetiva in NOMES_DAS_DIMENSOES_DE_QUESTOES_OBJETIVAS)
            {
                nomeDaColunaDaDimensaoID <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaObjetiva,".ID")]]
             
                dimensaoCATEGORIA <- ""
                if(paste0(nomeDaMedidaObjetiva,".CATEGORIA") %in% names(COLUNAS_POR_DIMENSAO))
                {
                    dimensaoCATEGORIA <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaObjetiva,".CATEGORIA")]]
                }

                dimensaoNOME <- ""
                if(paste0(nomeDaMedidaObjetiva,".NOME") %in% names(COLUNAS_POR_DIMENSAO))
                {
                    dimensaoNOME <- COLUNAS_POR_DIMENSAO[[paste0(nomeDaMedidaObjetiva,".NOME")]]
                }

                # Seleciona as opções escolhidas no parâmetro correspondente à medida corrente
                opcoesEscolhidasDaMedidaCorrente <- input[[nomeDaMedidaObjetiva]]
                # Para cada opção escolhida da medida corrente
                plot_output_list_objetivas <- lapply(opcoesEscolhidasDaMedidaCorrente, function(opcaoEscolhidaDaMedidaCorrente) {
                        
                    plotname <- paste0("plot", opcaoEscolhidaDaMedidaCorrente)
                    plot_output_object <- plotOutput(plotname)
                    plot_output_object <- renderPlot({
                        
                        mediasDaQuestaoCorrente <- list()

                        for(contexto in contextosDasBarrasDosGraficosObjetivas[[opcaoEscolhidaDaMedidaCorrente]])
                        {
                            mediasDaQuestaoCorrente <- c(mediasDaQuestaoCorrente, listaDeAcertosObjetivas[[contexto]][[opcaoEscolhidaDaMedidaCorrente]])
                        }

                        for(erroObjetiva in names(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]]))
                        {
                            mediasDaQuestaoCorrente <- c(mediasDaQuestaoCorrente, listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]][[erroObjetiva]])
                        }

                        rotulosDasBarrasDosGraficos <- c(rotulosDasBarrasDosGraficos, names(listaDeErrosObjetivas[[opcaoEscolhidaDaMedidaCorrente]]))

                        dadosDoGrafico <- data.frame(
                            Categoria = rotulosDasBarrasDosGraficos,
                            valores = as.numeric(c(paste(mediasDaQuestaoCorrente)))
                        )

                        dadosDoGrafico$Categoria <- factor(dadosDoGrafico$Categoria, levels = unique(dadosDoGrafico$Categoria))
                        
                        grafico <<- ggplot(dadosDoGrafico, aes(x=Categoria, y=valores)) 
                        
                        categoriaDaQuestaoObjetiva <- ""
                        if(dimensaoCATEGORIA != "")
                        {
                            opcaoEscolhidaDaMedidaObjetivaCorrenteObjeto <- dimensoes[[nomeDaMedidaObjetiva]] %>% filter(UQ(as.name(nomeDaColunaDaDimensaoID)) == opcaoEscolhidaDaMedidaCorrente)

                            opcaoCorreta <- gabaritoPorQuestao[[opcaoEscolhidaDaMedidaCorrente]];
                            print(opcaoCorreta)
                            if (!(opcaoCorreta %in% c("A", "B", "C", "D", "E")))
                            {
                                print(opcaoCorreta)
                                if(opcaoCorreta == "Z")
                                {
                                    opcaoCorreta <- "Questão excluída devido a anulação"
                                }
                                else if(opcaoCorreta == "X")
                                {
                                    opcaoCorreta <- "Questão excluída devido ao coeficiente pontobisserial menor que 0,20"
                                }
                                else if(opcaoCorreta == "N")
                                {
                                    opcaoCorreta <- "Questão não se aplica ao grupo de curso"
                                }
                            }
                            
                            print(opcaoCorreta)
                            categoriaDaQuestaoObjetiva <- paste0(opcaoEscolhidaDaMedidaObjetivaCorrenteObjeto[[dimensaoNOME]], "(", opcaoCorreta, ") ", opcaoEscolhidaDaMedidaObjetivaCorrenteObjeto[[dimensaoCATEGORIA]]) 
                        }   

                        grafico + 
                        ggtitle(categoriaDaQuestaoObjetiva) +
                        geom_bar(stat="identity", width=0.5, color="black", position=position_dodge(), fill="steelblue") +
                        geom_text(aes(label=valores), vjust=-1, position = position_dodge(0.5), size=3.5) +
                        theme_minimal() +
                        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                        xlab("") +
                        scale_y_continuous(name="Acertos (%)", limits=c(0, 100))                        
                    })
                })
            }

            plot_output_list <- NULL
            if(isTruthy(plot_output_list_discursivas))
            {
                plot_output_list <- plot_output_list_discursivas
            }

            if(isTruthy(plot_output_list_objetivas))
            {
                plot_output_list <- c(plot_output_list, plot_output_list_objetivas)
            }

            if(isTruthy(plot_output_list)) 
            {  
                do.call(tagList, plot_output_list)
            }

            ##################################################FIM DA ESTRUTURAÇÃO: ##############################################
        })
    })})
}
