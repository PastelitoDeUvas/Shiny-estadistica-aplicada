# LIBRARY ######################
# Cargar librerías necesarias

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("forestmodel", "see", "jtools", "olsrr", "parameters", "stats", 
              "ggplot2", "plot3D", "plot3Drgl", "apaTables", "gvlma", 
              "performance", "lmtest", "readxl", "tidyverse", "modelsummary", 
              "cowplot", "gridExtra", "dplyr", "caret", "car","shiny",
              "shinyWidgets","plotly","bslib","DT","kableExtra","plotly")
ipak(packages)

#🌸 Tema pastel para la app
tema_cute <- bs_theme(
  bootswatch = "minty",  
  primary = "#FFB6C1",  
  secondary = "#AEEEEE",  
  base_font = font_google("Poppins"),
  heading_font = font_google("Dancing Script")
)


# DATA SETS ######################

#Punto 1

datos_punto1 <- read_excel("C:\\Users\\JUAN\\Desktop\\R\\Proyecto corte 1\\Repository\\Shiny-estadistica-aplicada\\Data sets\\datos_punto1.xlsx")
 
#Punto 2
datos_punto2 <- read_excel("C:\\Users\\JUAN\\Desktop\\R\\Proyecto corte 1\\Repository\\Shiny-estadistica-aplicada\\Data sets\\datos_punto2.xlsx")

#Punto 3
datos_punto3 <- read_excel("C:\\Users\\JUAN\\Desktop\\R\\Proyecto corte 1\\Repository\\Shiny-estadistica-aplicada\\Data sets\\datos_punto3.xlsx")

#Punto 4
datos_punto4 <- read_excel("C:\\Users\\JUAN\\Desktop\\R\\Proyecto corte 1\\Repository\\Shiny-estadistica-aplicada\\Data sets\\datos_punto4.xlsx")

# UI ################################
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .tabla-container {
        display: inline-block; /* Permite que el contenedor crezca según el contenido */
        width: auto; /* Se ajusta al tamaño de la tabla */
        overflow-x: auto;
        background-color: #FFF0F5;
        padding: 10px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
      }
      .tabla-container table {
        width: 100% !important;
      }
      .dataTables_wrapper {
        width: 100% !important;
      }
    "))
  ),
  
  # THEME ###########################
  
  
  
  
  theme = tema_cute,
  titlePanel("Evaluación Regresión Lineal"),
  
  
  
  # TABLA DE CONTENIDOS ###########################
  
  fluidRow(
    column(
      width = 3,
      div(
        style = "position: fixed; width: 20%; height: 100%; background-color: #FFE4E1; padding: 15px; border-radius: 10px;",
        h3("📖 Tabla de Contenidos"),
        tags$ul(
          
          tags$li(tags$a(href = "#punto1", "Punto 1", style = "text-decoration: none; color: inherit;")),
          tags$ul(  # Subtemas de Punto 1
            tags$li(tags$a(href = "#punto1_A", "A.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_B", "B.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_C", "C.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_D", "D.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_E", "E.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_F", "F.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_G", "G.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_H", "H.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto1_I", "I.", style = "text-decoration: none; color: inherit;"))
          ),
          
          tags$li(tags$a(href = "#punto2", "Punto 2", style = "text-decoration: none; color: inherit;")),
          tags$ul(  # Subtemas de Punto 2
            tags$li(tags$a(href = "#punto2_A", "A.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto2_B", "B.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto2_C", "C.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto2_D", "D.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto2_E", "E.", style = "text-decoration: none; color: inherit;"))
          ),
          
          tags$li(tags$a(href = "#punto3", "Punto 3", style = "text-decoration: none; color: inherit;")),
          tags$ul(  # Subtemas de Punto 3
            tags$li(tags$a(href = "#punto3_A", "A.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto3_B", "B.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto3_C", "C.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto3_D", "D.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto3_E", "E.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto3_F", "F.", style = "text-decoration: none; color: inherit;"))
          ),
          
          tags$li(tags$a(href = "#punto4", "Punto 4", style = "text-decoration: none; color: inherit;")),
          tags$ul(  # Subtemas de Punto 4
            tags$li(tags$a(href = "#punto4_A", "A.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto4_B", "B.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto4_C", "C.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto4_D", "D.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto4_E", "E.", style = "text-decoration: none; color: inherit;")),
            tags$li(tags$a(href = "#punto4_F", "F.", style = "text-decoration: none; color: inherit;"))
          )
        )
      )
    ),
    
    
    column(
      width = 8, offset = 4, # Ajusta para que no se sobreponga con la TOC
      
      
      # Punto 1 ####
      div(id = "punto1",
        h2("Punto 1."),
        p("Una empresa ha registrado las utilidades (Y) durante diez años de operación, también
ha estimado la participación en el mercado (X1) y los descuentos concedidos(X2). Los
siguientes son los datos registrados:")
      ),
      sidebarLayout(
        sidebarPanel(
          h3("💖 Filtrar por X1"),
          selectInput("filtro_x1", "Selecciona X1:", choices = NULL, multiple = TRUE),
          actionButton("reset1", "Restablecer filtros", style = "background-color:#FF69B4; color:white;")
        ),
    
        mainPanel(
          div(class = "tabla-container", DTOutput("tabla_cute"))
          
        )
      ),
      
      ## A.####
      
      
      
      
      
      
      
      
      # Punto 2####
      
      div(id = "punto2",
        h2("Punto 2."),
        p("¿Cuánto tiempo por semana invierte un estudiante de una universidad local en alguna
práctica deportiva? ¿El rendimiento académico afecta esta práctica? Para resolver
estos interrogantes, el director de bienestar de esta universidad hace un seguimiento a
una muestra aleatoria de 20 estudiantes escogidos de la jornada diurna. Se
consideraron las siguientes variables: Y: Tiempo, en horas, que un estudiante realiza
alguna actividad deportiva, ��: Numero de créditos matriculados por semestre, ��:
Promedio acumulado, ��: tiempo, en horas que dedica al ocio. Los datos son los
siguientes:"),
      ),
        
      sidebarLayout(
        sidebarPanel(
          h3("Filtrar por Horas de Actividad Deportiva"),
          selectInput("filtro_Horas_de_Actividad_Deportiva", "Selecciona Horas de Actividad Deportiva:", choices = NULL, multiple = TRUE),
          actionButton("reset2", "Restablecer filtros", style = "background-color:#FF69B4; color:white;")
        ),
          
        mainPanel(
          div(class = "tabla-container", DTOutput("tabla_Punto_2"))
          
        )
      ),
      
      ## A. ####
      
      div(id = "punto2_A",
          h2("A."),
          div(
            p("Ajuste un modelo de regresión lineal múltiple para la variable dependiente Y:
         Tiempo, en horas, que un estudiante realiza alguna actividad deportiva y las
         variables indicadas."),
            style = "background-color: #FFE4E1; padding: 15px; border-radius: 10px; border: 1px solid #F3A6C8;"
          ),
          p("El modelo de regresión lineal múltiple con Horas de Actividad Deportiva como variable 
        dependiente y el resto de variables como independientes es el siguiente:")
      ),
      sidebarLayout(
        sidebarPanel(
          h4(" Variables:"),
          uiOutput("texto_con_bullets")
        ),
        mainPanel(
          uiOutput("resumen_modelo") # Siempre visible
        )
      ),
      div(
        p("Se tiene el intercepto en 28.15755 horas, el coeficiente betta 1 como -0.91859 de Créditos
        matriculados, el coeficiente betta 2 como -2.39382 de PromedioAcumulado y betta 3 como 
        0.09359 de HorasOcio"),
      ),
      
      ## B. ####
      
      div(id = "punto2_B",
          h2("B."),
          div(
            p("Evalúe la significancia general del modelo encontrado en a) y la significancia de
cada variable ��, ¿Qué explicación le puede dar a este resultado?"),
            style = "background-color: #FFE4E1; padding: 15px; border-radius: 10px; border: 1px solid #F3A6C8;"
          ),
          p("Para ecaluar la significancia general del modelo se debe asegurar que no hay 
            multicolinealidad entre las variables independientes, esto se hace con el uso 
            de la función cor:")
      ),
      uiOutput("tabla_correlacion"),
      div(
        p("De la tabla generada por cor se puede apreciar que la correlación entre las variables
     independientes no llega a tener una magnitud de 0.7 o más, lo que significa que se puede 
     trabajar con ellas sin esperar problemas mayores por multicolinealidad.

     A su vez se puede destacar las relaciones individuales de cada variable
     independiente con la variable dependiente y resaltar que CréditosMatriculados y 
     PromedioAcumulado tienen cada uno una correlación de más de 0.7 con las HorasActividadDeportiva,
     lo cual es elevado y favorable, mientras que las HorasOcio cuenta con una correlación con las
     HorasActividadDeportiva de menos de 0.5, considerablemente menor que las otras dos.

     Las siguientes líneas de código resultan en cuatro gráficas:"),
        
        tags$ul(
          tags$li("La primera, Residual vs Fitted, sirve para constatar si la variable dependiente tiene una
             relación puramente lineal con las variables independientes. En este caso, la línea suavizada en
             rojo de los datos parece estar distribuida alrededor del cero con cierto patrón no aleatorio,
             lo que indica que la relación no es puramente lineal, pero no se ve tan afectada por las alinealidades."),
          
          tags$li("La segunda compara la distribución de los residuos estandarizados con la distribución normal
             teórica. Si los puntos se alinean aproximadamente con la línea diagonal, indica que los residuos
             se distribuyen cercanamente a la normal. En este caso, se ven ciertas desviaciones en las colas, pero
             no llegan a ser tan grandes como para afectar severamente la normalidad de los residuos."),
          
          tags$li("La tercera se usa para identificar heteroscedasticidad (varianza no constante de los errores) en los
             datos. En este caso, no se ve que este fenómeno ocurra de forma tan marcada, por lo que se pasará por 
             alto las implicaciones que pueda tener la pequeña aparente curvatura en la gráfica, que podría indicar
             cierta heteroscedasticidad."),
          
          tags$li("La cuarta relaciona los residuos estandarizados con la influencia (leverage) de cada observación.
             Además, aparecen curvas de Cook’s Distance que indican el nivel de influencia de cada punto en la
             estimación del modelo. En este caso, todos los residuos estándarizados y los niveles de importancia
             de las observaciones se encuentran por dentro de las curvas de Cook de 0.5, por lo que no se le
             dará mayor importancia en este caso a su influencia en el intercepto de la regresión.")
        ),
      ),
        
      fluidRow(
        column(
          width = 12, align = "center",
          div(
            h3("Diagnóstico del Modelo", style = "color: #D81B60;"),
            uiOutput("plotContainer"),  
            actionButton("reset", "Volver a los 4 gráficos", style = "margin-top: 10px;"),  
            style = "background-color: #FFE4E1; padding: 15px; border-radius: 10px; border: 1px solid #F3A6C8; margin-top: 20px;"
          )
        )
      ),
      
      ## C.####
    
      div(id = "punto2_C",
          h2("C."),
          div(
            p("Ajuste un modelo de regresión lineal múltiple sin problemas de variables no
significativas. Use � = 0.05."),
            style = "background-color: #FFE4E1; padding: 15px; border-radius: 10px; border: 1px solid #F3A6C8;"
          ),
      ),
      
      
      
      
      # Punto 3 ####
      
      div(id = "punto3",
          h2("Punto 3."),
          p("El gerente del Banco de la República de Colombia quiere desarrollar un modelo de
regresión para determinar el impacto que tienen algunas de las variables de producción
más importantes en el país sobre el Producto Interno Bruto (PIB). Este modelo serviría
para que el estado tome acciones sobre el sector que más influencia tiene en el PIB.
Las variables a considerar son: Producción total de azúcar, Producción de cemento gris,
Producción de lingotes de acero y Vehículos ensamblados. Los datos correspondientes
a estas variables y al PIB se encuentran al final de las preguntas.")
      ),
      
      sidebarLayout(
        sidebarPanel(
          h3("Filtrar por Periodo"),
          selectInput("filtro_Periodo", "Seleccione el periodo:", choices = NULL, multiple = TRUE),
          actionButton("reset3", "Restablecer filtros", style = "background-color:#FF69B4; color:white;")
        ),
        
        mainPanel(
          div(class = "tabla-container", DTOutput("tabla_Punto_3"))
          
        )
      ),
      
      
      
      
      # Punto 4 ####
      
      
      
    )
    
    
    
  ),
  
  
  

  
  
)


# SERVER ########################################

server <- function(input, output, session) {
  
  
  
  

  #Punto1 ####
  ## TABLA FILTRADA ##################################
  # 📌 Llenar opciones del selectInput con valores únicos de X1
  observe({
    updateSelectInput(session, "filtro_x1", choices = unique(datos_punto1$X1))
  })
  
  # 🌸 Filtrar datos según selección
  datos_filtrados_1 <- reactive({
    if (is.null(input$filtro_x1) || length(input$filtro_x1) == 0) {
      return(datos_punto1)
    }
    datos_punto1[datos_punto1$X1 %in% input$filtro_x1, ]
  })
  
  # 📊 Mostrar tabla cute con diseño
  output$tabla_cute <- renderDT({
    datatable(
      datos_filtrados_1(),
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE),
      class = "cell-border stripe hover"
    )
  })
  
  # 🔄 Restablecer filtros
  observeEvent(input$reset1, {
    updateSelectInput(session, "filtro_x1", selected = character(0))
  })
  
  ## A. ####
  
  
  
  
  
  
  # Punto 2. ####
  
  
  ## TABLA FILTRADA ##################################
  
  # 📌 Llenar opciones del selectInput con valores únicos de X1
  observe({
    updateSelectInput(session, "filtro_Horas_de_Actividad_Deportiva", choices = unique(datos_punto2$`Horas de Actividad Deportiva`))
  })
  
  # 🌸 Filtrar datos según selección
  datos_filtrados_2 <- reactive({
    if (is.null(input$filtro_Horas_de_Actividad_Deportiva) || length(input$filtro_Horas_de_Actividad_Deportiva) == 0) {
      return(datos_punto2)
    }
    datos_punto2[datos_punto2$`Horas de Actividad Deportiva` %in% input$filtro_Horas_de_Actividad_Deportiva, ]
  })
  
  # 📊 Mostrar tabla cute con diseño
  output$tabla_Punto_2 <- renderDT({
    datatable(
      datos_filtrados_2(),
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE),
      class = "cell-border stripe hover"
    )
  })
  
  # 🔄 Restablecer filtros
  observeEvent(input$reset2, {
    updateSelectInput(session, "filtro_Horas_de_Actividad_Deportiva", selected = character(0))
  })
  
  ## A.####
  
  ### Modelo ####
  
  modelo <- lm(`Horas de Actividad Deportiva` ~ `Créditos Matriculados` + `Promedio Acumulado` + `Horas de Ocio`, data = datos_punto2)
  
  output$resumen_modelo <- renderUI({
    tabla <- summary(modelo) %>%
      broom::tidy() %>%
      mutate(term = gsub("`|\\(|\\)", "", term)) %>%  # Elimina comillas y paréntesis
      kable("html", digits = 4, caption = "📊 Resumen del Modelo de Regresión") %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                    font_size = 14) %>%
      row_spec(0, bold = TRUE, color = "white", background = "#F3A6C8") %>%
      column_spec(1, bold = TRUE)
    
    HTML(tabla) # Renderizar como HTML
  })
  
  ### Bullets ####
  
  output$texto_con_bullets <- renderUI({
    HTML("
  <div style='border: 1px solid #ddd; padding: 10px; border-radius: 10px; background-color: #f8f8f8;'>
    <ul style='list-style-type: none; padding-left: 10px;'>
      <li>💖 <strong>Horas de Actividad Deportiva</strong>: Tiempo dedicado al deporte.</li>
      <li>💖 <strong>Créditos Matriculados</strong>: Cantidad de créditos inscritos.</li>
      <li>💖 <strong>Promedio Acumulado</strong>: Promedio de calificaciones.</li>
      <li>💖 <strong>Horas de Ocio</strong>: Tiempo libre disponible.</li>
    </ul>
  </div>
  ")
  })
  
  ### Grafico ####
  selectedPlot <- reactiveVal(NULL)  
  
  output$plotContainer <- renderUI({
    if (is.null(selectedPlot())) {
      plotlyOutput("diagnosticoModelo", height = "600px")
    } else {
      plotlyOutput("graficoSeleccionado", height = "600px")
    }
  })
  
  output$diagnosticoModelo <- renderPlotly({
    plots <- list()
    
    for (i in 1:4) {
      p <- ggplot(mtcars, aes(x = mpg, y = hp)) + 
        geom_point() + 
        ggtitle(paste("Gráfico", i))
      
      plots[[i]] <- ggplotly(p, source = "A") %>%  # 🔥 Se agrega source = "A"
        event_register("plotly_click")  # 🔥 Se registra el evento correctamente
    }
    
    subplot(plots, nrows = 2, shareX = TRUE, shareY = TRUE)
  })
  
  observeEvent(event_data("plotly_click"), {
    clickData <- event_data("plotly_click")
    if (!is.null(clickData)) {
      selectedPlot(clickData$curveNumber + 1)
    }
  })
  
  output$graficoSeleccionado <- renderPlotly({
    req(selectedPlot())
    
    p <- ggplot(mtcars, aes(x = mpg, y = hp)) + 
      geom_point() + 
      ggtitle(paste("Gráfico", selectedPlot()))
    
    ggplotly(p)
  })
  
  observeEvent(input$reset, {
    selectedPlot(NULL)
  })
  
  
  ## B. ####
  
  # Calcular la matriz de correlación (solo con variables numéricas)
  cor_datos_punto_2 <- cor(datos_punto2[, sapply(datos_punto2, is.numeric)], use = "complete.obs")
  
  # Convertir a tabla con kableExtra
  tabla_correlacion <- cor_datos_punto_2 %>%
    round(2) %>%  # Redondear a 2 decimales
    kable("html", caption = "📊 Matriz de Correlación") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  font_size = 14) %>%
    row_spec(0, bold = TRUE, color = "white", background = "#F3A6C8")  # Encabezado con color
  
  # Renderizar como HTML en Shiny
  output$tabla_correlacion <- renderUI({
    HTML(tabla_correlacion)
  })
  
  # Renderiza las graficas de correlacion
  output$diagnosticoModelo <- renderPlot({
    par(mfrow = c(2, 2))  # Divide el área de gráficos en 4
    plot(modelo)  # Genera los 4 gráficos de diagnóstico
  })
  
  ## C.####
  modelo2 <- lm(`Horas de Actividad Deportiva` ~ `Créditos Matriculados` + `Promedio Acumulado`, data = datos_punto2)
  
  
  # Punto 3. ####
  
  ## TABLA FILTRADA ##################################
  # 📌 Llenar opciones del selectInput con valores únicos de X1
  observe({
    updateSelectInput(session, "filtro_Periodo", choices = unique(datos_punto3$Periodo))
  })
  
  # 🌸 Filtrar datos según selección
  datos_filtrados_3 <- reactive({
    if (is.null(input$filtro_Periodo) || length(input$filtro_Periodo) == 0) {
      return(datos_punto3)
    }
    datos_punto3[datos_punto3$Periodo %in% input$filtro_Periodo, ]
  })
  
  # 📊 Mostrar tabla cute con diseño
  output$tabla_Punto_3 <- renderDT({
    datatable(
      datos_filtrados_3(),
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE),
      class = "cell-border stripe hover"
    )
  })
  
  # 🔄 Restablecer filtros
  observeEvent(input$reset3, {
    updateSelectInput(session, "filtro_Periodo", selected = character(0))
  })
  
  
  
  
  # Punto 4. ####
  
}

shinyApp(ui, server)




