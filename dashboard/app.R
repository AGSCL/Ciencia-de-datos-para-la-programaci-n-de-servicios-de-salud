library(shiny)
library(bslib)
library(dplyr)
library(rio)
library(ggplot2)
library(plotly)
library(DT)
library(DiagrammeR)

# Load data
datos_multidimensionales <- 'https://github.com/rlagosb/taller_eiv/raw/refs/heads/main/datos_multidimensionales/'
cubo <- rio::import(paste0(datos_multidimensionales, "Cubo_consultas_nuevas.xlsx"))

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
# FUNCIONES
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
# Función para graficar series
graficar_serie <- function(y) {
  cubo %>%
    dplyr::group_by(Periodo, Centro_siglas) %>%
    dplyr::summarise(
      suma_y = sum(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Periodo, y = suma_y, color = Centro_siglas, group = Centro_siglas)) +
    geom_line() +
    labs(
      title = paste("Gráfico de", gsub("_"," ",y), "por Periodo y Centro"),
      x = "Periodo",
      y = y
    ) +
    theme_minimal() +
    geom_text(aes(label = suma_y), vjust = -0.5)+
    theme_minimal(base_size = 18) # Cambia 14 por el tamaño deseado
}

# Función para graficar series por especialidad
graficar_serie_esp <- function(esp=NULL, y) {
  cubo %>%
    {
      if (!is.null(esp)) {
        dplyr::filter(., Especialidad == esp)
      } else {
        .
      }
    } %>%
    dplyr::group_by(Periodo, Centro_siglas) %>%
    dplyr::summarise(
      suma_y = sum(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Periodo, y = suma_y, color = Centro_siglas, group = Centro_siglas)) +
    geom_line() +
    labs(
      title = paste(gsub("_"," ",y), "por Periodo y Centro (",esp,")"),
      x = "Periodo",
      y = y
    ) +
    theme_minimal() +
    geom_text(aes(label = suma_y), vjust = -0.5)+
    theme_minimal(base_size = 18) # Cambia 14 por el tamaño deseado
}


graficar_brechas_centro_flex <- function(especialidad = NULL, anio = NULL) {
  # Inicio con todos los datos
  datos_filtrados <- cubo
  
  # Aplicar filtros solo si se especifican
  if (!is.null(especialidad)) {
    datos_filtrados <- datos_filtrados %>%
      dplyr::filter(Especialidad == especialidad)
  }
  
  if (!is.null(anio)) {
    datos_filtrados <- datos_filtrados %>%
      dplyr::filter(Año == anio)
  }
  
  # Resto del proceso
  datos_filtrados %>%
    dplyr::group_by(Periodo, Centro_siglas) %>%
    dplyr::summarise(
      Oferta_consultas = sum(Oferta_consultas, na.rm = TRUE),
      Consultas_solicitadas = sum(Consultas_solicitadas, na.rm = TRUE),
      Lista_espera_inicial = sum(Lista_espera_inicial, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Lista_espera_inicial = Lista_espera_inicial / 3) %>%
    dplyr::group_by(Centro_siglas) %>%
    dplyr::mutate(
      Demanda_acumulada = cumsum(Consultas_solicitadas),
      Oferta_acumulada = cumsum(Oferta_consultas)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Periodo = gsub("-", "\n", Periodo)) %>%
    ggplot() +
    geom_area(aes(x = Periodo, y = Demanda_acumulada + Lista_espera_inicial, group = 1), 
              alpha = 0.3, fill = "#FF7F7F") +
    geom_area(aes(x = Periodo, y = Lista_espera_inicial, group = 1), 
              alpha = 0.5, fill = "lightblue") +
    geom_line(aes(x = Periodo, y = Oferta_acumulada, group = 1), 
              linewidth = 1, color = "darkblue") +
    geom_point(aes(x = Periodo, y = Oferta_acumulada, group = 1), 
               color = "darkblue", size = 2) +
    facet_grid(~Centro_siglas) +
    labs(
      title = paste(
        "Brechas por Periodo",
        ifelse(
          !is.null(especialidad) & !is.null(anio),
          paste(" (", especialidad, " al ", anio, ")", sep = ""),
          ifelse(
            !is.null(anio) & is.null(especialidad),
            paste(" (al ", anio, ")", sep = ""),
            ifelse(
              is.null(anio) & !is.null(especialidad),
              paste(" (", especialidad, ")", sep = ""),
              ""
            )
          )
        ),
        sep = ""
      ),
      x = "Periodo",
      y = "Cantidad",
      fill = "Métricas",
      color = "Métricas",
      caption = "Área azul claro= Lista espera inicial; Área rojo claro= Demanda acumulada;\nOferta acumulada= Punto azul oscuro; Etiqueta azul oscura= Recuento oferta acumulada"
    ) +
    theme_minimal() +
    geom_text(aes(x = Periodo, y = Oferta_acumulada, group = 1,
                  label = Oferta_acumulada), 
              color = "darkblue", vjust = -0.5)+
    theme_minimal(base_size = 18) # Cambia 14 por el tamaño deseado
}


#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

ui <- page_navbar(
  title = "Análisis de Consultas Médicas",
  
  # Pestaña Esquema Conceptual
  nav_panel(
    title = "Esquema Conceptual",
    page_sidebar(
      DiagrammeR::grVizOutput("diagram", height = "600px")
    )
  ),
  
  
  # Pestaña Datos
  nav_panel(
    title = "Datos",
    page_sidebar(
      DTOutput("tabla")
    )
  ),
  # Pestaña Gráficos
  nav_panel(
    title = "Gráficos",
    page_sidebar(
      sidebar = sidebar(
        selectInput("serie", "Seleccionar Serie",
                    choices = c("Consultas_producidas", "Consultas_inasistencia",
                                "Oferta_consultas", "Consultas_solicitadas",
                                "Lista_espera_inicial")),
        selectInput("especialidad", "Filtrar por Especialidad",
                    choices = c("Todas", unique(cubo$Especialidad)))
      ),
      plotOutput("grafico")
    )
  ),
  # Pestaña Gráficos de Brechas
  nav_panel(
    title = "Gráficos de Brechas",
    page_sidebar(
      sidebar = sidebar(
        selectInput("especialidad_brecha", "Seleccionar Especialidad",
                    choices = c("Todas", unique(cubo$Especialidad))),
        selectInput("anio_brecha", "Seleccionar Año",
                    choices = c("Todos", unique(cubo$Año)))
      ),
      plotOutput("grafico_brechas")
    )
  )  
)

server <- function(input, output) {
  
  output$tabla <- renderDT({
    datatable(cubo)
  })
  
  output$diagram <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(paste0('
        digraph erDiagram {
          graph [layout = dot, rankdir = BT]
          node [fontname = Helvetica]
        
          // -------------------------------------------------------------------
          // Nivel 1 (más bajo): Atributos de Persona y SIC
          // -------------------------------------------------------------------
          subgraph nivel_1 {
            rank = min;

            // Atributos de Producción
            estab [label = "ID_estab", shape = ellipse]
            cne_15 [label = "CNE <15", shape = ellipse]
            cne_1564 [label = "CNE 15-64", shape = ellipse]
            mes [label = "Mes", shape = ellipse]
            anio [label = "Año", shape = ellipse]
            prest [label = "Prestación", shape = ellipse]
            
            // Atributos de Establecimiento
            estab [label = "ID_estab", shape = ellipse]
            
            // Atributos de Persona
            persona_FechaNac [label = "Fecha nac", shape = ellipse]
            persona_Sexo     [label = "Sexo", shape = ellipse]
        
            // Atributos de SIC
            sic_TipoPrest    [label = "Tipo prest", shape = ellipse]
            sic_PrestaMin    [label = "Presta_min", shape = ellipse]
            sic_Especialidad [label = "Especialidad", shape = ellipse]
            sic_FEntrada     [label = "F_entrada", shape = ellipse]
            sic_EstabOrig    [label = "Estab_orig", shape = ellipse]
            sic_EstabDest    [label = "Estab_destino", shape = ellipse]
            sic_IDEstablec   [label = "ID_estab", shape = ellipse]
          }
        
          // -------------------------------------------------------------------
          // Nivel 2: Entidades Persona y SIC
          // -------------------------------------------------------------------
          subgraph nivel_2 {
            rank = same;
            rel_Consulta   [label = "consulta", shape = diamond, style = invis]
            Establecimiento [label = "Establecimiento", shape = box]
            Produccion [label = "Producción", shape = box]
            Persona [label = "Persona", shape = box]
            SIC     [label = "Solicitud Interconsulta (SIC)", shape = box]
          }
        
          // Conexión de atributos con entidades
          Produccion -> cne_15 [arrowhead = none]
          Produccion -> cne_1564 [arrowhead = none]
          Produccion -> mes [arrowhead = none]
          Produccion -> anio [arrowhead = none]
          Produccion -> prest [arrowhead = none]
          
          Establecimiento -> estab [arrowhead = none]
        
          Persona -> persona_FechaNac [arrowhead = none]
          Persona -> persona_Sexo     [arrowhead = none]
        
          SIC -> sic_TipoPrest    [arrowhead = none]
          SIC -> sic_PrestaMin    [arrowhead = none]
          SIC -> sic_Especialidad [arrowhead = none]
          SIC -> sic_FEntrada     [arrowhead = none]
          SIC -> sic_EstabOrig    [arrowhead = none]
          SIC -> sic_EstabDest    [arrowhead = none]
          SIC -> sic_IDEstablec   [arrowhead = none]
        
          // -------------------------------------------------------------------
          // Nivel 3: Rombos (relaciones)
          // -------------------------------------------------------------------
          subgraph nivel_3 {
            rank = mid;
            //rel_Agregacion [label = "Análisis", shape = diamond]
            rel_Agregacion [label = "", shape = diamond, style = invis]
          }
        
          // Conexión de relaciones con entidades
          Persona -> rel_Consulta [arrowhead = none, style = invis]
          rel_Consulta -> SIC      [arrowhead = none, style = invis]
        
          subgraph nivel_4 {
            rankdir = TB;
                  SIC -> rel_Agregacion   [arrowhead = none, style = invis]
          }
        
          subgraph cluster_nivel_sup {
            style = "filled,rounded";
            color = gray;
            fillcolor = none;
            label = "Agregación";
            fontname = Helvetica;
        
            // Nivel 4: Atributos de Proyección Consultas
            subgraph nivel_4 {
              rank = same;
              pc_IDEstablec    [label = "ID_estab", shape = ellipse]
              pc_Especialidad  [label = "Especialidad", shape = ellipse]
              pc_ConsNuevas    [label = "Consultas nuevas", shape = ellipse]
              pc_Controles     [label = "Controles", shape = ellipse]
              pc_Total         [label = "Total", shape = ellipse]
            }
        
            // Nivel 5 (más alto): Entidad Proyección Consultas
            ProyCons [label = "Proyección Consultas", shape = box]
        
            // Conexión de atributos con Proyección Consultas
            ProyCons -> pc_IDEstablec   [arrowhead = none]
            ProyCons -> pc_Especialidad [arrowhead = none]
            ProyCons -> pc_ConsNuevas   [arrowhead = none]
            ProyCons -> pc_Controles    [arrowhead = none]
            ProyCons -> pc_Total        [arrowhead = none]
        
            rel_Agregacion -> ProyCons [arrowhead = none, style = invis]
        
          // -------------------------------------------------------------------
          // Nivel 6 (más alto): Entidad Proyección Consultas
          // -------------------------------------------------------------------
          subgraph nivel_6 {
            rank = max;
            ProyCons [label = "Proyección Consultas", shape = box]
            }
          }
        }
    '))
  })
  
  output$grafico <- renderPlot({
    if (input$especialidad == "Todas") {
      graficar_serie(input$serie)
    } else {
      graficar_serie_esp(input$especialidad, input$serie)
    }
  })
  output$grafico_brechas <- renderPlot({
    esp <- if(input$especialidad_brecha == "Todas") NULL else input$especialidad_brecha
    anio <- if(input$anio_brecha == "Todos") NULL else as.numeric(input$anio_brecha)
    graficar_brechas_centro_flex(esp, anio)
  })
}

shinyApp(ui, server)
