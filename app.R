# app.R — paste this in a new file called app.R in the same folder
library(shiny)
library(plotly)
library(dplyr)
library(tibble)

# ── Load data ─────────────────────────────────────────────────────────────────
ALL_DATA    <- readRDS("Data/spi_results.rds")
YEARS       <- sort(unique(ALL_DATA$year))
MONTH_NAMES <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

classify_spi <- function(val) {
  dplyr::case_when(
    is.na(val)   ~ "No Data",
    val >  2.0   ~ "Extremely Wet",
    val >  1.5   ~ "Severely Wet",
    val >  1.0   ~ "Moderately Wet",
    val >= -1.0  ~ "Near Normal",
    val >= -1.5  ~ "Moderately Dry",
    val >= -2.0  ~ "Severely Dry",
    TRUE         ~ "Extremely Dry"
  )
}

CLASS_CONFIG <- tibble::tibble(
  label = c("Extremely Wet","Severely Wet","Moderately Wet",
            "Near Normal","Moderately Dry","Severely Dry",
            "Extremely Dry","No Data"),
  color = c("#0033CC","#2196F3","#81D4FA",
            "#D8D8C8","#FFD600","#FF5722",
            "#7B0000","#666666")
)
COLOR_MAP <- setNames(CLASS_CONFIG$color, CLASS_CONFIG$label)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tags$head(tags$style(HTML("
    body { background: #0D1117; color: #E6EDF3; }
    .stat-card { background: #161B22; border: 1px solid #30363D;
                 border-radius: 8px; padding: 9px 18px; min-width: 130px;
                 display: inline-block; margin-right: 10px; }
    .stat-lbl  { font-size: 10px; color: #8B949E; letter-spacing: 1px;
                 text-transform: uppercase; }
    .stat-val  { font-size: 22px; font-weight: 700; margin: 2px 0; }
    .legend-row { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 12px; }
    .legend-item { display: flex; align-items: center; gap: 5px;
                   font-size: 11px; color: #8B949E; }
    .legend-box  { width: 14px; height: 14px; border-radius: 3px;
                   border: 1px solid #30363D; display: inline-block; }
    select { background: #161B22 !important; color: #E6EDF3 !important;
             border: 1px solid #30363D !important; }
  "))),
  
  titlePanel("Argentina · Standardized Precipitation Index"),
  
  # Controls row
  fluidRow(
    column(2, selectInput("year",  "YEAR",
                          choices = setNames(YEARS, YEARS), selected = max(YEARS))),
    column(2, selectInput("month", "MONTH",
                          choices = setNames(1:12, MONTH_NAMES), selected = 3)),
    column(2, selectInput("spi_index", "SPI INDEX",
                          choices = c("SPI-3" = "spi3", "SPI-6" = "spi6", "SPI-12" = "spi12"),
                          selected = "spi3")),
    column(6,
           div(class = "stat-card",
               div(class = "stat-lbl", "Mean SPI"),
               div(class = "stat-val", uiOutput("stat_mean_ui", inline = TRUE))
           ),
           div(class = "stat-card",
               div(class = "stat-lbl", "Dry Area (SPI < −1)"),
               div(class = "stat-val", style = "color:#FF5722",
                   textOutput("stat_dry", inline = TRUE))
           ),
           div(class = "stat-card",
               div(class = "stat-lbl", "Wet Area (SPI > +1)"),
               div(class = "stat-val", style = "color:#2196F3",
                   textOutput("stat_wet", inline = TRUE))
           ),
           div(class = "stat-card",
               div(class = "stat-lbl", "Mean Precip (mm)"),
               div(class = "stat-val", style = "color:#81D4FA",
                   textOutput("stat_precip", inline = TRUE))
           )
    )
  ),
  
  br(),
  plotlyOutput("spi_map", height = "580px"),
  
  # Legend
  div(class = "legend-row",
      lapply(seq_len(nrow(CLASS_CONFIG)), function(i) {
        div(class = "legend-item",
            span(class = "legend-box",
                 style = paste0("background:", CLASS_CONFIG$color[i], ";")),
            CLASS_CONFIG$label[i]
        )
      })
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  filtered <- reactive({
    idx <- input$spi_index
    ALL_DATA |>
      filter(year == as.integer(input$year),
             month == as.integer(input$month)) |>
      mutate(
        spi_val = .data[[idx]],
        class   = factor(classify_spi(spi_val), levels = CLASS_CONFIG$label)
      )
  })
  
  output$stat_mean_ui <- renderUI({
    m   <- mean(filtered()$spi_val, na.rm = TRUE)
    col <- if (m < -0.5) "#FF5722" else if (m > 0.5) "#2196F3" else "#E6EDF3"
    tags$span(style = paste0("color:", col),
              formatC(m, digits = 2, format = "f"))
  })
  
  output$stat_dry <- renderText({
    v <- filtered()$spi_val
    paste0(formatC(mean(v < -1, na.rm = TRUE) * 100,
                   digits = 1, format = "f"), "%")
  })
  
  output$stat_wet <- renderText({
    v <- filtered()$spi_val
    paste0(formatC(mean(v >  1, na.rm = TRUE) * 100,
                   digits = 1, format = "f"), "%")
  })
  
  output$stat_precip <- renderText({
    formatC(mean(filtered()$monthly_precip, na.rm = TRUE),
            digits = 1, format = "f")
  })
  
  output$spi_map <- renderPlotly({
    df        <- filtered()
    idx       <- input$spi_index
    spi_label <- switch(idx, spi3="SPI-3", spi6="SPI-6", spi12="SPI-12")
    
    present <- levels(droplevels(df$class))
    pal     <- COLOR_MAP[present]
    
    df <- df |>
      mutate(hover_txt = paste0(
        "<b>", class, "</b><br>",
        spi_label, ": <b>", round(spi_val, 2), "</b><br>",
        "Precip: <b>", round(monthly_precip, 1), " mm</b><br>",
        "Lon: ", x, "°W | Lat: ", y, "°S"
      ))
    
    plot_ly(data=df, x=~x, y=~y, color=~class, colors=pal,
            type="scatter", mode="markers",
            marker=list(symbol="square", size=6, line=list(width=0)),
            text=~hover_txt, hoverinfo="text", showlegend=FALSE) |>
      layout(
        title = list(text = paste0(spi_label, " — ",
                                   MONTH_NAMES[as.integer(input$month)], " ", input$year),
                     font = list(size=15, color="#E6EDF3"), x=0.01),
        paper_bgcolor = "#0D1117", plot_bgcolor = "#1C2128",
        font  = list(color="#E6EDF3"),
        xaxis = list(title="Longitude (°W)", gridcolor="rgba(255,255,255,0.05)",
                     zeroline=FALSE),
        yaxis = list(title="Latitude (°S)",  gridcolor="rgba(255,255,255,0.05)",
                     zeroline=FALSE, scaleanchor="x", scaleratio=1),
        hoverlabel = list(bgcolor="#161B22", bordercolor="#30363D",
                          font=list(color="#E6EDF3", size=12)),
        margin = list(l=60, r=20, t=50, b=60)
      )
  })
}

shinyApp(ui, server)
