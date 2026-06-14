library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(readxl)
# untuk lokasi data cuaca sesuaikan dengan path data cuaca di simpan 
data_cuaca <- read_excel("DataCuaca.xlsx")

ui <- fluidPage(
  titlePanel("Aplikasi Interaktif Visualisasi Data Cuaca"),
  sidebarLayout(
    sidebarPanel(
      selectInput("jenis_plot", "1. Pilih Jenis Visualisasi:",
                  choices = c("a. Scatter Plot Interaktif" = "scatter",
                              "b. Line Plot Interaktif" = "line",
                              "c. Bar Plot Interaktif" = "bar",
                              "d. Tabel Data" = "table")),
      conditionalPanel(
        condition = "input.jenis_plot != 'table'",
        selectInput("variabel_x", "2. Pilih Variabel Sumbu X:", 
                    choices = names(data_cuaca), selected = names(data_cuaca)[1]),
        selectInput("variabel_y", "3. Pilih Variabel Sumbu Y:", 
                    choices = names(data_cuaca), selected = names(data_cuaca)[2])
      ),
      
      hr(),
      helpText("Catatan: Untuk Line Plot dan Scatter plot, pilih dua variabel bertipe numerik untuk hasil terbaik.")
    ),
    
    mainPanel(
      plotlyOutput("plot_tampil"),
      DTOutput("tabel_tampil")
    )
  )
)

server <- function(input, output, session) {
  output$plot_tampil <- renderPlotly({
    if (input$jenis_plot == "table") return(NULL)
    p <- ggplot(data_cuaca, aes(x = .data[[input$variabel_x]], y = .data[[input$variabel_y]]))
    
    if (input$jenis_plot == "scatter") {
      p <- p + geom_point(color = "dodgerblue", alpha = 0.7) + 
        theme_minimal() + 
        labs(title = paste("Scatter Plot:", input$variabel_x, "vs", input$variabel_y))
    } else if (input$jenis_plot == "line") {
      p <- p + geom_line(color = "tomato", group = 1) + 
        theme_minimal() + 
        labs(title = paste("Line Plot:", input$variabel_x, "vs", input$variabel_y))
    } else if (input$jenis_plot == "bar") {
      p <- p + geom_col(fill = "seagreen") + 
        theme_minimal() + 
        labs(title = paste("Bar Plot:", input$variabel_x, "vs", input$variabel_y))
    }
    ggplotly(p)
  })
  output$tabel_tampil <- renderDT({
    if (input$jenis_plot == "table") {
      datatable(data_cuaca, 
                options = list(pageLength = 10, scrollX = TRUE),
                caption = "Tabel Data Cuaca")
    }
  })
}
shinyApp(ui = ui, server = server)
