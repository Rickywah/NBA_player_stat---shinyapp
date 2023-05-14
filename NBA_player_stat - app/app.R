#load packages
library(shiny)
library(tidyverse)

#load data
NBA = read.csv(file = "data/NBA.csv")

ui = navbarPage(title = "NBA Player",
                tabPanel(title = "Input / Visualization",
                         titlePanel(title = "2021-2022 NBA Player Regular Season Stat"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(
                               inputId = "tm",
                               label = "Team:",
                               choices = sort(unique(NBA$Team)),
                               selected = "LAL"),
                             selectInput(
                               inputId = "pos",
                               label = "Position:",
                               choices = sort(unique(NBA$Pos))),
                             selectInput(
                               inputId = "plyer",
                               label = "Player:",
                               choices = sort(unique(NBA$Player))),
                             checkboxInput(inputId = "Tam",
                                           label = "Filter Teams to Position",
                                           value = FALSE)
                           ),
                           mainPanel(plotOutput("plot"))
                         )),
                tabPanel(title = "Table", dataTableOutput("table")),
                tabPanel(title = 'About', includeMarkdown("about.Rmd"))
                )

server = function(input, output) {

  NBA_tm = reactive({
    NBA |>
      filter(Team == input$tm)
  })

  observeEvent(
    eventExpr = input$tm,
    handlerExpr = {
      updateSelectInput(inputId = "pos",
                        choices = sort(unique(NBA_tm()$Pos)))
      updateSelectInput(inputId = "plyer",
                        choices = sort(unique(NBA_tm_pos()$Player)))
    }
  )

  NBA_tm_pos = reactive({
    NBA_tm() |>
      filter(Pos == input$pos)
  })

  observeEvent(
    eventExpr = input$pos,
    handlerExpr = {
      updateSelectInput(inputId = "plyer",
                        choices = sort(unique(NBA_tm_pos()$Player)))
    }
  )

    output$plot = renderPlot({

      NBA |>
        filter(Team == input$tm) |>
        filter(Pos == input$pos) |>
        filter(Player == input$plyer) |>
        pivot_longer(PTS:BLK, names_to = 'Type', values_to = "Stat") |>
        group_by(Type) |>
        summarise(Stat = sum(Stat)) |>
        ggplot()+
        aes(x = Type, y = Stat, fill = Type) |>
        geom_bar(stat = "identity") +
        coord_polar(theta = "x", direction=1) +
        theme_bw()
    })

    output$table = renderDataTable({

      tab = NBA_tm() |>
        calc_EFF()

      if(input$Tam){
        tab = tab |>
          filter(Pos == input$pos)
      }

      tab

    })
}

shinyApp(ui = ui, server = server)

