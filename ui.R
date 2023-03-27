options(shiny.sanitize.errors = FALSE)
library(dplyr)
library(stringr)
library(rvest)
library(DT)
library(reactable)

#Pull functions
source(paste0("funcs.R"))


ui <- fluidPage(
  #Enlargen text
  tags$head(tags$style('
   body {
      font-size: 17.5px; 
   }'
  )),
  #Background color of selected rows in DataTables (doesn't work when pushed to AWS)
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: lightblue !important;}')),
  title = "NBA Playoff Tool",
  # App title ----
  titlePanel(
    h1("NBA Playoff Seedings Projection Tool")
  ),
  
  #Sidebar with inputs
  sidebarLayout(
    sidebarPanel(position = "left",
                 h4("Select Inputs"),
                 #Select NBA team
                 selectizeInput(
                   'team', label = "Team", choices = sort(team_names),
                   selected = NULL,
                   options = list(create = F,
                                  placeholder = 'Type or select an NBA Team',
                                  onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 #Conditional panel to input desired Seed. Only populated when 
                 #not in custom pick tab.
                 conditionalPanel(condition = "input.tabs != '<strong>Pick Remaining Games</strong>'",
                                  numericInput('dseed', label = "Desired Seed",  value = 10,
                                               min = 1, max = 10),
                                  ),
                 #Conditional panel to input Assumptions. Only populated when 
                 #not in custom pick tab.
                 conditionalPanel(condition = "input.tabs != '<strong>Pick Remaining Games</strong>'",
                                  radioButtons('ass', 'Pace Assumptions', 
                                               c('Current Pace' = 'proj_current',
                                                 '538 Projections' = 'proj_538'))),
                 p("See instructions for more information",
                   style = "font-size:12px;"),
                 actionButton("do", "Run", class = "btn-primary"),
                 p(style = "font-size:15px;text-align: left;padding-top:20px" ,
                   strong(a(href = "https://www.statswithsasa.com/", "Stats with Sasa"),
                   ))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(position = "right",
              #Tabs for each output
              tabsetPanel(id = "tabs",
                #Pre-built assumptions panel setup
                tabPanel(id = "test1",
                         strong("Pre-Built Assumptions"),
                         h4("Selected Team"),
                         reactableOutput("table_selected"),
                         br(),
                         htmlOutput("seed_label"),
                         reactableOutput("table_comparison"),
                         br(),
                         htmlOutput("info_output"),
                         br(),
                         htmlOutput("text_output")
                ),
                #Custom pick tab
                tabPanel(id = "test3",
                         strong("Pick Remaining Games"),
                         h4("Selected Team"),
                         reactableOutput("custom_selected"),
                         br(),
                         h4("Remaining Schedule"),
                         p('Double click an outcome cell to change projected result.
                           Press tab or click out of the cell to save your edit.'),
                         DT::dataTableOutput("custom_sched")),
                tabPanel(id = "test2",
                         strong("Current Standings"),
                         DT::dataTableOutput("table_standings")
                ),
                #Instructions panel
                tabPanel(id = "test4",
                         strong("Instructions"),
                         h3("Instructions"),
                         p("Welcome to the NBA Playoff Seedings Projection Tool! 
                           As the 2022-23 NBA playoffs approach, fans are starting to look ahead 
                           and speculate about their favorite team's potential playoff seed."),
                         p("This tool was designed to help you explore different playoff scenarios 
                           for your favorite team. It has two main features:"),
                         strong("1) Pre-Built Assumptions"),
                         p(style = "margin-top: 7.5px",
                           "The tool pulls the current standings from ",
                           a(href = "https://www.espn.com/nba/standings", "ESPN.com"),
                           "and calculates how many regular season games each team has remaining.
                           Using this information, it shows you what win percentage is needed 
                           for your favorite team to reach any given playoff seed (1-10)"),
                         p("You can choose from two", strong("simple assumptions"), "to determine how the 
                         remaining NBA games will play out:"),
                         tags$ul(
                           tags$li(strong("Current Pace:"), "Assume that
                                   all remaining teams" ,strong("maintain their current win pace"), 
                                   "until the end of the season."),
                           tags$li(strong("538 Projections:"), "Assume that
                                   all remaining teams end the season according to", 
                                   a(href = "https://projects.fivethirtyeight.com/2023-nba-predictions/", 
                                     strong("538 projections.")))
                           ),
                         br(),
                         strong("2) Custom Pick Remaining Games"),
                         p(style = "margin-top: 7.5px",
                         "After selecting a team, the tool displays their remaining schedule 
                         according to ESPN.com."),
                         p("To begin, the tool populates the remaining games
                           by assigning wins when the opponent has a lower current win percentage, 
                           and losses when their win percentage is higher."),
                         p("You can then", strong("edit this table"), "and", 
                           strong("self-assign Wins and Losses"), "as you see fit. To do so, 
                           simply", strong("double-click on the outcome you want to change,"), 
                           "make your edits,", strong("and then press tab or click outside the cell 
                                                         to save your changes.")),
                         p("After each W/L change, the tool will recalculate the selected team's
                           final record. Then, it will give a", strong("projected seed"), 
                           "by comparing the projected win pct to current standings."),
                         p("Enjoy exploring the different playoff scenarios for your 
                           favorite (and least favorite) team with this tool!"),
                         br(),
                         p(strong("P.S."), "If you like this app and are interested
                           in checking out more cool stuff,", strong("check out my personal site",
                           a(href="https://www.statswithsasa.com/", "Stats with Sasa."))),
                         p(strong("P.P.S."), "If you really, really liked the tool, you can also feel free to",
                           a(href = "https://www.buymeacoffee.com/statswithsasa", "buy me a coffee!"))
                )
              )
              
              
              
    )
  )
)
