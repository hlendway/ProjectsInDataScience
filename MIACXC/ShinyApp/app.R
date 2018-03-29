library(shiny)
#install.packages('rsconnect')
library(ggplot2)
library(tidyverse)
library(forcats)
library(plotly)
library(gapminder)
library(scales)
library(ggrepel)
library(tree)
library(randomForest)
library(ggrepel)
library(markdown)
library(knitr)

# Hannah Updated 12/19/17

#shinyapps::deployApp('C:\\Users\\CPC24\\Documents\\HeatherRTrainingFun\\projects-in-ds')
#Set working directory in session to be project folder and restart session if issue
#setwd("C:/Users/CPC24/Documents/HeatherRTrainingFun/projects-in-ds/miacShinyApp")
miacData <- read_csv("./data/finalMIAC171101Final.csv")
wiscData <- read_csv("./data/wiscMeets.csv")
minnesota_xc <- read.csv("./data/miacNoTime171104.csv")

##Can we add an rmd tab/page - http://scottshepard.io/markdown-pages-as-tabs-in-shiny/
#Make titles first name last name

miacSchools = c("Augsburg", "Bethel", "Carleton", "Concordia Moorhead", "Gustavus Adolphus", "Hamline", "Macalester", "St. Benedict", "St. Catherine", "St. John's", "Saint Mary's", "St. Olaf", "St. Thomas")

miacFiltered <- miacData %>% 
  mutate(meet_date = as.Date(meet_date,format="%m/%d/%Y")) %>% 
  rbind(wiscData) %>% 
  mutate(meet_kilometers = ifelse((meet_name=="St. Olaf Invitational" & meet_year==2016 & race_group=="Women"), 6, meet_kilometers)) %>%
  mutate(meet_kilometers = ifelse((meet_name=="St. Kate's Invite" & meet_year==2014 & race_group=="Women"), 4, meet_kilometers)) %>%
  mutate(meet_kilometers = ifelse((meet_name=="MIAC Championships" & meet_year==2013 & race_group=="Women"), 6, meet_kilometers)) %>%
  mutate(team = ifelse(grepl("St Olaf", team), "St. Olaf", team)) %>%
  mutate(team = ifelse(grepl("St Catherine", team), "St. Catherine", team)) %>%
  mutate(team = ifelse(grepl("St Benedict", team), "St. Benedict", team)) %>%
  mutate(team = ifelse(grepl("St Thomas", team), "St. Thomas", team)) %>%
  mutate(team = ifelse(grepl("Saint John", team), "St. John's", team)) %>%
  mutate(team = ifelse(grepl("St Olaf", team), "St. Olaf", team)) %>%
  mutate(team = ifelse(grepl("Gustavus", team), "Gustavus Adolphus", team)) %>% 
  mutate(meet_name = ifelse(grepl("BluGold|Blugold", meet_name), "Blugold Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Jim Drews", meet_name), "UW La Crosse Jim Drews Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("SMU Alumni", meet_name), "SMU Alumni Invte", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Jim Drews", meet_name), "UW La Crosse Jim Drews Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("St. Bonifacius", meet_name), "St. Bonifacius Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Twilight", meet_name), "MSU - Moorhead Dragon Twilight", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Crown College Storm", meet_name), "Crown College Storm Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Running of the Cows", meet_name), "Running of the Cows", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("UMAC", meet_name), "UMAC CC Championships", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Augsburg", meet_name), "Augsburg Open", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Martin Luther", meet_name), "Martin Luther Invitational", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("Minnesota Intercollegiate Championships", meet_name), "MIAC Championships", meet_name)) %>% 
  mutate(meet_name = ifelse(grepl("NCAA Division III Central Region", meet_name), "DIII Central Region Championships", meet_name)) %>% 
  mutate(meet_name = as.factor(meet_name),
         meet_name = fct_recode(meet_name,`MIAC Championships`="MIAC   CC Championships",
                                `St. Olaf Invitational`="ST. OLAF INVITATIONAL",
                                `St. Olaf Invitational`="St Olaf Invitational",
                                `Roy Griak Invitational`="ROY GRIAK INVITATIONAL",
                                `St. Kate's Invite` = "St. Kates Invite")) %>% 
  filter(team %in% miacSchools) %>% 
  unite("meetNameDate",c("meet_name","meet_date"),sep=" - ",remove=FALSE) %>% 
  mutate(meetNameDate = as.factor(meetNameDate)) %>% 
  mutate(perKilometerTime = new_time/meet_kilometers) %>% 
  arrange(meet_date)

wisc_meets_no_time <-
  wiscData %>% 
  subset(select = - c(time)) %>% 
  rename("X" = "X1")

# miac cleaned data - includes 2013 conference
miacClean <- 
  minnesota_xc %>% 
  rbind(wisc_meets_no_time) %>% 
  mutate(name = trimws(name, "both")) %>% 
  mutate(team = trimws(team, "both")) %>%
  mutate(meet_name = trimws(meet_name, "both")) %>%
  mutate(team = gsub("\xa0", NA, team)) %>%
  mutate(team = ifelse(grepl("St Olaf", team), "St. Olaf", team)) %>%
  mutate(team = ifelse(grepl("St Catherine", team), "St. Catherine", team)) %>%
  mutate(team = ifelse(grepl("St Benedict", team), "St. Benedict", team)) %>%
  mutate(team = ifelse(grepl("St Thomas", team), "St. Thomas", team)) %>%
  mutate(team = ifelse(grepl("Saint John", team), "St. John's", team)) %>%
  mutate(team = ifelse(grepl("St Olaf", team), "St. Olaf", team)) %>%
  mutate(team = ifelse(grepl("Gustavus", team), "Gustavus Adolphus", team)) %>% 
  mutate(score = ifelse(score == 0, NA, score)) %>%
  mutate(class_year = as.character(class_year)) %>%
  mutate(class_year = gsub("\xa0", NA, class_year)) %>%
  mutate(class_year = substr(class_year,1,2),
         class_year = as.factor(toupper(class_year)),
         meet_name = ifelse((grepl("MIAC", meet_name)),"Minnesota Intercollegiate Championships",as.character(meet_name))) %>% 
  mutate(meet_kilometers = ifelse(meet_name == "Minnesota Intercollegiate Championships" & race_group == "Women", 6, meet_kilometers)) %>% 
  filter(team %in% miacSchools)

# women team rankings
miac_women_team_placings <-
  miacClean %>% 
  filter(race_group == "Women") %>%
  filter(grepl("Minnesota Intercollegiate Championships", meet_name)) %>%
  filter((meet_kilometers==6 | meet_kilometers==8)) %>%
  mutate(score = as.numeric(score)) %>%
  filter(!is.na(score)) %>%
  group_by(team, meet_year) %>%
  top_n(n = -5, wt = score) %>% 
  summarise(team_score = sum(score)) %>%
  group_by(meet_year) %>%
  arrange(team_score, desc(team)) %>%
  mutate(team_rank = row_number()) %>%
  arrange(meet_year)

# men team rankings
miac_men_team_placings <-
  miacClean %>%
  filter(race_group == "Men") %>%
  filter(grepl("Minnesota Intercollegiate Championships", meet_name)) %>%
  mutate(score = as.numeric(score)) %>%
  filter(!is.na(score)) %>%
  group_by(team, meet_year) %>%
  top_n(n = -5, wt = score) %>% 
  summarise(team_score = sum(score)) %>%
  group_by(meet_year) %>%
  arrange(team_score, desc(team)) %>%
  mutate(team_rank = row_number()) %>%
  arrange(meet_year)

# real miac results 2017
miac_2017_results <-
  miacClean %>% 
  filter(meet_year == 2017, meet_name == "Minnesota Intercollegiate Championships") %>% 
  select(name, place, new_time) %>% 
  rename('real_minutes' = new_time, 'real_place'= place)

# predictions data set
# fastest three times prior to conference
# if only ran one race - carry race times from meet1 to meet 2 and meet3
# if ran two races - average meet1 and meet2 times for meet3
miac_xc_model_set <-
  miacClean %>%
  filter(meet_kilometers==8 & race_group == "Men" | meet_kilometers==6 & race_group == "Women") %>%
  select(name,meet_name,meet_year,new_time,race_group,class_year,team) %>%
  group_by(name,meet_name,meet_year,class_year,race_group) %>%
  distinct(name,meet_name,meet_year,class_year,race_group, .keep_all= TRUE) %>%
  spread(meet_name,new_time,fill="") %>%
  unite("times",c(6:15,18:34),sep=",",remove=TRUE) %>%
  mutate(times = gsub("(,,*)",",",times), times = gsub("^,*|,*$","",times)) %>%
  separate(times,c("meet1","meet2","meet3","meet4","meet5"),sep=",",remove = TRUE, extra = "drop") %>% 
  select(-`NCAA Division III Central Region Cross Country Championships`) %>% 
  gather("meet","time",meet1,meet2,meet3,meet4,meet5) %>% 
  group_by(name,meet_year,class_year,race_group) %>%
  arrange(name,meet_year,class_year,race_group,time) %>% 
  select(-meet) %>% 
  mutate(meet = paste0("meet",row_number())) %>% 
  spread(meet,time) %>%
  select(-meet4, -meet5) %>%
  select(-`Minnesota Intercollegiate Championships`, everything()) %>% 
  filter(! meet1 == "") %>% 
  mutate(meet3 = ifelse((is.na(meet2)),meet1,meet3), meet2 = ifelse((is.na(meet2)),meet1,meet2)) %>%
  mutate(meet1=as.numeric(meet1),
         meet2=as.numeric(meet2),
         meet3 = ifelse((is.na(meet3)),((meet1+meet2)/2),as.numeric(meet3))) %>%
  filter(`Minnesota Intercollegiate Championships`!="") %>%
  rename(Minnesota.Intercollegiate.Championships=`Minnesota Intercollegiate Championships`) %>%
  mutate(Minnesota.Intercollegiate.Championships = as.numeric(Minnesota.Intercollegiate.Championships))

# predictions train, test, lm model, regression tree model, random forest model
RunnersTest <- 
  miac_xc_model_set %>% 
  filter(meet_year == 2017)
RunnersTrain <- dplyr::setdiff(miac_xc_model_set, RunnersTest)

# models: linear, regression tree, random forest
set.seed(34)
lm <- lm(Minnesota.Intercollegiate.Championships ~ race_group+class_year+meet1+meet2+meet3, RunnersTrain)
tree <- tree(Minnesota.Intercollegiate.Championships ~ race_group+class_year+meet1+meet2+meet3, RunnersTrain)
forest <- randomForest(Minnesota.Intercollegiate.Championships ~ race_group+class_year+meet1+meet2+meet3, RunnersTrain)

# predict MIAC times for runners in RunnersTrain
lmtrain <- predict(lm, RunnersTrain)
treetrain <- predict(tree, RunnersTrain)
foresttrain <- predict(forest, RunnersTrain)

RunnersTrain <- RunnersTrain %>% 
  cbind(lmTrain = lmtrain, treeTrain = treetrain, forestTrain = foresttrain) 

# predict MIAC times for runners in RunnersTest
lmtest <- predict(lm, RunnersTest)
treetest <- predict(tree, RunnersTest)
foresttest <- predict(forest, RunnersTest)

TestResults <- RunnersTest %>% 
  dplyr::select(c(name, team, meet_year, race_group, class_year, meet1, meet2, meet3, Minnesota.Intercollegiate.Championships)) %>% 
  cbind(lmTest = lmtest) %>% 
  cbind(treeTest = treetest) %>% 
  cbind(forestTest = foresttest) 

miac_women_team_placings_2017 <-
  miacClean %>%
  filter(grepl("Minnesota Intercollegiate Championships", meet_name)) %>% 
  filter(race_group == "Women", meet_year == 2017) %>%
  mutate(score = as.numeric(score)) %>% 
  filter(!is.na(score)) %>%
  group_by(team, meet_year) %>%
  top_n(n = -5, wt = score) %>%
  summarise(team_score = sum(score)) %>% 
  group_by(meet_year) %>%
  arrange(team_score, desc(team)) %>%
  mutate(team_rank = row_number()) %>%
  arrange(meet_year) %>% 
  rename("real_score"="team_score") %>%
  rename("real_rank"="team_rank")

miac_men_team_placings_2017 <-
  miacClean %>%
  filter(grepl("Minnesota Intercollegiate Championships", meet_name)) %>% 
  filter(race_group == "Men", meet_year == 2017) %>%
  mutate(score = as.numeric(score)) %>% 
  filter(!is.na(score)) %>%
  group_by(team, meet_year) %>%
  top_n(n = -5, wt = score) %>%
  summarise(team_score = sum(score)) %>% 
  group_by(meet_year) %>%
  arrange(team_score, desc(team)) %>%
  mutate(team_rank = row_number()) %>%
  arrange(meet_year) %>% 
  rename("real_score"="team_score") %>%
  rename("real_rank"="team_rank")

# linear prediction
miac_2017_prediction_placings_lm <-
  TestResults %>%
  arrange(race_group,lmTest) %>%
  group_by(race_group) %>%
  mutate(place = ave(lmTest, FUN=ordered)) %>% 
  left_join(miac_2017_results, by = c("name" = "name")) %>% 
  select(place, real_place, name, team, class_year, lmTest, real_minutes, race_group) %>%
  mutate(min_difference = round(real_minutes - lmTest, digits = 2)) %>%
  rename('predict_place' = place) %>%  
  rename('predict_minutes' = lmTest)

# regression tree prediction
miac_2017_prediction_placings_tree <-
  TestResults %>%
  arrange(race_group,treeTest) %>%
  group_by(race_group) %>%
  mutate(place = ave(treeTest, FUN=ordered)) %>% 
  left_join(miac_2017_results, by = c("name" = "name")) %>%
  select(place, real_place, name, team, class_year, treeTest, real_minutes, race_group) %>%
  mutate(min_difference = round(real_minutes - treeTest, digits = 2)) %>%
  rename('predict_place' = place) %>%  
  rename('predict_minutes' = treeTest)

# random forest prediction
miac_2017_prediction_placings_forest <-
  TestResults %>%
  arrange(race_group,forestTest) %>%
  group_by(race_group) %>%
  mutate(place = ave(forestTest, FUN=ordered)) %>% 
  left_join(miac_2017_results, by = c("name" = "name")) %>% 
  select(place, real_place, name, team, class_year, forestTest, real_minutes, race_group) %>%
  mutate(min_difference = round(real_minutes - forestTest, digits = 2)) %>%
  rename('predict_place' = place) %>%  
  rename('predict_minutes' = forestTest)

# MIAC champion, all conference, honorable mention
places = c("1", "15", "25")
miac_xc_championships <-
  miacFiltered %>% 
  filter(grepl("MIAC", meet_name)) %>%
  filter(place %in% places) %>%
  mutate(place_category = ifelse(as.numeric(place) == 15, "All Conference", "Honorable Mention")) %>% 
  mutate(place_category = ifelse(as.numeric(place) == 1, "Champion", place_category))  
miac_xc_championships2 <-
  miacFiltered %>%
  filter(grepl("MIAC", meet_name)) %>%
  filter(as.numeric(place) < 26) %>%
  mutate(place_category2 = ifelse(as.numeric(place) <= 15, "All Conference", "Honorable Mention")) %>%
  mutate(place_category2 = ifelse(as.numeric(place) == 1, "Champion", place_category2)) 


ui <- navbarPage("MIAC Cross Country",
                 tabPanel("Summary",
                          mainPanel(
                            includeMarkdown("MIACXCSummary.rmd")
                          )), 
                 navbarMenu("Runner Progression",
                            tabPanel("Team Progression",
                                     fluidRow(
                                       column(12,
                                              selectInput(inputId = "team", 
                                                          label = strong("Select a team"), 
                                                          choices = unique(miacFiltered$team),
                                                          selected = "Macalester"),
                                              selectInput(inputId = "year", 
                                                          label = strong("Year"), 
                                                          choices = unique(miacFiltered$meet_year),
                                                          selected = "2017"),
                                              htmlOutput("name")
                                       )
                                     ),
                                     fluidRow(
                                       column(6,
                                              plotOutput("teamProgress")
                                       ),
                                       column(6,
                                              plotOutput("indivProgress") 
                                       )
                                     )
                            ),
                            tabPanel("Individual Progression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "runProgTeam", 
                                                     label = strong("Select a team"), 
                                                     choices = unique(miacFiltered$team),
                                                     selected = "Macalester"),
                                         htmlOutput("runProgSex"),
                                         htmlOutput("runProgName"),
                                         htmlOutput("runProgMeet")
                                       ),
                                       mainPanel(
                                         plotOutput("runProgTime"),
                                         plotOutput("runProgPlace")
                                       )
                                     )
                                     
                            )
                 ),
                 tabPanel("Conference Awards",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Sex", 
                                          label = strong("Select a gender"), 
                                          choices = unique(miacFiltered$race_group),
                                          selected = "Women"),
                              htmlOutput("TeamConfAwards")
                            ),
                            mainPanel(
                              plotlyOutput("miacAwards",height = 600),
                              plotOutput("miacAwards2")
                            )
                          )
                          
                 ),
                 tabPanel("Top Times",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "sexTopTimes", 
                                          label = strong("Select a gender"), 
                                          choices = unique(miacFiltered$race_group),
                                          selected = "Women"),
                              htmlOutput("teamTopTimes")
                            ),
                            mainPanel(
                              plotlyOutput("topTimes",height = 1000)
                            )
                          )
                          
                 ),
                 navbarMenu("Team Rankings",
                            tabPanel("Men Team Rankings",
                                     h1("MIAC Men Team Rankings"),
                                     h3("Select a team to view 2012 - 2017 rankings"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "team2", 
                                                     label = strong("Select a team"), 
                                                     choices = unique(c("All Teams", miac_men_team_placings$team)),
                                                     selected = "All Teams"),
                                         tableOutput('menTeamRankTable')
                                       ),
                                       mainPanel(
                                         plotlyOutput('teamRanking2',height=500)
                                       )
                                     )
                            ),
                            tabPanel("Women Team Rankings",
                                     h1("MIAC Women Team Rankings"),
                                     h3("Select a team to view 2012 - 2017 rankings"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "team3", 
                                                     label = strong("Select a team"), 
                                                     choices = unique(c("All Teams", miac_women_team_placings$team)),
                                                     selected = "All Teams"),
                                         tableOutput('womenTeamRankTable')
                                       ),
                                       mainPanel(
                                         plotlyOutput('teamRanking',height=500)
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Predictions",
                            tabPanel("Men Team Predictions",
                                     h1("MIAC Men 2017 Team Predictions"),
                                     h3("Select a model to view predicted 2017 team rankings"),
                                     actionButton("lm", "Linear"),
                                     actionButton("tree", "Regression Tree"),
                                     actionButton("forest", "Random Forest"),
                                     actionButton("reset", "Clear"),
                                     hr(),
                                     fluidRow(
                                       column(6,
                                              plotlyOutput('predictDataMen')
                                       ),
                                       column(6,
                                              tableOutput('predictOrderMenTeam')
                                       )
                                     )
                            ),
                            tabPanel("Women Team Predictions",
                                     h1("MIAC Women 2017 Team Predictions"),
                                     h3("Select a model to view predicted 2017 team rankings"),
                                     actionButton("lm2", "Linear"),
                                     actionButton("tree2", "Regression Tree"),
                                     actionButton("forest2", "Random Forest"),
                                     actionButton("reset2", "Clear"),
                                     hr(),
                                     fluidRow(
                                       column(6,
                                              plotlyOutput('predictDataWomen')
                                       ),
                                       column(6,
                                              tableOutput('predictOrderWomenTeam')
                                       )
                                     )
                            ),
                            tabPanel("Individual Predictions",
                                     h1("MIAC 2017 Individual Predictions"),
                                     h3("Select a model and gender to view predicted 2017 individual rankings"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "individualModel", 
                                                     label = strong("Select a model"), 
                                                     choices = unique(c("Linear", "Regression Tree", "Random Forest")),
                                                     selected = "Linear"),
                                         selectInput(inputId = "genderModel", 
                                                     label = strong("Select a gender"), 
                                                     choices = unique(c("Men", "Women")),
                                                     selected = "Men")
                                       ),
                                       mainPanel(
                                         dataTableOutput('predictOrderIndividual')
                                       )
                                     )
                            )
                 ),
                 tabPanel("More", fluidPage(
                   htmlOutput('more')
                 ))
)


server <- function(input, output,session) {
  
  output$more <- renderText({  
    readLines("miacOverviewUpdated.html")  
  })
  
  output$name <- renderUI({
    
    available <- miacFiltered[miacFiltered$team == input$team, "name"]
    
    selectInput(
      inputId = "name", 
      label = "Name",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$TeamConfAwards <- renderUI({
    
    available2 <- miacFiltered[miacFiltered$race_group == input$Sex, "team"]
    
    selectInput(
      inputId = "teamMiacAwards", 
      label = "Team",
      choices = c("All",unique(available2)),
      selected = "All")
    
  })
  
  output$runProgSex <- renderUI({
    
    available35 <- miacFiltered[miacFiltered$team == input$runProgTeam, "race_group"]
    
    selectInput(
      inputId = "indProgSex", 
      label = "Gender",
      choices = unique(available35),
      selected = unique(available35)[1])
    
  })
  
  output$runProgName <- renderUI({
    
    available3 <- miacFiltered[miacFiltered$team == input$runProgTeam & miacFiltered$race_group == input$indProgSex, "name"]
    
    selectInput(
      inputId = "indProgName", 
      label = "Name",
      choices = unique(available3),
      selected = unique(available3)[1])
    
  })
  
  output$runProgMeet <- renderUI({
    
    available4 <- miacFiltered[miacFiltered$team == input$runProgTeam & miacFiltered$race_group == input$indProgSex & miacFiltered$name == input$indProgName, "meet_name"]
    
    selectInput(
      inputId = "indProgMeet", 
      label = "Meet",
      choices = unique(available4),
      selected = unique(available4)[1])
    
  })
  
  dataGraph <- reactive({
    filtered <- miacFiltered %>% 
      arrange(meet_date) %>% 
      filter(team == input$team & meet_year == input$year)
    #print(filtered)   
    filtered
  })
  
  dataGraph2 <- reactive({
    team_placings <- miac_women_team_placings %>% 
      filter(team == input$team2)
    team_placings
  })
  
  output$teamProgress <- renderPlot({
    #this can be filled with as much R script as you want input$team
    dataTest <- dataGraph()
    ggplot(data=dataTest,aes(x=reorder(meetNameDate,meet_date),y=perKilometerTime,color=race_group,group=name)) +
      geom_point(size=2) +
      geom_line() +
      labs(title = paste(input$year," ", input$team," Team Results",sep="")) +
      xlab("Meet") +
      ylab("Minutes Per Kilometer") +
      guides(color=guide_legend(title="Sex")) +
      theme(text = element_text(size=12)) +
      theme(axis.text.x=element_text(angle=55,hjust=1,size=12))
  }, height = 800)
  
  dataIndivGraph <- reactive({
    filtered2 <- miacFiltered %>% 
      filter(team == input$team & name == input$name) %>% 
      mutate(month = format(meet_date, "%m"),
             day = format(meet_date,"%d")) %>% 
      arrange(month,day) %>% 
      unite("monthDay",c(22:23),sep="-") %>% 
      arrange(monthDay)
    #print(filtered)  
    filtered3 <- filtered2 %>% 
      group_by(meet_name) %>% 
      slice(1) %>% 
      select(meet_name,monthDay) %>% 
      rename(firstDate="monthDay")
    
    filtered4 <- filtered2 %>% 
      left_join(filtered3) %>% 
      mutate(meet_name = factor(meet_name,levels(factor(meet_name))[order(filtered3$firstDate)]),
             meet_year = as.factor(meet_year))
    
    filtered4
  })
  
  output$indivProgress <- renderPlot({
    #this can be filled with as much R script as you want input$team
    dataIndiv <- dataIndivGraph()
    ggplot(data=dataIndiv,aes(x=reorder(meet_name,monthDay),y=perKilometerTime,color=meet_year)) +
      geom_point(size=2) +
      geom_line(aes(group=meet_year),size=1.25) +
      xlab("Meet") +
      ylab("Minutes Per Kilometer") +
      theme(axis.text.x=element_text(angle=65,hjust=1,size=12)) +
      labs(title = paste(input$name," Results",sep="")) +
      theme(text = element_text(size=12)) +
      guides(color=guide_legend(title="Year"))
  }, height = 800)
  
  dataRunProg <- reactive({
    runnerProg <- miacFiltered %>% 
      filter(team==input$runProgTeam & name == input$indProgName & meet_name == input$indProgMeet)
  })  
  
  boxPlotTimes <- reactive({
    allTimes <- miacFiltered %>% 
      filter(meet_name == input$indProgMeet & race_group==input$indProgSex) %>% 
      select(meet_name,race_minutes,race_seconds,new_time,meet_year)
  })
  
  output$runProgTime <- renderPlot({
    #this can be filled with as much R script as you want input$team
    dataRunProg <- dataRunProg()
    boxplotTimes <- boxPlotTimes()
    ggplot(data=dataRunProg,aes(x=as.factor(meet_year),y=new_time,color=meet_name)) +
      geom_boxplot(data = boxplotTimes %>% filter(meet_year %in% unique(dataRunProg$meet_year)),aes(x=as.factor(meet_year),y=new_time),color="grey",alpha=.75) +
      geom_point(size=3,color="blue") +
      geom_text_repel(aes(label=paste(race_minutes,":",race_seconds,sep="")),size=3,color="blue") +
      geom_line(aes(group=meet_name),size=1.25,color="blue") +
      xlab("Year") +
      ylab("Time") +
      labs(title = paste(input$indProgName," Results",sep="")) +
      theme(text = element_text(size=12))
  }, height = 300)
  
  #Make y axis only show whole numbers
  output$runProgPlace <- renderPlot({
    #this can be filled with as much R script as you want input$team
    dataRunProg <- dataRunProg()
    ggplot(data=dataRunProg,aes(x=as.factor(meet_year),y=place,color=meet_name)) +
      geom_point(size=2) +
      geom_line(aes(group=meet_name),size=1.25) +
      geom_label_repel(aes(label=place)) +
      theme(text = element_text(size=12)) +
      xlab("Year") +
      ylab("Place") +
      scale_y_continuous(breaks=seq(from=min(dataRunProg$place)-5,to=max(dataRunProg$place)+5,by=5),
                         limits=c(min(dataRunProg$place)-1,max(dataRunProg$place)+1)) +
      guides(color=FALSE)
  }, height = 300)
  
  dataMiacAwardsGraph <- reactive({
    miacAwards <- miac_xc_championships %>% 
      filter(race_group == input$Sex)
  })
  
  dataMiacAwardsGraph2 <- reactive({
    if(input$teamMiacAwards == "All") {
      miacAwards <- miac_xc_championships2 %>% 
        filter(race_group == input$Sex)
    }
    else{
      miacAwards <- miac_xc_championships2 %>% 
        filter(race_group == input$Sex & team == input$teamMiacAwards)
    }
    miacAwards
  })
  
  output$miacAwards <- renderPlotly({
    #this can be filled with as much R script as you want input$team
    dataMiacAwards2 <- dataMiacAwardsGraph2()
    p<-ggplot(dataMiacAwards2) + 
      geom_point(data = dataMiacAwards2, alpha = 0.5, aes(x = meet_year, y = new_time,color = place_category2,text=paste("Place: ", place, "\n Name: ",name,"\n School: ",team,"\n Time: ",new_time,"\n Year: ",meet_year,sep=""))) + 
      geom_point(data = subset(dataMiacAwards2,place %in% c("1", "15", "25")), aes(x = meet_year, y = new_time,color = place_category2), size = 3) + 
      #geom_line(data = subset(dataMiacAwards2,place %in% c("1", "15", "25")), aes(x = meet_year, y = new_time,color = place_category2,group=place_category2)) + 
      scale_color_manual(values = c("red", "green", "blue"), breaks=c("Champion","All Conference","Honorable Mention")) + 
      labs(title = "MIAC Championships", subtitle = "Champion, All-Conference, Honorable Mention", 
           x = "Year",  y = "Time (minutes)", fill = "place") +
      theme(text = element_text(size=12)) +
      guides(color=guide_legend(title="Place Category"))
    ggplotly(p,tooltip=c("text"))
  })
  
  
  
  output$miacAwards2 <- renderPlot({
    #this can be filled with as much R script as you want input$team
    dataMiacAwards <- dataMiacAwardsGraph()
    dataMiacAwards2 <- dataMiacAwardsGraph2()
    ggplot(dataMiacAwards, aes(x = meet_year, y = new_time)) + 
      geom_point(data = dataMiacAwards2, alpha = 0.5, aes(color = place_category2)) + 
      geom_point(aes(color = place_category), size = 3) + 
      geom_line(data = dataMiacAwards, aes(color = place_category, group = place_category)) + 
      scale_color_manual(values = c("red", "green", "blue"), breaks=c("Champion","All Conference","Honorable Mention")) + 
      labs(title = "MIAC Championships", subtitle = "Champion, All-Conference, Honorable Mention", 
           x = "Year",  y = "Time (minutes)", fill = "place") +
      guides(color=guide_legend(title="Place Category"))
  })
  
  output$teamTopTimes <- renderUI({
    
    available3 <- miacFiltered[miacFiltered$race_group == input$sexTopTimes, "team"]
    
    selectInput(
      inputId = "topTimeTeam", 
      label = "Team:",
      choices = c("All",unique(available3)),
      selected = "All")
    
  })
  
  #topTimeTeam
  dataTopTimes <- reactive({
    
    if(input$topTimeTeam == "All") {
      miacTopTimes <- miacFiltered %>% 
        filter(race_group == input$sexTopTimes) %>% 
        arrange(meet_name,new_time) %>% 
        group_by(meet_name) %>% 
        top_n(-10)
    }
    else{
      miacTopTimes <- miacFiltered %>% 
        filter(race_group == input$sexTopTimes) %>% 
        filter(team %in% input$topTimeTeam) %>% 
        arrange(meet_name,new_time) %>% 
        group_by(meet_name) %>% 
        top_n(-10)
    }
    miacTopTimes
    
  })
  
  output$topTimes <- renderPlotly({
    #this can be filled with as much R script as you want input$team
    dataTopTimes <- dataTopTimes()
    topTimesPlot<-ggplot(dataTopTimes) + 
      geom_point(aes(x = meet_name, y = new_time,color = as.factor(meet_year),
                     text=paste("Name: ",name,"\n School: ",team,"\n Time: ",new_time,"\n Year: ",meet_year,sep="")),size=2) + 
      labs(title = "Top 10 Times Historical", x = "Meet",  y = "Time (minutes)") +
      theme(axis.text.x=element_text(angle=65,hjust=1,size=12)) +
      theme(text = element_text(size=12)) +
      guides(color=guide_legend(title="Year"))
    gpw <- as.widget(ggplotly(topTimesPlot,tooltip=c("text")))
    gpw$x$layout$margin$b = 400
    gpw
  })
  
  # Data for Women Team Ranking
  dataWomenTeamRanking <- reactive({
    if(input$team3 == "All Teams") {
      team_placings <- miac_women_team_placings
    }
    else{
      team_placings <- miac_women_team_placings %>%
        filter(team == input$team3)
    }
    team_placings
  })
  
  # Tables for Women Team Ranking
  output$womenTeamRankTable <- renderTable({
    if(input$team3 != "All Teams") {
      team_placings <- miac_women_team_placings %>%
        filter(team == input$team3) %>% 
        rename("year"="meet_year") %>%
        rename("score"="team_score") %>%
        rename("rank"="team_rank")
    }
    else {
      team_placings <- miac_women_team_placings %>% 
        filter(meet_year == 2017) %>% 
        rename("year"="meet_year") %>%
        rename("score"="team_score") %>%
        rename("rank"="team_rank")
    }
    team_placings
  },
  width = 2,
  bordered = TRUE
  )
  
  # Women Team Ranking Plot
  output$teamRanking <- renderPlotly({
    dataTest <- dataWomenTeamRanking()
    print(dataWomenTeamRanking())
    wTeamPlot <- ggplot() +
      geom_line(data=miac_women_team_placings, aes(color = team, x=meet_year,y=team_rank,group = team), size = 1, alpha = 0.3) + 
      geom_point(data=dataTest, aes(color = team, x=meet_year,y=team_rank, size = 5)) +
      geom_line(data=dataTest, aes(color = team, x=meet_year,y=team_rank,group = team, text=paste("Team: ", team, "\n Year: ", meet_year, "\n Place: ",team_rank, "\n Score: ",team_score)), size = 2) +
      labs(title = paste( input$team3," Women Team Rankings",sep="")) +
      xlab("Year") +
      ylab("Team Rank") +
      guides(color=guide_legend(title="Team")) +
      theme(axis.text.x=element_text(hjust=1,size=10)) +
      scale_color_manual(values=c("palevioletred1", "violetred1", "red", "darkorange2", "goldenrod", "green1", "green4", 
                                  "turquoise", "blue", "steelblue4", "purple", "slategrey")) 
    ggplotly(wTeamPlot,tooltip=c("text"))
  })
  
  # Data for Men Team Ranking
  dataMenTeamRanking <- reactive({
    if(input$team2 == "All Teams") {
      team_placings <- miac_men_team_placings 
    }
    else{
      team_placings <- miac_men_team_placings %>%
        filter(team == input$team2)
    }
    team_placings
  })
  
  # Tables for Men Team Ranking
  output$menTeamRankTable <- renderTable({
    if(input$team2 != "All Teams") {
      team_placings <- miac_men_team_placings %>%
        filter(team == input$team2) %>% 
        rename("year"="meet_year") %>%
        rename("score"="team_score") %>%
        rename("rank"="team_rank")
    }
    else {
      team_placings <- miac_men_team_placings %>% 
        filter(meet_year == 2017) %>% 
        rename("year"="meet_year") %>%
        rename("score"="team_score") %>%
        rename("rank"="team_rank")
    }
    team_placings
  },
  width = 2,
  bordered = TRUE
  )
  
  # Men Team Ranking Plot
  output$teamRanking2 <- renderPlotly({
    dataTest <- dataMenTeamRanking()
    print(dataMenTeamRanking())
    mTeamPlot <- ggplot() +
      geom_line(data=miac_men_team_placings, aes(color = team, x=meet_year,y=team_rank,group = team), size = 1, alpha = 0.3) + 
      geom_point(data=dataTest, aes(color = team, x=meet_year,y=team_rank, size = 5)) +
      geom_line(data=dataTest, aes(color = team, x=meet_year,y=team_rank,group = team, text=paste("Team: ", team, "\n Year: ", meet_year, "\n Place: ",team_rank, "\n Score: ",team_score)), size = 2) +
      labs(title = paste(input$team2," Men Team Rankings",sep="")) +
      xlab("Year") +
      ylab("Team Rank") +
      guides(color=guide_legend(title="Team")) +
      theme(axis.text.x=element_text(hjust=1,size=10)) +
      scale_color_manual(values=c("palevioletred1", "violetred1", "red", "darkorange2", "goldenrod", 
                                  "green1", "green4", "turquoise", "blue", "purple", "slategrey")) 
    ggplotly(mTeamPlot,tooltip=c("text"))
    
  })
  
  # team predictions men
  teamMenValues <- reactiveValues(linear = 0, tree = 0, forest = 0)
  vMen <- reactiveValues(data = NULL)
  vMen2 <- reactiveValues(data = NULL)
  
  # linear model
  observeEvent(input$lm, {
    # data for plot
    vMen$data <- RunnersTrain %>% 
      filter(race_group == "Men")
    # data table with team prediction results
    vMen2$data <- 
      miac_2017_prediction_placings_lm %>% 
      # score top seven runners per team
      filter(race_group == "Men") %>% 
      group_by(team) %>% 
      top_n(n = -7, wt = predict_place) %>%
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=seq_along)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = score) %>%
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered)))  %>%    
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_men_team_placings_2017, by = c("team" = "team")) %>% 
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamMenValues$linear <- 1
    teamMenValues$tree <- 0
    teamMenValues$forest <- 0
    
  })
  
  # regression tree model
  observeEvent(input$tree, {
    # data for plot
    vMen$data <- RunnersTrain %>% 
      filter(race_group == "Men")
    # data table with team prediction results
    vMen2$data <- 
      miac_men_2017_prediction_placings_tree <-
      # assign places
      miac_2017_prediction_placings_tree %>% 
      # score top seven runners per team
      filter(race_group == "Men") %>% 
      group_by(team) %>%
      mutate(team_num = ave(predict_place, FUN=seq_along)) %>% 
      top_n(n = -7, wt = team_num) %>% 
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=ordered)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = team_num) %>% 
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered)))  %>%  
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_men_team_placings_2017, by = c("team" = "team")) %>% 
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamMenValues$linear <- 0
    teamMenValues$tree <- 1
    teamMenValues$forest <- 0
  })
  # random forest model
  observeEvent(input$forest, {
    # data for plot
    vMen$data <- RunnersTrain %>% 
      filter(race_group == "Men")
    # data table with team prediction results
    vMen2$data <- 
      miac_2017_prediction_placings_forest %>% 
      # score top seven runners per team
      filter(race_group == "Men") %>% 
      group_by(team) %>% 
      top_n(n = -7, wt = predict_place) %>%
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=seq_along)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = score) %>%
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered)))  %>%   
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_men_team_placings_2017, by = c("team" = "team")) %>% 
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamMenValues$linear <- 0
    teamMenValues$tree <- 0
    teamMenValues$forest <- 1
  })
  
  # reset button
  observeEvent(input$reset, {
    vMen$data <- NULL
    vMen2$data <- NULL
    teamMenValues$linear <- 0
    teamMenValues$tree <- 0
    teamMenValues$forest <- 0
  }) 
  
  output$predictDataMen <- renderPlotly({
    if (is.null(vMen$data)) return()
    predPlotW <- NULL
    if (teamMenValues$linear) {
      predPlotW <- ggplot(vMen$data,aes(x = meet1,y=Minnesota.Intercollegiate.Championships)) +
        geom_jitter(aes(text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)),alpha=0.5, color = "turquoise3") +
        geom_smooth(method = "lm", se=F, color = "grey30") +
        labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Men Linear Model MIAC Championship Time <br> Training Data 2012-2016")}
    else
      if(teamMenValues$tree) {
        predPlotW <- ggplot(vMen$data, aes(y=treeTrain, x=meet1)) +
          geom_jitter(aes(y=Minnesota.Intercollegiate.Championships, x=meet1,
                          text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)), color = "turquoise3", alpha = 0.5) +
          geom_line(color = "grey30") +
          labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Men Regression Tree Model MIAC Championship Time <br> Training Data 2012-2016")
      }
    else
      if(teamMenValues$forest) {
        predPlotW <- ggplot(vMen$data, aes(y=forestTrain, x=meet1)) +
          geom_jitter(aes(y=Minnesota.Intercollegiate.Championships, x=meet1,
                          text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)), color = "turquoise3", alpha = 0.5) +
          geom_line(color = "grey30") +
          labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Men Random Forest Model MIAC Championship Time <br> Training Data 2012-2016")
      }
    ggplotly(predPlotW,tooltip=c("text"))
  })
  
  # men team rank prediction table
  output$predictOrderMenTeam <- renderTable({
    if (is.null(vMen2$data)) return()
    vMen2$data 
  })
  
  # team predictions women
  teamWomenValues <- reactiveValues(linear = 0, tree = 0, forest = 0)
  vWomen <- reactiveValues(data = NULL)
  vWomen2 <- reactiveValues(data = NULL)
  
  # linear model
  observeEvent(input$lm2, {
    # data for plot
    vWomen$data <- RunnersTrain %>% 
      filter(race_group == "Women")
    # data table with team prediction results
    vWomen2$data <- 
      miac_2017_prediction_placings_lm %>% 
      # score top seven runners per team
      filter(race_group == "Women") %>% 
      group_by(team) %>% 
      top_n(n = -7, wt = predict_place) %>%
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=seq_along)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = score) %>%
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered))) %>%  
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_women_team_placings_2017, by = c("team" = "team")) %>% 
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamWomenValues$linear <- 1
    teamWomenValues$tree <- 0
    teamWomenValues$forest <- 0
    
  })
  
  # regression tree model
  observeEvent(input$tree2, {
    # data for plot
    vWomen$data <- RunnersTrain %>% 
      filter(race_group == "Women")
    # data table with team prediction results
    vWomen2$data <- 
      miac_women_2017_prediction_placings_tree <-
      # assign places
      miac_2017_prediction_placings_tree %>% 
      # score top seven runners per team
      filter(race_group == "Women") %>% 
      group_by(team) %>%
      mutate(team_num = ave(predict_place, FUN=seq_along)) %>% 
      top_n(n = -7, wt = team_num) %>% 
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=ordered)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = team_num) %>% 
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered))) %>%  
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_women_team_placings_2017, by = c("team" = "team")) %>%
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamWomenValues$linear <- 0
    teamWomenValues$tree <- 1
    teamWomenValues$forest <- 0
  })
  # random forest model
  observeEvent(input$forest2, {
    # data for plot
    vWomen$data <- RunnersTrain %>% 
      filter(race_group == "Women")
    # data table with team prediction results
    vWomen2$data <- 
      miac_2017_prediction_placings_forest %>% 
      # score top seven runners per team
      filter(race_group == "Women") %>% 
      group_by(team) %>% 
      top_n(n = -7, wt = predict_place) %>%
      ungroup() %>% 
      mutate(score = ave(predict_minutes, FUN=seq_along)) %>% 
      # calculate team score from top 5 runners
      group_by(team) %>% 
      top_n(n = -5, wt = score) %>%
      summarise(team_score = as.integer(sum(score))) %>%
      arrange(team_score) %>%
      mutate(team_rank = as.integer(ave(team_score, FUN=ordered))) %>%  
      rename("predict_score"="team_score") %>%
      rename("predict_rank"="team_rank") %>% 
      left_join(miac_women_team_placings_2017, by = c("team" = "team")) %>% 
      mutate(real_score = as.integer(real_score)) %>% 
      select(team, predict_score, real_score, predict_rank, real_rank)
    teamWomenValues$linear <- 0
    teamWomenValues$tree <- 0
    teamWomenValues$forest <- 1
  })
  
  # reset button
  observeEvent(input$reset2, {
    vWomen$data <- NULL
    vWomen2$data <- NULL
    teamWomenValues$linear <- 0
    teamWomenValues$tree <- 0
    teamWomenValues$forest <- 0
  }) 
  
  output$predictDataWomen <- renderPlotly({
    if (is.null(vWomen$data)) return()
    predPlotM <- NULL
    if (teamWomenValues$linear) {
      predPlotM <- ggplot(vWomen$data,aes(x = meet1,y=Minnesota.Intercollegiate.Championships)) +
        geom_jitter(aes(text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)),alpha=0.5, color = "indianred") +
        geom_smooth(method = "lm", se=F, color = "grey30") +
        labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Women Linear Model MIAC Championship Time <br> Training Data 2012-2016")}
    else
      if(teamWomenValues$tree) {
        predPlotM <- ggplot(vWomen$data, aes(y=treeTrain, x=meet1)) +
          geom_jitter(aes(y=Minnesota.Intercollegiate.Championships, x=meet1,
                          text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)), color = "indianred", alpha = 0.5) +
          geom_line(color = "grey30") +
          labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Women Regression Tree Model MIAC Championship Time <br> Training Data 2012-2016")
      }
    else
      if(teamWomenValues$forest) {
        predPlotM <- ggplot(vWomen$data, aes(y=forestTrain, x=meet1)) +
          geom_jitter(aes(y=Minnesota.Intercollegiate.Championships, x=meet1,
                          text=paste("Name: ", name, "\n Year: ", meet_year, "\n Team: ",team, "\n Class Year: ", class_year, "\n Meet 1: ", meet1, "\n Meet 2: ", meet2, "\n Meet 3: ", meet3, "\n MIAC: ", Minnesota.Intercollegiate.Championships)), color = "indianred", alpha = 0.5) +
          geom_line(color = "grey30") +
          labs(x = "Meet 1 Time", y = "MIAC Championship Time", title = "Women Random Forest Model MIAC Championship Time <br> Training Data 2012-2016")
      }
    ggplotly(predPlotM,tooltip=c("text"))
  })
  
  # men team rank prediction table
  output$predictOrderWomenTeam <- renderTable({
    if (is.null(vWomen2$data)) return()
    vWomen2$data 
  })
  
  output$predictOrderIndividual <- renderDataTable({
    # Individual Men Predictions
    if (input$genderModel == "Men"){
      if (input$individualModel == "Linear") {
        individual_results <-
          miac_2017_prediction_placings_lm %>% 
          ungroup() %>% 
          filter(race_group == "Men") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference)
      }
      if (input$individualModel == "Regression Tree") {
        individual_results <-
          miac_2017_prediction_placings_tree %>% 
          ungroup() %>% 
          filter(race_group == "Men") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference)
      }
      if (input$individualModel == "Random Forest") {
        individual_results <-
          miac_2017_prediction_placings_forest %>% 
          ungroup() %>% 
          filter(race_group == "Men") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference) 
      }
    }
    # Individual Women Predictions
    else {
      if (input$individualModel == "Linear") {
        individual_results <-
          miac_2017_prediction_placings_lm %>% 
          ungroup() %>% 
          filter(race_group == "Women") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference) 
      }
      if (input$individualModel == "Regression Tree") {
        individual_results <-
          miac_2017_prediction_placings_tree %>% 
          ungroup() %>% 
          filter(race_group == "Women") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference) 
      }
      if (input$individualModel == "Random Forest") {
        individual_results <-
          miac_2017_prediction_placings_forest %>% 
          ungroup() %>% 
          filter(race_group == "Women") %>% 
          select(predict_place, real_place, name, team, class_year, predict_minutes, real_minutes, min_difference) 
      }
    }
    individual_results
  })
  
}

shinyApp(ui = ui, server = server)

#Shinyapps.io