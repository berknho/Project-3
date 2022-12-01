#libraries, import data from project 1

library(shiny)
library(ggplot2)

shootings <- read.csv('Project1_police_shootings.csv')
shootings$Person.Age[shootings$Person.Age==0] = NA

#define ui
ui <- fluidPage(
  
  #Application title
  titlePanel("Police Shooting Data"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      #select box for variable
      selectInput("selectvar", label = h3("Choose a Variable"),
                  choices=list("Age"=1, "Gender"=2, "Race"=3, "Fleeing Factor"=4, "Shooting Manner"=5),
                  selected = 1),
      
      #option to show mean
      checkboxInput("checkbox_mean", label="Display Mean or Five Number Summary", value=FALSE),
      
      #option to show standard deviation
      checkboxInput("checkbox_sd", label="Display Standard Deviation", value=FALSE),
    ),
    
    
    
    #Show plot of generated distribution
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      p('Mean or Five Number Summary'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard Deviation'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      img(src = "police shootings.jpeg", height = 400, width = 560)
    )
    
  )
)

#define server and histograms
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    #histogram for age
    if(input$selectvar == 1){
      ggplot(shootings) + geom_histogram(aes(x = Person.Age), binwidth = 1, na.rm=TRUE) + 
        ylim(0, 250) + 
        labs(title = 'Distribution of Ages in Police Shootings', y='Frequency', x='Age') + 
        theme(text=element_text(size=17, face="bold"), plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"))
    }
    
    #for gender
    else if(input$selectvar == 2){
      ggplot(shootings, aes(x=Person.Gender, y=Person.Age, fill=Person.Gender)) + 
        geom_boxplot(na.rm=TRUE) + 
        labs(title = 'Age of Shooting Victims by Gender', x = 'Gender', y = 'Age') +
        scale_fill_brewer(palette="Dark2") +
        guides(fill=guide_legend(title="Gender")) +
        theme(text=element_text(size=13, face="bold"), plot.margin = unit(c(1.0, 1.5, 1.0, 1.5), "cm"))
    }
    
    #for race
    else if(input$selectvar == 3){
      ggplot(shootings, aes(x=Person.Race, y=Person.Age, fill=Person.Race)) +
        geom_boxplot( na.rm=TRUE) + 
        labs(title = 'Age of Shooting Victims by Race', x = 'Race', y = 'Age') + 
        scale_fill_brewer(palette="Dark2") + 
        theme(title=element_text(size=17, face="bold"), axis.title=element_text(size=13, face="bold"), plot.margin = unit(c(1.5, 1, 1, 1.5), "cm")) +
        guides(fill=guide_legend(title="Race")) +
        scale_x_discrete(limits=c("Other", "Native American", "African American", "Hispanic", "Asian", "White", "Unknown"))
    }
    
    #for fleeing factor
    else if(input$selectvar == 4){
      ggplot(shootings, aes(x=Factors.Fleeing, y=Person.Age, fill=Factors.Fleeing)) + 
        geom_boxplot(na.rm=TRUE) + 
        labs(title = 'Age of Shooting Victims by Fleeing Factor', x = 'Fleeing Factor', y = 'Age') +
        guides(fill=guide_legend(title="Method of Fleeing")) +
        scale_x_discrete(limits=c("Car", "Foot", "Not fleeing", "Other", "unknown"), labels=c("Car" = "Car", "Foot" = "Foot", "Not fleeing" = "Not Fleeing", "Other" = "Other", "unknown" = "Unknown")) +
        scale_fill_brewer(palette="Dark2") +
        theme(title=element_text(size=17, face="bold"), axis.title=element_text(size=13, face="bold"), plot.margin = unit(c(1.5, 1, 1, 1.5), "cm")) +
        scale_fill_discrete(name="Fleeing Manner", labels = c("Car", "Foot", "Not Fleeing", "Other", "Unknown"))
      
    }
    
    #for shooting manner
    else if(input$selectvar == 5){
      ggplot(shootings, aes(x=Shooting.Manner, y=Person.Age, fill=Shooting.Manner)) +
        geom_boxplot(na.rm=TRUE) +
        labs(title = 'Age of Shooting Victims by Shooting Manner', x = 'Shooting Manner', y = 'Age') +
        guides(fill=guide_legend(title="Shooting Manner")) +
        scale_x_discrete(limits=c("shot", "shot and Tasered"), labels=c("shot" = "Shot", "shot and Tasered" = "Shot and Tasered")) +
        theme(text=element_text(size=10, face="bold"), plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm")) +
        scale_fill_discrete(name="Shooting Manner", labels = c("Shot", "Shot and Tasered"))
    }
    
  })
  
  #display mean if selected
  output$mean <- renderPrint({
    
    #mean for age
    if(input$checkbox_mean == TRUE & input$selectvar == 1){
      mean(shootings$Person.Age, na.rm=TRUE)}
    
    #five number summary for gender
    else if(input$checkbox_mean == TRUE & input$selectvar == 2){
      fivenum_male <- fivenum(shootings$Person.Age[shootings$Person.Gender=="Male"])
      fivenum_female <- fivenum(shootings$Person.Age[shootings$Person.Gender=="Female"])
      fivenum_gender_unknown <- fivenum(shootings$Person.Age[shootings$Person.Gender=="Unknown"])
      tab <- matrix(c(fivenum_male, fivenum_female, fivenum_gender_unknown), ncol=5, byrow=TRUE)
      rownames(tab) <- c('Male Five Number Summary', 'Female Five Number Summary', 'Gender Unknown Five Number Summary')
      colnames(tab) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
      tab <- as.table(tab)
      tab
    }
    
    #five number summary for race
    else if(input$checkbox_mean == TRUE & input$selectvar == 3){
      fivenum_other <- fivenum(shootings$Person.Age[shootings$Person.Race=="Other"])
      fivenum_native <- fivenum(shootings$Person.Age[shootings$Person.Race=="Native American"])
      fivenum_afr <- fivenum(shootings$Person.Age[shootings$Person.Race=="African American"])
      fivenum_hisp <- fivenum(shootings$Person.Age[shootings$Person.Race=="Hispanic"])
      fivenum_asian <- fivenum(shootings$Person.Age[shootings$Person.Race=="Asian"])
      fivenum_white <- fivenum(shootings$Person.Age[shootings$Person.Race=="White"])
      fivenum_unk <- fivenum(shootings$Person.Age[shootings$Person.Race=="Unknown"])
      race_tab <- matrix(c(fivenum_other, fivenum_native, fivenum_afr, fivenum_hisp, fivenum_asian, fivenum_white, fivenum_unk), ncol=5, byrow=TRUE)
      rownames(race_tab) <- c('Other', 'Native American', 'African American', 'Hispanic', 'Asian', 'White', 'Unknown')
      colnames(race_tab) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
      race_tab <- as.table(race_tab)
      race_tab
    }
    
    #five number summary for fleeing factors
    else if(input$checkbox_mean == TRUE & input$selectvar == 4){
      fivenum_car <- fivenum(shootings$Person.Age[shootings$Factors.Fleeing=="Car"])
      fivenum_foot <- fivenum(shootings$Person.Age[shootings$Factors.Fleeing=="Foot"])
      fivenum_notfleeing <- fivenum(shootings$Person.Age[shootings$Factors.Fleeing=="Not fleeing"])
      fivenum_flee_other <- fivenum(shootings$Person.Age[shootings$Factors.Fleeing=="Other"])
      fivenum_flee_unknown <- fivenum(shootings$Person.Age[shootings$Factors.Fleeing=="unknown"])
      flee_tab <- matrix(c(fivenum_car, fivenum_foot, fivenum_notfleeing, fivenum_flee_other, fivenum_flee_unknown), ncol=5, byrow=TRUE)
      rownames(flee_tab) <- c('Car', 'Foot', 'Not Fleeing', 'Other', 'Unknown')
      colnames(flee_tab) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
      flee_tab <- as.table(flee_tab)
      flee_tab
    }
    
    #five number summary for shooting manner
    else if(input$checkbox_mean == TRUE & input$selectvar == 5){
      fivenum_shot <- fivenum(shootings$Person.Age[shootings$Shooting.Manner=="shot"])
      fivenum_shot_taser <- fivenum(shootings$Person.Age[shootings$Shooting.Manner=="shot and Tasered"])
      manner_tab <- matrix(c(fivenum_shot, fivenum_shot_taser), ncol=5, byrow=TRUE)
      rownames(manner_tab) <- c('Shot', 'Shot and Tasered')
      colnames(manner_tab) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
      manner_tab
    }
    
  })
  
  #display sd if selected
  output$sd <- renderPrint({
    
    #sd for age
    if(input$checkbox_sd == TRUE & input$selectvar == 1){
      sd(shootings$Person.Age, na.rm=TRUE)}
    
    #sd for gender
    else if(input$checkbox_sd == TRUE & input$selectvar == 2){
      sd_male <- sd(shootings$Person.Age[shootings$Person.Gender=="Male"], na.rm=TRUE)
      sd_female <- sd(shootings$Person.Age[shootings$Person.Gender=="Female"], na.rm=TRUE)
      sd_gender_unk <- sd(shootings$Person.Age[shootings$Person.Gender=="Unknown"], na.rm=TRUE)
      sd_tab <- matrix(c(sd_male, sd_female, sd_gender_unk), ncol=1, byrow=TRUE)
      rownames(sd_tab) <- c('Male', 'Female', 'Unknown')
      colnames(sd_tab) <- c('Standard Deviation')
      sd_tab <- as.table(sd_tab)
      sd_tab
    }
    
    #sd for race
    else if(input$checkbox_sd == TRUE & input$selectvar == 3){
      sd_race_other <- sd(shootings$Person.Age[shootings$Person.Race=="Other"], na.rm=TRUE)
      sd_native <- sd(shootings$Person.Age[shootings$Person.Race=="Native American"], na.rm=TRUE)
      sd_afr <- sd(shootings$Person.Age[shootings$Person.Race=="African American"], na.rm=TRUE)
      sd_hisp <- sd(shootings$Person.Age[shootings$Person.Race=="Hispanic"], na.rm=TRUE)
      sd_asian <- sd(shootings$Person.Age[shootings$Person.Race=="Asian"], na.rm=TRUE)
      sd_white <- sd(shootings$Person.Age[shootings$Person.Race=="White"], na.rm=TRUE)
      sd_race_unk <- sd(shootings$Person.Age[shootings$Person.Race=="Unknown"], na.rm=TRUE)
      sd_race_tab <- matrix(c(sd_race_other, sd_native, sd_afr, sd_hisp, sd_asian, sd_white, sd_race_unk), ncol=1, byrow=TRUE)
      rownames(sd_race_tab) <- c("Other Standard Deviation", "Native American Standard Deviation", "African American Standard Deviation", "Hispanic Standard Deviation", "Asian Standard Deviation", "White Standard Deviation", "Unknown Standard Deviation")
      colnames(sd_race_tab) <- c('Standard Deviation')
      sd_race_tab <- as.table(sd_race_tab)
      sd_race_tab
    }
    
    #sd for fleeing factors
    else if(input$checkbox_sd == TRUE & input$selectvar == 4){
      sd_car <- sd(shootings$Person.Age[shootings$Factors.Fleeing=="Car"], na.rm=TRUE)
      sd_foot <- sd(shootings$Person.Age[shootings$Factors.Fleeing=="Foot"], na.rm=TRUE)
      sd_notflee <- sd(shootings$Person.Age[shootings$Factors.Fleeing=="Not fleeing"], na.rm=TRUE)
      sd_flee_other <- sd(shootings$Person.Age[shootings$Factors.Fleeing=="Other"], na.rm=TRUE)
      sd_flee_unknown <- sd(shootings$Person.Age[shootings$Factors.Fleeing=="unknown"], na.rm=TRUE)
      sd_flee_tab <- matrix(c(sd_car, sd_foot, sd_notflee, sd_flee_other, sd_flee_unknown), ncol=1, byrow=TRUE)
      rownames(sd_flee_tab) <- c("Car", "Foot", "Not Fleeing", "Other", "Unknown")
      colnames(sd_flee_tab) <- c('Standard Deviation')
      sd_flee_tab <- as.table(sd_flee_tab)
      sd_flee_tab
    }
    
    #sd for shooting manner
    else if(input$checkbox_sd == TRUE & input$selectvar == 5){
      sd_shot <- sd(shootings$Person.Age[shootings$Shooting.Manner=="shot"], na.rm=TRUE)
      sd_shot_tased <- sd(shootings$Person.Age[shootings$Shooting.Manner=="shot and Tasered"], na.rm=TRUE)
      sd_manner_tab <- matrix(c(sd_shot, sd_shot_tased), ncol=1, byrow=TRUE)
      rownames(sd_manner_tab) <- c("Shot", "Shot and Tasered")
      colnames(sd_manner_tab) <- c("Standard Deviation")
      sd_manner_tab <- as.table(sd_manner_tab)
      sd_manner_tab
    }
      
    })  
  
  
  }
  
  
#run App
shinyApp(ui = ui, server = server)  
  

    