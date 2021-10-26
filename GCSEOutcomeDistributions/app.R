#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)

library(dplyr)
library(tibble)
library(tidyr)
library(data.table)


doubleaward <- paste0(c(9:1),"-",c(9:1))
legacy <- c("A*",LETTERS[1:7])
U <- "U"
reformed <- c(9:1)

bdat <- read.csv("GCSE data.csv") %>%
    filter(Cumulative == "Non-cumulative percentages") %>% #raw percentages
    filter(Grades %in% c(legacy,reformed,U,doubleaward)) %>%  #unbinned only
    filter(Gender != "All",
           Age == "All ages") %>%
    filter(Subject != "All subjects") %>%
    mutate(TypeSubject = case_when(Subject %in% c("Additional science",
                                                  "Additional science (further)",
                                                  "Biology",
                                                  "Chemistry",
                                                  "Computing",
                                                  "Engineering",
                                                  "Mathematics",
                                                  "Physics",
                                                  "Science",
                                                  "Science: Double award",
                                                  "Statistics",
                                                  "Other sciences",
                                                  "Other technology") ~ "STEM",
                                   Subject %in% c("English language",
                                                  "English literature",
                                                  "French",
                                                  "German",
                                                  "Other modern languages",
                                                  "Spanish") ~ "Languages",
                                   Subject %in% c("Business and comm. systems",
                                                  "Economics",
                                                  "Business studies",
                                                  "Citizenship studies",
                                                  "Religious studies",
                                                  "Social science subjects",
                                                  "Humanities",
                                                  "Classical subjects",
                                                  "Geography",
                                                  "History") ~ "Social sciences and Humanities",
                                   Subject %in% c("Food preparation and nutrition",
                                                  "Construction",
                                                  "Health and social care",
                                                  "Home economics",
                                                  "Manufacturing",
                                                  "Hospitality",
                                                  "ICT",
                                                  "Leisure and tourism") ~ "Applied",
                                   Subject %in% c("Art and design subjects",
                                                  "Music",
                                                  "Physical education",
                                                  "Media / Film / TV studies",
                                                  "Performing / expressive arts",
                                                  "Drama",
                                                  "Design and technology") ~ "Sports and Arts",
                                   TRUE ~ "MISSING"))




x <- as.data.table(bdat)

y <- dcast(x, Subject + Year + Gender + Age + Type + Candidates + TypeSubject ~ .,
           fun.aggregate = sum,
           value.var = 'Percent')
colnames(y)[8] <- c('PercentTot')

y[, Percent:= 100 - PercentTot]
y[, Grades:= "U"]


dat <- tibble::as_tibble(rbind(x, y,fill=TRUE)) %>% arrange(Subject,Year,Grades) %>%
    select(-PercentTot) %>%
    mutate(Percent = ifelse(Percent < 0, 0, Percent)) %>%
    mutate(Candidates = as.numeric(as.character(Candidates))) %>%
    mutate(Rawnum = (Percent/100) * Candidates) %>%
    mutate(TypeGrade = case_when(Grades %in% c(legacy[1:2],reformed[1:3],doubleaward[1:3]) ~ "Top",
                                 Grades %in% c(legacy[3:4],reformed[4:6],doubleaward[4:6]) ~ "Pass",
                                 Grades %in% c(legacy[5:8],reformed[7:9],doubleaward[7:9],"U") ~ "Fail",
                                 TRUE ~ "MISSING"))



# dummyframe <- tibble(Year = rep(c(2008:2021),each=3),
#                      Gender = NA,
#                      Subject = NA,
#                      TypeGrade = rep(c("Fail","Pass","Top"),length(c(2008:2021))),
#                      value = rep(c(25000,30000,15000),length(c(2008:2021)))) %>%
#   group_by(Year) %>%
#   mutate(percent = value/sum(value)) %>%
#   mutate(total = NA)


#  str(STEMonly)

theme_prov <- theme(axis.text.x= element_text(size=8),
                    axis.text.y= element_text(size=8),
                    plot.background = element_rect(fill = "transparent",color="black"),
                    panel.background = element_rect(color="transparent"),
                    axis.title= element_text(size=8),
                    axis.title.y = element_blank(),
                    strip.text = element_text(size=8),
                    plot.title = element_text(size=12,face="bold"),
                    strip.background = element_rect(color="transparent",fill="transparent"),
                   # strip.text.x = element_text(angle = 0),
                    legend.position = "none")




totaln <- dat %>% select(Subject,Year,Gender,Candidates,TypeSubject) %>% distinct() %>%
    group_by(Subject,Year,Gender,TypeSubject) %>%
    mutate(Candidates = as.numeric(as.character(Candidates))) %>%
    summarize(Can = sum(Candidates)) %>%
    mutate(TypeSubject = factor(TypeSubject,levels=c("Applied","STEM","Languages","Sports and Arts","Social sciences and Humanities")))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution of Fail / Pass / Top GCSE grades"),

    fluidRow(
        column(12,
             tags$p("Data in this shiny app is taken from ",
                    tags$a(href = "https://analytics.ofqual.gov.uk/apps/GCSE/Outcomes/", "analytics.ofqual.gov.uk. "),"The preprocessed data supplied by Ofqual, in turn, is based on data supplied by the Joint Council for Qualifications."),
                    tags$p("For ease of presentation, the following simplifications were made:",
                    tags$ul(tags$li("plotted data are 'all ages' only, not 'Aged 16'"),
                            tags$li("Reformed and legacy grades were simplified into 'Fail', 'Pass' and 'Top',  where:",
                                    tags$ul(tags$li("Fail: D, E, F, G and U (Legacy); U, 3, 2, 1 and U (Reformed); 3-3, 2-2, 1-1 and U (Double-Award); where 'U' is Unclassified."),
                                            tags$li("Pass: B and C (Legacy); 6, 5 and 4 (Reformed); 6-6, 5-5, 4-4 (Double-Award)"),
                                            tags$li("Top: A* and A (Legacy); 9, 8 and 7 (Reformed); 9-9, 8-8, 7-7 (Double-Award).")),
                                    "This means 'Pass' is used for grades that allowed passing without being the top-level grades."))),
             tags$p("Subjects are organized by subject area for ease of presentation. Some subjects span different subject areas, e.g., Classical subjects, but were still assigned to only one subject area."))),
    fluidRow(
        column(2, tableOutput("tableApplied")),
        column(2, tableOutput("tableLanguages")),
        column(2, tableOutput("tableSocial")),
        column(2, tableOutput("tableSports")),
        column(2, tableOutput("tableSTEM"))),
    fluidRow(
        column(12,
             tags$p("All graphs have the same structure and contain the following elements and information: "),

             div(img(src="Explanation.png",width="50%"), align="center"),

             tags$p("Note: The individual numbers and percentages are only approximately correct. In the aggregated data set provided by Ofqual
                      the total number of students per year, course, gender and age bracket is provided, alongside the percentage of candidates
                     (to a single decimal) who achieved a given grade. For the analysis here, I retransformed percentages into raw frequencies to then
                      recombine these across grade brackets. Percentages for 'U' were inferred on the basis of the given data."),
             tags$p("See", tags$a(href = "https://github.com/nklange/gcsegrades/", "github.com/nklange/gcsegrades"), "for the underlying R scripts and data.")
        )),

    tabsetPanel(
        tabPanel("Across subjects, by year",
                 fluidRow(

                     column(3,
                            tags$br(),
                            selectInput("InputYear", "Year", c(2021:2008)),
                            radioButtons("InputArea", "Subject area",
                                         c("Applied","Languages","Social sciences and Humanities","Sports and Arts","STEM"))
                     ),
                     column(8,
                            plotlyOutput("byYear")
                     )
                 )
        ),
       tabPanel("Across years: Applied",


                fluidRow(
                    column(3,
                           tags$h2("Applied Subjects"),
                            radioButtons("InputSubject_Applied", "Subject",
                                        choices = unique(dat %>% filter(TypeSubject == "Applied") %>% .$Subject))
                    ),

                    column(6, plotlyOutput("bySubject_Applied",height=1000)
                    ))
                ),

       tabPanel("Across years: Languages",
                fluidRow(
                    column(3,
                           tags$h2("Languages"),
                           radioButtons("InputSubject_Languages", "Subject",
                                        choices = unique(dat %>% filter(TypeSubject == "Languages") %>% .$Subject))
                    ),
                    column(6, plotlyOutput("bySubject_Languages",height=1000)
                    )
                )
       ),
       tabPanel("Across year: Social sciences",
                fluidRow(
                    column(3,
                           tags$h2("Social Sciences and Humanities"),
                           radioButtons("InputSubject_Social", "Subject",
                                        choices = unique(dat %>% filter(TypeSubject == "Social sciences and Humanities") %>% .$Subject))
                    ),
                    column(6, plotlyOutput("bySubject_Social",height=1000)
                    )
                )
       ),
       tabPanel("Across years: Sports and Arts",
                fluidRow(
                    column(3,
                           tags$h2("Sports and Arts"),
                           radioButtons("InputSubject_Sports", "Subject",
                                        choices = unique(dat %>% filter(TypeSubject == "Sports and Arts") %>% .$Subject))
                    ),
                    column(6, plotlyOutput("bySubject_Sports",height=1000)
                    )
                )
       ),
       tabPanel("Across years: STEM subjects",
                fluidRow(
                    column(3,
                           tags$h2("STEM Subjects"),
                           radioButtons("InputSubject_STEM", "Subject",
                                        choices = unique(dat %>% filter(TypeSubject == "STEM") %>% .$Subject))
                    ),
                    column(6, plotlyOutput("bySubject_STEM",height=1000)
                    )
                    )
                )






    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    output$byYear <- renderPlotly({

        STEMonly <- dat %>%
           filter(TypeSubject == input$InputArea) %>%
            #filter(TypeSubject == "STEM") %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject)))) %>%
            filter(Year == input$InputYear)
            #filter(Year ==2021)

        totaln2 <- totaln %>%
            filter(Year == input$InputYear) %>%
            filter(TypeSubject == input$InputArea) %>%
            # filter(TypeSubject == "STEM") %>%
            # filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = Can) %>%

            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))




        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," candidates)"))) +

            facet_wrap(Subject~.,nrow=length(unique(STEMonly$Subject)),drop=F,
                       strip.position = "left") +
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            ggtitle(paste0(input$InputYear))+
            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+
            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text"),height=1400) %>%
            layout(margin = list(t = 60),

                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.1,
                               x=0.4,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })

    output$bySubject_Applied <- renderPlotly({

        STEMonly <- dat %>% filter(Subject == input$InputSubject_Applied) %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            #mutate(percent = ifelse(TypeGrade == "Fail", -percent,percent)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Year = factor(Year, levels = c(2021:2008)))



        totaln2 <- totaln %>%
            filter(Subject == input$InputSubject_Applied) %>%
            #filter(TypeSubject == "STEM") %>%
            #filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = ifelse(is.na(Can),0,Can)) %>%
            mutate(Year = factor(Year, levels = c(2021:2008))) %>%


            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))


        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," students)"))) +
            facet_wrap(Year~.,nrow=length(c(2021:2008)),drop=F,
                       strip.position = "left") +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+

            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+
            ggtitle(paste0(input$InputSubject_Applied))+
            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text")) %>%
            layout(margin = list(t = 60),
                   height=1000,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.05,
                               x=0.3,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })

    output$bySubject_STEM <- renderPlotly({

        STEMonly <- dat %>% filter(Subject == input$InputSubject_STEM) %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            #mutate(percent = ifelse(TypeGrade == "Fail", -percent,percent)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Year = factor(Year, levels = c(2021:2008)))



        totaln2 <- totaln %>%
            filter(Subject == input$InputSubject_STEM) %>%
            #filter(TypeSubject == "STEM") %>%
            #filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = ifelse(is.na(Can),0,Can)) %>%
            mutate(Year = factor(Year, levels = c(2021:2008))) %>%


            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))

        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," students)"))) +
            facet_wrap(Year~.,nrow=length(c(2021:2008)),drop=F,
                       strip.position = "left") +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+
            ggtitle(paste0(input$InputSubject_STEM))+
            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+

            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text")) %>%
            layout(margin = list(t = 60),
                   height=1000,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.05,
                               x=0.3,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })

    output$bySubject_Sports <- renderPlotly({

        STEMonly <- dat %>% filter(Subject == input$InputSubject_Sports) %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            #mutate(percent = ifelse(TypeGrade == "Fail", -percent,percent)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Year = factor(Year, levels = c(2021:2008)))


        totaln2 <- totaln %>%
            filter(Subject == input$InputSubject_Sports) %>%
            #filter(TypeSubject == "STEM") %>%
            #filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = ifelse(is.na(Can),0,Can)) %>%
            mutate(Year = factor(Year, levels = c(2021:2008))) %>%


            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))

        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," students)"))) +
            facet_wrap(Year~.,nrow=length(c(2021:2008)),drop=F,
                       strip.position = "left") +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+

            ggtitle(paste0(input$InputSubject_Sports))+
            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+
            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text")) %>%
            layout(margin = list(t = 60),
                   height=1000,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.05,
                               x=0.3,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })

    output$bySubject_Languages <- renderPlotly({

        STEMonly <- dat %>% filter(Subject == input$InputSubject_Languages) %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            #mutate(percent = ifelse(TypeGrade == "Fail", -percent,percent)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Year = factor(Year, levels = c(2021:2008)))



        totaln2 <- totaln %>%
            filter(Subject == input$InputSubject_Languages) %>%
            #filter(TypeSubject == "STEM") %>%
            #filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = ifelse(is.na(Can),0,Can)) %>%
            mutate(Year = factor(Year, levels = c(2021:2008))) %>%


            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))
        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," students)"))) +
            facet_wrap(Year~.,nrow=length(c(2021:2008)),drop=F,
                       strip.position = "left") +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+

            ggtitle(paste0(input$InputSubject_Languages))+
            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+
            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text")) %>%
            layout(margin = list(t = 60),
                   height=1000,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.05,
                               x=0.3,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })


    output$bySubject_Social <- renderPlotly({

        STEMonly <- dat %>% filter(Subject == input$InputSubject_Social) %>%
            group_by(Year,Subject,Gender,TypeGrade) %>%
            summarize(value = sum(Rawnum)) %>%
            mutate(percent = value/sum(value)) %>%
            #mutate(percent = ifelse(TypeGrade == "Fail", -percent,percent)) %>%
            mutate(TypeGrade = factor(TypeGrade,levels=c("Fail","Pass","Top"))) %>%
            arrange(TypeGrade) %>%
            group_by(Year,Subject,Gender) %>%
            mutate(total = round(sum(value)))

        #plottest <- bind_rows(STEMonly,dummyframe)


        plottest <- STEMonly  %>%
            mutate(Year = factor(Year, levels = c(2021:2008)))



        totaln2 <- totaln %>%
            filter(Subject == input$InputSubject_Social) %>%
            #filter(TypeSubject == "STEM") %>%
            #filter(Year ==2021) %>%
            mutate(TypeGrade = "-",
                   percent  = 1,
                   value = ifelse(is.na(Can),0,Can)) %>%
            mutate(Year = factor(Year, levels = c(2021:2008))) %>%


            mutate(Subject = factor(Subject, levels = sort(unique(STEMonly$Subject))))
        toop <- ggplot(plottest,
                       aes(x = percent * 100,y = Gender, group = Gender,fill=TypeGrade,
                           text = paste0("Grade: ", TypeGrade, "\n",round(percent * 100,1), "% (",round(value)," students)"))) +
            facet_wrap(Year~.,nrow=length(c(2021:2008)),drop=F,
                       strip.position = "left") +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Top","Pass")),
                     position = position_stack(reverse = T)) +
            geom_col(data = plottest %>% filter(TypeGrade %in% c("Fail")),
                     aes(x = -percent * 100))+
            geom_vline(xintercept=0)+
            geom_text(data = totaln2, aes(x = -100, y = Gender, label = paste0("N = ",value)),hjust=0,size=3)+

            ggtitle(paste0(input$InputSubject_Social))+
            scale_fill_manual(values = c("#cccccc","#E69F00", "#56B4E9", "#009E73"),drop = FALSE)+
            scale_x_continuous(name="Percentage",breaks=seq(-100,100,25),labels=paste0(abs(seq(-100,100,25)),"%"),
                               limits = c(-100,100))+
            theme_bw()+
            theme_prov


        ggplotly(toop, tooltip = c("text")) %>%
            layout(margin = list(t = 60),
                   height=1000,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.05,
                               x=0.3,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")

    })

    datos <- dat %>% select(TypeSubject,Subject) %>% distinct() %>%
        arrange(TypeSubject,Subject) %>% pivot_wider(names_from = "TypeSubject",values_from ="Subject")


    output$tableApplied = renderTable({
        datos %>% select(Applied) %>% unnest()
    })
    output$tableLanguages = renderTable({
        datos %>% select(Languages) %>% unnest()
    })

    output$tableSocial = renderTable({
        datos[,3] %>% unnest()
    })

    output$tableSports = renderTable({
        datos[,4] %>% unnest()
    })
    output$tableSTEM = renderTable({
        datos[,5] %>% unnest()
    })

}

# Run the application
shinyApp(ui = ui, server = server)
