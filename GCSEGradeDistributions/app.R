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

library(viridis)
library(dplyr)
library(tibble)
library(tidyr)

library(data.table)

doubleaward <- paste0(c(9:1),"-",c(9:1))
legacy <- c("A*",LETTERS[1:7])
U <- "U"
reformed <- c(9:1)

dat <- read.csv("GCSE data.csv") %>%
    filter(Cumulative == "Non-cumulative percentages") %>% #raw percentages
    filter(Grades %in% c(legacy,reformed,U,doubleaward)) %>%  #unbinned only
    filter(Gender == "All",
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


x <- as.data.table(dat)

y <- dcast(x, Subject + Year + Gender + Age + Type + Candidates + TypeSubject ~ .,
           fun.aggregate = sum,
           value.var = 'Percent')
colnames(y)[8] <- c('PercentTot')

y[, Percent:= 100 - PercentTot]
y[, Grades:= "U"]




dat2 <- tibble::as_tibble(rbind(x, y,fill=TRUE)) %>% arrange(Subject,Year,Grades) %>%
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
                   # strip.text.y = element_text(angle=90),
                    legend.position = "bottom")




totaln <- dat %>% select(Subject,Year,Gender,Candidates,TypeSubject) %>% distinct() %>%
    mutate(TypeSubject = factor(TypeSubject,levels=c("Applied","STEM","Languages","Sports and Arts","Social sciences and Humanities")))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution of GCSE grades"),

    fluidRow(
        column(12,
             tags$p("Data in this shiny app is taken from ",
                    tags$a(href = "https://analytics.ofqual.gov.uk/apps/GCSE/Outcomes/", "analytics.ofqual.gov.uk"), ". The preprocessed data supplied by Ofqual, in turn, is based on data supplied by the Joint Council for Qualifications."),
                    tags$p("For ease of presentation, the following simplifications were made:",
                    tags$ul(tags$li("'all ages' only, not 'Aged 16'"),
                            tags$li("female and male candidates combined"),
                            tags$li("Legacy and reformed separately. Science double-awards are represented within reformed single awards."))),
             tags$p("Subjects are organized by subject area for ease of presentation. Some subject span different subject areas, e.g., Classical subjects, but were still assigned to only one subject area."))),
    fluidRow(
        column(2, tableOutput("tableApplied")),
        column(2, tableOutput("tableLanguages")),
        column(2, tableOutput("tableSocial")),
        column(2, tableOutput("tableSports")),
        column(2, tableOutput("tableSTEM"))),
    fluidRow(
        column(12,
             tags$p("In all heat maps, color gradients range from 0 to 50%,
                    i.e., the same gradient is applied to all graphs, rather than determined relative to the range of values in a given graph.
                    Note, the gradient is determined by the percentage of candidates, not the absolute number of candidates. Hovering over a
                    given data point will show both sets of information."),
             tags$p("Further, the grade 'U' (Unclassified, or below < G and < 1 respectively) was not directly supplied in the Ofqual data set.
                    I inferred it as the percentage points missing from 100 in the supplied data."),
             tags$p("See", tags$a(href = "https://github.com/nklange/gcsegrades/", "github.com/nklange/gcsegrades"), "for the underlying R scripts and data."))),

    tabsetPanel(
        tabPanel("Legacy (A* - G): 2008 - 2018",
                 fluidRow(

                     column(3,
                            tags$br(),
                            selectInput("InputYear", "Year", c(2018:2008))
                     ),
                     column(8,
                            plotlyOutput("Legacy")
                     )
                 )
        ),
       tabPanel("Reformed (9 - 1): 2021 - 2017",

                fluidRow(

                    column(3,
                           tags$br(),
                           selectInput("InputYear2", "Year", c(2021:2017))
                    ),
                    column(8,
                           plotlyOutput("Reformed")
                    )
                )
                )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    output$Legacy <- renderPlotly({



        STEMonly <- dat2 %>% filter(Type=="Legacy")%>%
            mutate(Grades = factor(Grades, levels = c("A*",LETTERS[1:7],"U")))

        plot_width<- STEMonly %>%
            filter(Year == input$InputYear) %>%
            group_by(TypeSubject) %>%
            summarise(num = length(unique(Subject))) %>%
            ungroup() %>%
            mutate(width_pct = num/sum(num))

        STEMonly %>% filter(Year == input$InputYear) %>%
            split(.$TypeSubject) %>%
            purrr::map(function(x) {
                ggplot(data=x,aes(x=Grades,y=Subject,fill=Percent,
                                  text = paste0("Candidates: ", round(Rawnum))))+
                    #facet_wrap(TypeSubject~.)+
                    #ggforce::facet_col(facets = vars(TypeSubject))+
                    facet_grid(TypeSubject~.) +
                    geom_tile()+
                    #theme(strip.text.y = element_text(angle=90))+
                    # coord_flip()+
                    scale_fill_viridis_c(option = "plasma",limits=c(0,50))+
                    #scale_fill_distiller(palette = "Yellow-Green-Blue",direction=1,limits=c(0,50))+
                    #scale_y_discrete(expand = c(0, 0.5)) +

                    theme_bw()+
                    theme_prov
            }) %>%
            subplot(margin = 0.0005, shareX = T, heights = plot_width$width_pct,nrows=5) %>%
            layout(margin = list(t = 60),
                   height = 800,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.1,
                               x=0.4,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")


        # toop <- ggplot(STEMonly %>% filter(Year == input$InputYear), aes(x=Grades,y=Subject,fill=Percent,
        #                                                                  text = paste0("Grade: ", Grades, "\n",round(Percent,1), "% (",round(Rawnum)," candidates)")))+
        #     facet_wrap(TypeSubject ~.,scales="free",nrow=5) +
        #     geom_tile()+
        #     scale_fill_distiller(palette = "Spectral",limits=c(0,50))+
        #     theme_bw()+
        #     theme_prov
        #
        #
        # ggplotly(toop, tooltip = c("text"),height=1000) %>%
        #     layout(margin = list(t = 60),
        #
        #            legend=list(itemclick = "toggleothers",
        #
        #                        bgcolor="white",
        #                        bordercolor="white",
        #                        y = -0.1,
        #                        x=0.4,
        #                        orientation = 'h',
        #                        title = list(font = list(size=14)),
        #                        font = list(size=12)),
        #            hovermode = "closest")

    })

    output$Reformed <- renderPlotly({



        STEMonly <- dat2 %>% filter(Type=="Reformed") %>%
            mutate(Grades = as.character(Grades)) %>%
            mutate(Grades = ifelse(nchar(Grades) > 2, substr(Grades, 1, 1),Grades)) %>%
            mutate(Grades = factor(Grades, levels = c(9:1,"U")))


        plot_width<- STEMonly %>%
            #filter(Year == 2017) %>%
            filter(Year == input$InputYear2) %>%
            group_by(TypeSubject) %>%
            summarise(num = length(unique(Subject))) %>%
            ungroup() %>%
            mutate(width_pct = num/sum(num))

        STEMonly %>%
            #filter(Year == 2017) %>%
            filter(Year == input$InputYear2) %>%
            split(.$TypeSubject) %>%
            purrr::map(function(x) {
                ggplot(data=x,aes(x=Grades,y=Subject,fill=Percent,
                                  text = paste0("Candidates: ", round(Rawnum))))+
             #facet_wrap(TypeSubject~.)+
            #ggforce::facet_col(facets = vars(TypeSubject))+
            facet_grid(TypeSubject~.) +
            geom_tile()+
            #theme(strip.text.y = element_text(angle=90))+
           # coord_flip()+
            scale_fill_viridis_c(option = "plasma",limits=c(0,50))+

            #scale_fill_distiller(palette = "Blues",direction=1,limits=c(0,50))+
            #scale_y_discrete(expand = c(0, 0.5)) +

            theme_bw()+
            theme_prov
            }) %>%
            subplot(margin = 0.0005, shareX = T, heights = plot_width$width_pct,nrows=dim(plot_width)
                    [1]) %>%
            layout(margin = list(t = 60),
                   height = 800,
                   legend=list(itemclick = "toggleothers",

                               bgcolor="white",
                               bordercolor="white",
                               y = -0.1,
                               x=0.4,
                               orientation = 'h',
                               title = list(font = list(size=14)),
                               font = list(size=12)),
                   hovermode = "closest")


        # toop <- ggplot(STEMonly %>% filter(Year == 2020), aes(x=Grades,y=Subject,fill=Percent,
        #                                                                  text = paste0("Grade: ", Grades, "\n",round(Percent,1), "% (",round(Rawnum)," candidates)")))+
        #     facet_grid(TypeSubject ~.,scales = "free", space = "free") +
        #     geom_tile()+
        #     scale_fill_distiller(palette = "Spectral",limits=c(0,50))+
        #     #scale_y_discrete(expand = c(0, 0.5)) +
        #
        #     theme_bw()+
        #     theme_prov
        #
        #
        # plotly::ggplotly(toop, tooltip = c("text")) %>%
        #     layout(margin = list(t = 60),
        #
        #            legend=list(itemclick = "toggleothers",
        #
        #                        bgcolor="white",
        #                        bordercolor="white",
        #                        y = -0.1,
        #                        x=0.4,
        #                        orientation = 'h',
        #                        title = list(font = list(size=14)),
        #                        font = list(size=12)),
        #            hovermode = "closest")

    })


    datos <- dat2 %>% select(TypeSubject,Subject) %>% distinct() %>%
        arrange(TypeSubject,Subject) %>% pivot_wider(names_from = "TypeSubject",values_from ="Subject")


    output$tableApplied = renderTable({
        datos %>% select(Applied) %>% unnest(cols = c("Applied"))
    })
    output$tableLanguages = renderTable({
        datos %>% select(Languages) %>% unnest(cols = c("Languages"))
    })

    output$tableSocial = renderTable({
        datos[,3] %>% unnest(cols = c("Social sciences and Humanities"))
    })

    output$tableSports = renderTable({
        datos[,4] %>% unnest(cols = c("Sports and Arts"))
    })
    output$tableSTEM = renderTable({
        datos[,5] %>% unnest(cols = c("STEM"))
    })

}

# Run the application
shinyApp(ui = ui, server = server)

