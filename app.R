#########################################################
######################## Setup ##########################
#########################################################
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)
library(bslib)
#theme
thematic::thematic_shiny(font='auto')
#create folder for data storage
dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
    url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
    download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
#download data
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")
#import datas
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")
#not needed, but imports datas as well
if (!exists("injuries")) {
    injuries <- vroom::vroom("injuries.tsv.gz")
    products <- vroom::vroom("products.tsv")
    population <- vroom::vroom("population.tsv")
}
#########################################################
################### User Interface ######################
#########################################################
ui <- fluidPage(
    # theme
    theme=bs_theme(),
    # selection variables
    fluidRow(
        column(8,selectInput("code","Product",
                             choices=setNames(products$prod_code,
                                              products$title),
                             width="100%")),
        column(2,selectInput("y","Y axis",
                             c("rate", "count"))),
        column(2,numericInput('N',"Select Number of Table Rows",
                              value=5,min=1,max=10))),
    # table variables
    fluidRow(
        column(4,tableOutput("diag")),
        column(4,tableOutput("body_part")),
        column(4,tableOutput("location"))),
    # plot variable
    fluidRow(
        column(12, plotOutput("age_sex"))),
    # narrative variables
    fluidRow(
        column(3,actionButton('AfterStory',"Tell me a new story")),
        column(3,actionButton("BeforeStory","Tell me the previous story")),
        column(10,textOutput("narrative")))
)
#########################################################
################# Function Junction #####################
#########################################################
count_top<-function(df,var,n=5){
    df%>%
        mutate({{var}}:=fct_lump(fct_infreq({{var}}),n=n))%>%
        group_by({{var}})%>%
        summarise(n=as.integer(sum(weight)))
}
#########################################################
###################### Server ###########################
#########################################################
server<-function(input,output,session){
    # theme
    bs_themer()
    # objects for outputs
    selected<-reactive(injuries%>%
                           filter(prod_code==input$code))
    userN<-reactive(input$N)
    # table output
    output$diag<-renderTable(count_top(selected(),
                                       diag,
                                       userN()),
                             width="100%")
    output$body_part<-renderTable(count_top(selected(),
                                            body_part,
                                            userN()),
                                  width="100%")
    output$location<-renderTable(count_top(selected(),
                                           location,
                                           userN()),
                                 width="100%")
    # plot output
    summary<-reactive({
        selected()%>%
            count(age,sex,wt=weight)%>%
            left_join(population,by=c("age","sex"))%>%
            mutate(rate=n/population*1e4)
    })
    # plot binary selection
    output$age_sex<-renderPlot({
        if(input$y=="count")
            {summary()%>%
                ggplot(aes(age,n,colour=sex))+
                geom_line(size=1)+
                theme_bw()+
                labs(y="Estimated number of injuries")+
                scale_color_manual(values=PNWColors::pnw_palette('Bay',2))
        }
        else
            {summary()%>%
                ggplot(aes(age,rate,colour=sex))+
                geom_line(na.rm=T,size=1)+
                theme_bw()+
                labs(y="Injuries per 10,000 people")+
                scale_color_manual(values=PNWColors::pnw_palette('Bay',2))
        }
    },res=96)
    
    # circular narrative section edited using code from: 
    # https://mastering-shiny-solutions.org/case-study-er-injuries.html
    
    # narrative section
    # Find the maximum possible number of rows.
    max_no_rows<-reactive(
        max(length(unique(selected()$diag)),
            length(unique(selected()$body_part)),
            length(unique(selected()$location)))
    )
    # Update the maximum value for the numericInput based on max_no_rows().
    observeEvent(input$code,{
        updateNumericInput(session,'rows',max=max_no_rows())
    })
    # Store the maximum possible number of stories.
    max_no_stories<-reactive(length(selected()$narrative))
    # Reactive used to save the current position in the narrative list.
    story<-reactiveVal(1)
    # Reset the story counter if the user changes the product code. 
    observeEvent(input$code,{
        story(1)
    })
    # When the user clicks "Next story", increase the current position in the
    # narrative but never go beyond the interval [1, length of the narrative].
    # Note that the mod function (%%) is keeping `current`` within this interval.
    observeEvent(input$AfterStory,{
        story((story()%%
                   max_no_stories())+1)
    })
    # When the user clicks "Previous story" decrease the current position in the
    # narrative. Note that we also take advantage of the mod function.
    observeEvent(input$BeforeStory,{
        story(((story()-2)%%
                   max_no_stories())+1)
    })
    output$narrative<-renderText({
        selected()$narrative[story()]
    })
    #>>
}
#########################################################
##################### END/Run App #######################
#########################################################
shinyApp(ui, server)