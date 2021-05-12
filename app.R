########################################################
#################### Libraries #########################
########################################################
library(shiny)
library(tidyverse)
########################################################
################# Data Wrangling #######################
########################################################
#Data Used
BP<-read.csv('BainbridgeIslandShorelineData2019.csv')
#fill with NAs
BP$tidalInfl<-NA
#input values by site
BP[BP$site=='A',]$tidalInfl<-2.886
BP[BP$site=='B',]$tidalInfl<-5.433
BP[BP$site=='C',]$tidalInfl<-5.010
BP[BP$site=='D',]$tidalInfl<-4.440
BP[BP$site=='E',]$tidalInfl<-4.053
BP[BP$site=='F',]$tidalInfl<-4.566
BP[BP$site=='G',]$tidalInfl<-3.888
BP[BP$site=='H',]$tidalInfl<-5.224
BP[BP$site=='K',]$tidalInfl<-6.835
BP[BP$site=='L',]$tidalInfl<-7.950
BP[BP$site=='M',]$tidalInfl<-0.890
BP[BP$site=='N',]$tidalInfl<-4.493
BP[BP$site=='P',]$tidalInfl<-8.403
BP[BP$site=='S',]$tidalInfl<-4.866
BP[BP$site=='U',]$tidalInfl<-5.363
BP[BP$site=='Z',]$tidalInfl<-5.895
#create columns for elevations and distances
bp.df<-BP%>%
    group_by(site)%>%
    mutate(elevation_cm=cumsum(slope_cm),
           width_cm=cumsum(string_cm),
           elevation_m=elevation_cm*0.01,
           width_m=width_cm*0.01,
           tidalElevation_m=elevation_m+tidalInfl,
           elevation_ft=tidalElevation_m*3.281,
           width_ft=width_m*3.281)
#pivot biology longer
BioDF.line<-bp.df[,c(2,9:18,85,86)]%>%
    pivot_longer(!c(site,width_ft,elevation_ft),
                 names_to='Biology',
                 values_to='Presence')
########################################################
####################### UI #############################
########################################################
ui<-fluidPage(
    # Application title
    titlePanel('Bainbridge Island Shoreline Analysis'),
    fluidRow(
        column(3,
            # Select Box for site choices (plot 1)
            selectInput('select','Site for Plot 1',
                        choices=list('Site A','Site B',
                                     'Site C','Site D',
                                     'Site E','Site F',
                                     'Site G','Site H',
                                     'Site K','Site L',
                                     'Site M','Site N',
                                     'Site P','Site S',
                                     'Site U','Site Z'),
                        selected='Site A')),
        column(3,
            # Select Box for Biology Choices (plot 1)
            checkboxGroupInput('select2','Biology for Plot 1',
                               choices=list('Bare Beach','Green Algae',
                                            'Brown Algae','Red Algae',
                                            'Barnacles','Tube Worms',
                                            'Z.japonica','Z.marina',
                                            'Anemones','Mussels'))),
        column(3,
            # Select Box for site choices (plot 2)
            selectInput('select3','Site for Plot 2',
                        choices=list('Site A','Site B',
                                     'Site C','Site D',
                                     'Site E','Site F',
                                     'Site G','Site H',
                                     'Site K','Site L',
                                     'Site M','Site N',
                                     'Site P','Site S',
                                     'Site U','Site Z'),
                        selected='Site A')),
        column(3,
            # Select Box for Biology Choices (plot 2)
            checkboxGroupInput('select4','Biology for Plot 2',
                               choices=list('Bare Beach','Green Algae',
                                            'Brown Algae','Red Algae',
                                            'Barnacles','Tube Worms',
                                            'Z.japonica','Z.marina',
                                            'Anemones','Mussels')))),
    # Plot 1
    fluidPage(
        verticalLayout(
            titlePanel('Plot 1'),
            plotOutput('plot1')
        )),
    # Plot 1
    fluidPage(
        verticalLayout(
            titlePanel('Plot 2'),
            plotOutput('plot2')
        ))
    )
########################################################
###################### Server ##########################
########################################################
server<-function(input,output) {
    #Site Selection for Plot 1
    output$plot1<-renderPlot({ggplot()+
            geom_line(data=BioDF.line%>%
                          filter(site==input$select)%>%
                          filter(Presence==1),
                      aes(x=width_ft,
                          y=elevation_ft))+
            geom_point(data=BioDF.line%>%
                           filter(site==input$select)%>%
                           filter(Presence==1)%>%
                           filter(Biology==input$select2),
                       aes(x=width_ft,
                           y=elevation_ft,
                           color=Biology),
                       alpha=0.8)+
            labs(x=element_blank(),
                 y='Beach Elevation (ft)')+
            theme_classic()+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  legend.position=0)+
            scale_x_continuous(limits=c(0,500))+
            scale_y_continuous(limits=c(-5,30))+
            scale_color_manual(values='dodgerblue1')})
    #Site Selection for Plot 2
    output$plot2<-renderPlot({ggplot()+
            geom_line(data=BioDF.line%>%
                          filter(site==input$select3)%>%
                          filter(Presence==1),
                      aes(x=width_ft,
                          y=elevation_ft))+
            geom_point(data=BioDF.line%>%
                           filter(site==input$select3)%>%
                           filter(Presence==1)%>%
                           filter(Biology==input$select4),
                       aes(x=width_ft,
                           y=elevation_ft,
                           color=Biology),
                       alpha=0.8)+
            labs(x=element_blank(),
                 y='Beach Elevation (ft)')+
            theme_classic()+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  legend.position=0)+
            scale_x_continuous(limits=c(0,500))+
            scale_y_continuous(limits=c(-5,30))+
            scale_color_manual(values='dodgerblue1')})
}
########################################################
##################### Run App ##########################
########################################################
# Run the application 
shinyApp(ui = ui, server = server)
########################################################