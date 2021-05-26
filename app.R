########################################################
#################### Libraries #########################
########################################################
library(shiny)#running this app
library(tidyverse)#data manipulation (pipes)
library(PNWColors)#color palettes
library(plotly)#interactive plots
########################################################
###################### Data ############################
########################################################
# Slope/Biology
BioDF.line<-read.csv('SlopeBio.csv')
# Sediment
All.Sed<-read.csv('AllSed.csv')
# Reorder Sediment to go from high to mid to low elevations
All.Sed$location<-factor(All.Sed$location,levels=c('H','M','L'))
# wrack line
wracked<-read.csv('wracked.csv')
########################################################
####################### UI #############################
########################################################
ui<-fluidPage(
    # Application title
    titlePanel('Bainbridge Island Shoreline Analysis'),
    tabsetPanel(type='tabs',
    #####
    tabPanel('Introduction',
    sidebarLayout(
        sidebarPanel(
            # Welcome section
            h1('Welcome!',align='center'),
            p("Hi, I'm Skylar Wood and this is my first Shiny App. The purpose of this app is to allow you to explore the data of a project I was a part of in 2019, and for me to test out a few different ways of visualizing the data using shiny. As part of the Shoreline Master's Program, Western Washington University faculty and students collaborated with the City of Bainbridge Island in an effort to establish baseline monitoring data for the shoreline of Bainbridge Island, WA.")),
            # Site Map Image (note: never name these files with CAPS!)
        mainPanel(img(src='bblayout.png',
                      height=(3300)*0.17,
                      width=(2550)*0.17))),
    # Methods section
    mainPanel(
        h2('Methods',align='center'),
        p('The methods are split into the following three sections:' ),
        p('* Beach Parameters (Profile, Biology, sediment, and wrack line)'),
        p('* Vegetation (log line, understory, overstory, dunegrass, nativity, and function)'),
        p('* Water Chemistry'),
        p("However, because of time I will exclude about half of the sections/subsections."),
        tagList("Most methods began, or are linked to, the online ",
                a("Shoreline Monitoring Toolbox Protocols",
                  href="https://sites.google.com/a/uw.edu/toolbox/protocols")),
        # beach profile
        h3("Beach Profile",align='center'),
        p("To measure the beach profile, or the topography and slope of the beach, from the Mean Higher High Water (MHHW) to the Mean Lower Low Water (MLLW). The ‘string method’ for measuring beach profile requires three people, a 100-meter transect tape, two measuring sticks, two levels and a 3.5-meter, or longer, string. The steps include:"),
        p("1. Set the transect tape perpendicular to the shoreline, spanning from the MHHW to the MLLW. This may not require all 100 meters of the transect tape. Take four photos of the starting location from various perspectives."),
        br(),
        # hyperlink 
        tagList("2.Person one places a measuring stick vertically at the starting location, marked by a GPS coordinate and compass direction. The GPS location and compass direction of the starting location were taken using the phone application",
                a("GPS & Maps: Track Coordinates, Compass + Waypoints' by 2kit Consulting",
                  href="https://apps.apple.com/us/app/gps-maps-trackcoordinate-compass-waypoints/id477998011")),
        br(),
        br(),
        p("3.Hold a string at a specific measurement along the first measuring stick. Record this measurement. The field team used 80 cm as the specific measurement."),
        p("4.Person two places a second measuring stick vertically three feet down the transect tape and holds the other end of the string (held by person one) perpendicular to the second measuring stick (see Figure 2.3). Use a level to ensure that the measuring sticks are vertical and that the string is horizontal."),
        p("5. Person three measures and records the height where the string intersects the second measuring stick."),
        p("6. Person one moves the initial measuring stick three feet down the transect from person two and repeats the above measurements. Repeat this process until the low tide line is crossed."),
        p("7. Note the time and the low-tide line location along the transect. Later, consult the nearest tide station for the tidal elevation."),
        br(),
        p("When processing the field data, enter the data into the Excel spreadsheet. Calculate the beach slope from MHHW to MLLW by finding the difference between the string heights at each point along the transect. The resulting slope line can then be translated to elevation points by consulting the local tide station for tidal elevation at the time recorded in step 7."),
        p("This protocol is resource intensive as it requires three people to properly measure beach profile. Several sites are very muddy which, during the baseline data collection, introduced a physical challenge to taking precise measurements. Additionally, to collect the greatest amount of the beach profile data requires being at a site at the lowest tide so that the transect line passes MLLW. This timing does not always align with adequate sunlight. Working around the timing of low tides posed another challenge during the baseline data collection and beach profile was often measured at only one site per day because of this." ),
        p("The Shoreline Monitoring Toolbox recommends measuring the beach profile with a laser level instead of with the string method (Toft et al., 2015). The string method was used for the baseline data collection as a laser level was not available for the field team."),
        # biology
        h3("Sessile Biology",align='center'),
        p("Record the categories of sessile organisms present within 1-meter of either side of the transect points measured for the beach profile. The categories include green algae, brown algae, red algae, barnacles, tube worms, dwarf eelgrass (Z. japonica), common eelgrass (Z. marina), mussels, and anemones. On the field data sheet (see Appendix 1), each category of organisms is recorded as present at each measured transect point when at least one individual of that category is present. It is critical that an identification tool, like a dichotomous key, or an expert, is available when identifying sessile biology to ensure proper identification."),
        # sediment
        h3("Sediment",align='center'),
        p("The materials needed to measure sediment size include a 50-meter transect tape, a 32x32 cm quadrat, divided with string into 25 6x6 cm squares (see Figure 2.4), and a hand trowel. Establish three 50-meter transects at: MHHW, MSL, and MLLW. The transects should be set perpendicular to the beach profile transect, generally to the left of the beach profile transect line when the researcher is facing the water. In several cases, the 50-meter transect was set to the right of the beach profile transect because property lines or natural boundaries prevented the 50-meter transect form being set to the left side."), 
        p("Five sediment size classes:"),
        p("Cobble (>6 cm)"),
        p("Pebble (4 mm to 6 cm)"),
        p("Granule (2-4 mm)"),
        p("Sand (“gritty” up to 2 mm)"),
        p("Silt/clay (smooth between your fingers)"),
        p("The MHHW transect should start from the same starting point as the beach profile measurements (the recorded GPS points included in Chapter 1 for the baseline data collection), or just below the new wrack line. The MLLW transect should be set at the low tide line, and the MSL transect should be exactly halfway between the MHHW and MLLW transects"),
        p("Place a quadrat at five random points along each transect. Within the quadrat, make a visual estimate of the percent cover of surface and subsurface (five cm depth) sediment of each of the five sediment size classes: cobble, pebble, granule, sand, silt/clay. Use the 25 smaller squares within the quadrat to make this estimate. Each small square equals 4% of the quadrat. If the quadrat contains wooden logs or pillars, consider this negative space. For example, if a pillar takes up 25% of a quadrat, then the total percentage for the quadrat is 75% instead of 100%."),
        p("A challenge in the low-tide sediment size classifications is that there is a limited window to take these measurements. Once the recorder starts taking the measurements, the tide almost immediately comes back up, making the low tide sediment quadrat samples increasingly more difficult to sample. This can be remedied by collecting the sediment in a bag and sifting later or higher on the beach to avoid the incoming tide"),
        # wrack line
        h3("Beach Wrack",align='center'),
        p("Materials needed to measure beach wrack include a 50-meter transect tape and a 32 x 32 cm quadrat divided with string into 25 6x6 cm squares. Measure both the newer wrack line (closest to the water) and the older wrack line (furthest from the water) at each site. The MHHW transect used in the sediment size measurements can be used, again, for the newer wrack line transect. This transect should be at the most recent high tide line that has fresh wrack deposition. Another 50-meter transect should be set up parallel to and inland from the newer wrack line transect 
at the old wrack line"),
        p("At ten randomly determined points along each transect, place a quadrat on the beach surface and make a visual estimate of the percent composition of green algae, brown algae, red algae, dead algae, eelgrass, terrestrial plants, and trash. Use the 25 smaller squares within the quadrat to make this estimate. Each small square equals 4% of the quadrat."),
        p("During the preliminary field collection, there was not an obvious old wrack line at some sites. When this was the case, old beach wrack data was not collected. At sites where a rock wall was present (i.e. in the case of shoreline armoring) and the old wrack line was within the rock wall, a quadrat was estimated (a quadrat could not be placed over the entire sample area)."),
        p("Additionally, in cases where the old and new wrack lines overlapped due to the shoreline’s dynamic 
shape, the random quadrat sample for either the old or new wrack line was observed normally."),
        # log line
        h3("Log Line",align='center'),
        p("Set up a 50-meter transect tape along the upper shoreline, perpendicular to the beach profile transect, or use the MHHW transect from the sediment size measurements. See Table 2.1 for information about the 50-meter transect location used during the baseline data collection. Mark five randomly determined points along the 50-meter transect. At each point along a line perpendicular to the transect, measure and record the distance between the log furthest from and closest to the water. This is the width of the log line. Next, count and record the abundance of small logs (smaller than 2 meters) and large logs (larger than 2 meters) that intersect the measuring tape. Record if these are natural or human altered. Make note of any other characteristics of these logs, such as moss, lichen, barnacles, or plants present on the logs."),
        p("A physical challenge with the log line method includes the need for the recorder to walk through vegetation, the mud or on uneven ground at certain sites. This can make it difficult to accurately measure the log line."),
        # vegetation
        h3("Vegetation",align='center'),
        p("The vegetation monitoring protocols are detailed in the vegetation protocol sheet from the Shoreline Monitoring Toolbox (Toft et al., 2015). Materials needed to measure vegetation include two 50-meter transect tapes, a 0.25m² quadrat, and a plant identification tool, such as a dichotomous key. Set up a 50-meter transect tape along the upper shoreline, in an area where some vegetation is present. See Table 2.1 for information about the 50-meter transect placement during the baseline data collection. Record the GPS location and compass direction of the starting point of the transect. At three locations that are representative of the vegetation along the transect, delineate a 5-meter x 5-meter plot. The plot does not need to intersect the transect. Within each quadrat:"),
        p("1. Generate a plant species list: identify and record species present within the plot, noting native and introduced species."),
        p("2. Measure the percent cover of overstory and understory vegetation: make a visual estimate of the percent cover of understory and overstory species present within each plot."),
        p("3. Characterize the health ratings of vegetation: give each understory and overstory species within each plot an average health rating between 1 (dead) and 5 (vigorous growth)."),
        p("4. Measure the canopy diameter of trees: use a transect tape to estimate the percent cover of each overstory species within each plot."),
        p("5. Record dunegrass presence, dunegrass bed width, percent cover, and density: establish a transect parallel to shore along the length of any dunegrass patches present, or for 50 m for large patches. Record dunegrass presence/absence at five random points along the transect line. Where dunegrass is present, use a 0.25m² quadrat to estimate shoot density and percent cover."),
        p("The Shoreline Monitoring Toolbox (Toft et al., 2015) contains protocols for estimating the supratidal (the area above the high tide line that is often splashed with water but not submerged) and backshore (the area between the coastline and the extreme inland limit of the beach that is only exposed to waves during extreme tides or storm surges) vegetation. However, the field team determined that the terrain was not hospitable to safely execute this method. In response, for the baseline data collection, presence/absence of understory and overstory cover was recorded instead"),
        p("6. Understory and overstory vegetation presence/absence: at each meter interval from zero to fifty along the transect line, measure and record the presence/absence of understory and overstory vegetation. Vegetation is recorded as present at each measured transect point when any vegetation is present."),
        # water chemistry
        h3("Water Chemistry",align='center'),
        p("Materials needed to measure water chemistry include a 50-meter transect line, a Hydrolab MS5 sonde, and Hanna nitrate and phosphate kits. To measure water chemistry, establish a 50-meter transect line parallel to the water's edge. At three random locations along the line, walk perpendicular from the line and into the water to a depth of about one meter, lay the Hydrolab MS5 sonde down, and record the stabilized readings.")
    )),
    #####
    #####
    # Slope/Biology Tab
    tabPanel('Slope/Biology',
    # Vertical Spacing
    br(),
    # Selections
    fluidRow(
    # Select Box for site choices (Slope/Bio plot 1)
        column(3,
            selectInput('slope.plot.1','Site for Plot 1',
                        choices=list('Agate Passage'='A',
                                     'Manzanita Bay'='B',
                                     'Battle Point'='C',
                                     'Fletcher Bay'='D',
                                     'Gazzam Trail'='E',
                                     'Point White Dock'='F',
                                     'Lynwood Center'='G',
                                     'Fort Ward'='H',
                                     'Blakely Harbor'='K',
                                     'Pitchard Park'='L',
                                     'Adams Park'='M',
                                     'Eagle Harbor'='N',
                                     'Hawley Cove'='P',
                                     'Manitou Beach'='S',
                                     'Fay Bainbridge'='U',
                                     'West Port Madison'='Z'),
                        selected='Site A')),
    # Select Box for Biology Choices (Slope/Bio plot 1)
        column(3,
            checkboxGroupInput('bio.plot.1','Biology for Plot 1',
                               choices=list('Bare Beach'='Bare',
                                            'Green Algae'='Green_Algae',
                                            'Brown Algae'='Brown_Algae',
                                            'Red Algae'='Red_Algae',
                                            'Barnacles'='Barnacles',
                                            'Tube Worms'='Tube_Worms',
                                            'Z.japonica'='Z_japonica',
                                            'Z.marina'='Z_marina',
                                            'Anemones'='Anemone',
                                            'Mussels'='Mussels'))),
    # Select Box for site choices (Slope/Bio plot 2)
        column(3,
            selectInput('slope.plot.2','Site for Plot 2',
                        choices=list('Agate Passage'='A',
                                     'Manzanita Bay'='B',
                                     'Battle Point'='C',
                                     'Fletcher Bay'='D',
                                     'Gazzam Trail'='E',
                                     'Point White Dock'='F',
                                     'Lynwood Center'='G',
                                     'Fort Ward'='H',
                                     'Blakely Harbor'='K',
                                     'Pitchard Park'='L',
                                     'Adams Park'='M',
                                     'Eagle Harbor'='N',
                                     'Hawley Cove'='P',
                                     'Manitou Beach'='S',
                                     'Fay Bainbridge'='U',
                                     'West Port Madison'='Z'),
                        selected='Site A')),
    # Select Box for Biology Choices (Slope/Bio plot 2)
        column(3,
            checkboxGroupInput('bio.plot.2','Biology for Plot 2',
                               choices=list('Bare Beach'='Bare',
                                            'Green Algae'='Green_Algae',
                                            'Brown Algae'='Brown_Algae',
                                            'Red Algae'='Red_Algae',
                                            'Barnacles'='Barnacles',
                                            'Tube Worms'='Tube_Worms',
                                            'Z.japonica'='Z_japonica',
                                            'Z.marina'='Z_marina',
                                            'Anemones'='Anemone',
                                            'Mussels'='Mussels'))),
    # Slope/Bio Plot 1
    fluidPage(
        verticalLayout(
            titlePanel('Plot 1'),
            plotlyOutput('slope.bio.plot.1')
    )),
    # Slope/Bio Plot 2
    fluidPage(
        verticalLayout(
            titlePanel('Plot 2'),
            plotlyOutput('slope.bio.plot.2')))
    )),
    #####
    #####
    # Sediment Tab
    tabPanel('Sediment',
    # Vertical Spacing
    br(),
    # Selections
    fluidRow(
    # Select Box for site choices (sediment plot 1)
         column(6,
                selectInput('sed.select.1','Site 1',
                            choices=list('Agate Passage'='A',
                                         'Manzanita Bay'='B',
                                         'Battle Point'='C',
                                         'Fletcher Bay'='D',
                                         'Gazzam Trail'='E',
                                         'Point White Dock'='F',
                                         'Lynwood Center'='G',
                                         'Fort Ward'='H',
                                         'Blakely Harbor'='K',
                                         'Pitchard Park'='L',
                                         'Adams Park'='M',
                                         'Eagle Harbor'='N',
                                         'Hawley Cove'='P',
                                         'Manitou Beach'='S',
                                         'Fay Bainbridge'='U',
                                         'West Port Madison'='Z'),
                            selected='A')),
         column(6,
    # Select Box for site choices (sediment plot 2)
                selectInput('sed.select.2','Site 2',
                            choices=list('Agate Passage'='A',
                                         'Manzanita Bay'='B',
                                         'Battle Point'='C',
                                         'Fletcher Bay'='D',
                                         'Gazzam Trail'='E',
                                         'Point White Dock'='F',
                                         'Lynwood Center'='G',
                                         'Fort Ward'='H',
                                         'Blakely Harbor'='K',
                                         'Pitchard Park'='L',
                                         'Adams Park'='M',
                                         'Eagle Harbor'='N',
                                         'Hawley Cove'='P',
                                         'Manitou Beach'='S',
                                         'Fay Bainbridge'='U',
                                         'West Port Madison'='Z'),
                            selected='B'))),
    # Sediment surface plot 1
    fluidPage(
        column(6,
            titlePanel('Site 1 (Surface)'),
            plotOutput('sed.plot.1')),
    # Sediment surface Plot 2
        column(6,
            titlePanel('Site 2 (Surface)'),
            plotOutput('sed.plot.2'))),
    # Sediment 5cm below plot 1
    fluidPage(
        column(6,
            titlePanel('Site 1 (5cm deep)'),
            plotOutput('sed.plot.3')),
    # Sediment 5cm below plot 2
        column(6,
            titlePanel('Site 2 (5cm deep)'),
            plotOutput('sed.plot.4')))),
    #####
    #####
    # Beach Wrack Line Tab
    tabPanel('Wrack Line',
     # Vertical Spacing
     br(),
     # Selections
     fluidRow(
         # Select Box for site choices (wrack plot 1)
         column(6,
                selectInput('wrack.select.1','Site 1',
                            choices=list('Agate Passage'='A',
                                         'Manzanita Bay'='B',
                                         'Battle Point'='C',
                                         'Fletcher Bay'='D',
                                         'Gazzam Trail'='E',
                                         'Point White Dock'='F',
                                         'Lynwood Center'='G',
                                         'Fort Ward'='H',
                                         'Blakely Harbor'='K',
                                         'Pitchard Park'='L',
                                         'Adams Park'='M',
                                         'Eagle Harbor'='N',
                                         'Hawley Cove'='P',
                                         'Manitou Beach'='S',
                                         'Fay Bainbridge'='U',
                                         'West Port Madison'='Z'),
                            selected='A')),
         column(6,
                # Select Box for site choices (wrack plot 2)
                selectInput('wrack.select.2','Site 2',
                            choices=list('Agate Passage'='A',
                                         'Manzanita Bay'='B',
                                         'Battle Point'='C',
                                         'Fletcher Bay'='D',
                                         'Gazzam Trail'='E',
                                         'Point White Dock'='F',
                                         'Lynwood Center'='G',
                                         'Fort Ward'='H',
                                         'Blakely Harbor'='K',
                                         'Pitchard Park'='L',
                                         'Adams Park'='M',
                                         'Eagle Harbor'='N',
                                         'Hawley Cove'='P',
                                         'Manitou Beach'='S',
                                         'Fay Bainbridge'='U',
                                         'West Port Madison'='Z'),
                            selected='B'))),
     # wrack new plot 1
     fluidPage(
         column(6,
                titlePanel('Site 1 (New Line)'),
                plotOutput('wrack.plot.1')),
         # wrack new Plot 2
         column(6,
                titlePanel('Site 2 (New Line)'),
                plotOutput('wrack.plot.2'))),
     # wrack old plot 1
     fluidPage(
         column(6,
                titlePanel('Site 1 (Old Line)'),
                plotOutput('wrack.plot.3')),
         # wrack old plot 2
         column(6,
                titlePanel('Site 2 (Old Line)'),
                plotOutput('wrack.plot.4'))))))
     #####
     #####
########################################################
###################### Server ##########################
########################################################
server<-function(input,output) {
    # Slope/Bio Plot 1
    output$slope.bio.plot.1<-
        renderPlotly({ggplotly(ggplot()+
            geom_line(data=BioDF.line%>%
                          filter(site%in%input$slope.plot.1),
                      aes(x=round(width_ft,1),
                          y=round(elevation_ft,1)))+
            geom_point(data=BioDF.line%>%
                           filter(site%in%input$slope.plot.1)%>%
                           filter(Biology%in%input$bio.plot.1),
                       aes(x=round(width_ft,1),
                           y=round(elevation_ft,1),
                           fill=Bio.Colors),
                       alpha=0.6,
                       size=3,
                       shape=22,
                       color='white')+
            labs(x=element_blank(),
                 y='Beach Elevation (ft)')+
            theme_classic()+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  legend.position=0)+
            scale_x_continuous(limits=c(0,500))+
            scale_y_continuous(limits=c(-5,30))+
            scale_fill_manual(values=c(black='black',
                                        seagreen='seagreen',
                                        chocolate4='chocolate4',
                                        firebrick='firebrick',
                                        bisque2='bisque2',
                                        gold='gold',
                                        green2='green2',
                                        green4='green4',
                                        orange='orange',
                                        purple='purple')))})
    # Slope/Bio Plot 2
    output$slope.bio.plot.2<-
        renderPlotly({ggplotly(ggplot()+
            geom_line(data=BioDF.line%>%
                          filter(site%in%input$slope.plot.2),
                      aes(x=round(width_ft,1),
                          y=round(elevation_ft,1)))+
            geom_point(data=BioDF.line%>%
                           filter(site%in%input$slope.plot.2)%>%
                           filter(Biology%in%input$bio.plot.2),
                       aes(x=round(width_ft,1),
                           y=round(elevation_ft,1),
                           fill=Bio.Colors),
                       alpha=0.6,
                       size=3,
                       shape=22,
                       color='white')+
            labs(x=element_blank(),
                 y='Beach Elevation (ft)')+
            theme_classic()+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  legend.position=0)+
            scale_x_continuous(limits=c(0,500))+
            scale_y_continuous(limits=c(-5,30))+
            scale_fill_manual(values=c(black='black',
                                       seagreen='seagreen',
                                       chocolate4='chocolate4',
                                       firebrick='firebrick',
                                       bisque2='bisque2',
                                       gold='gold',
                                       green2='green2',
                                       green4='green4',
                                       orange='orange',
                                       purple='purple')))})
    # Sediment Plot 1
    output$sed.plot.1<-
        renderPlot({ggplot()+
            geom_bar(data=All.Sed%>%
                         filter(site%in%input$sed.select.1)%>%
                         filter(height=='Surface'),
                     aes(y=value,
                         x=location,
                         fill=size),
                     stat="identity",
                     position='stack')+
            theme_bw()+
            scale_fill_manual(values=rev(pnw_palette('Mushroom')))+
            labs(x='Tidal Location (H=high, M=mid, L=low)',
                 y='Average Cover (%)',
                 fill='Sediment Size')})
    # Sediment Plot 2
    output$sed.plot.2<-
        renderPlot({ggplot()+
                geom_bar(data=All.Sed%>%
                             filter(site%in%input$sed.select.2)%>%
                             filter(height=='Surface'),
                         aes(y=value,
                             x=location,
                             fill=size),
                         stat="identity",
                         position='stack')+
                theme_bw()+
                scale_fill_manual(values=rev(pnw_palette('Mushroom')))+
                labs(x='Tidal Location (H=high, M=mid, L=low)',
                     y='Average Cover (%)',
                     fill='Sediment Size')})
    # Sediment Plot 3 (site 1 deep)
    output$sed.plot.3<-
        renderPlot({ggplot()+
                geom_bar(data=All.Sed%>%
                             filter(site%in%input$sed.select.1)%>%
                             filter(height=='5cm Deep'),
                         aes(y=value,
                             x=location,
                             fill=size),
                         stat="identity",
                         position='stack')+
                theme_bw()+
                scale_fill_manual(values=rev(pnw_palette('Mushroom')))+
                labs(x='Tidal Location (H=high, M=mid, L=low)',
                     y='Average Cover (%)',
                     fill='Sediment Size')})
    # Sediment Plot 4 (site 2 deep)
    output$sed.plot.4<-
        renderPlot({ggplot()+
                geom_bar(data=All.Sed%>%
                             filter(site%in%input$sed.select.2)%>%
                             filter(height=='5cm Deep'),
                         aes(y=value,
                             x=location,
                             fill=size),
                         stat="identity",
                         position='stack')+
                theme_bw()+
                scale_fill_manual(values=rev(pnw_palette('Mushroom')))+
                labs(x='Tidal Location (H=high, M=mid, L=low)',
                     y='Average Cover (%)',
                     fill='Sediment Size')})
    # Wrack New Plot 1
    output$wrack.plot.1<-
        renderPlot({ggplot()+
                geom_bar(data=wracked%>%
                             filter(Site%in%input$wrack.select.1)%>%
                             filter(wrackline=='New'),
                         aes(y=value,
                             x=wrackline,
                             fill=variable),
                         stat="identity",
                         position='stack',
                         color='black')+
                theme_bw()+
                scale_fill_manual(values=c('white',
                                           'sandybrown',
                                           'grey80',
                                           'springgreen',
                                           'yellowgreen',
                                           'red4',
                                           'forestgreen',
                                           'grey25'))+
                labs(x=element_blank(),
                     y='Average Cover (%)',
                     fill='Debris')})
    # Wrack New Plot 2
    output$wrack.plot.2<-
        renderPlot({ggplot()+
                geom_bar(data=wracked%>%
                             filter(Site%in%input$wrack.select.2)%>%
                             filter(wrackline=='New'),
                         aes(y=value,
                             x=wrackline,
                             fill=variable),
                         stat="identity",
                         position='stack',
                         color='black')+
                theme_bw()+
                scale_fill_manual(values=c('white',
                                           'sandybrown',
                                           'grey80',
                                           'springgreen',
                                           'yellowgreen',
                                           'red4',
                                           'forestgreen',
                                           'grey25'))+
                labs(x=element_blank(),
                     y='Average Cover (%)',
                     fill='Debris')})
    # Wrack Old Plot 3 (site 1)
    output$wrack.plot.3<-
        renderPlot({ggplot()+
                geom_bar(data=wracked%>%
                             filter(Site%in%input$wrack.select.1)%>%
                             filter(wrackline=='Old'),
                         aes(y=value,
                             x=wrackline,
                             fill=variable),
                         stat="identity",
                         position='stack',
                         color='black')+
                theme_bw()+
                scale_fill_manual(values=c('white',
                                           'sandybrown',
                                           'grey80',
                                           'springgreen',
                                           'yellowgreen',
                                           'red4',
                                           'forestgreen',
                                           'grey25'))+
                labs(x=element_blank(),
                     y='Average Cover (%)',
                     fill='Debris')})
    # Wrack Old Plot 4 (site 2)
    output$wrack.plot.4<-
        renderPlot({ggplot()+
                geom_bar(data=wracked%>%
                             filter(Site%in%input$wrack.select.2)%>%
                             filter(wrackline=='Old'),
                         aes(y=value,
                             x=wrackline,
                             fill=variable),
                         stat="identity",
                         position='stack',
                         color='black')+
                theme_bw()+
                scale_fill_manual(values=c('white',
                                           'sandybrown',
                                           'grey80',
                                           'springgreen',
                                           'yellowgreen',
                                           'red4',
                                           'forestgreen',
                                           'grey25'))+
                labs(x=element_blank(),
                     y='Average Cover (%)',
                     fill='Debris')})
}
########################################################
##################### Run App ##########################
########################################################
# Run the application 
shinyApp(ui = ui, server = server)
########################################################