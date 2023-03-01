library(leaflet)
library(plotly)

shinyUI(pageWithSidebar(
    headerPanel("Analysis of Reported Voting Areas using Open Elections Data"),
    sidebarPanel(
        width = 2,
        selectInput("state2", "STATE",
                    choices = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL",
                                "GA","HI","IA","ID","IL","IN","KS","KY","LA","MA",
                                "MD","ME","MI","MN","MO","MS","MT","NC","ND","NE",
                                "NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI",
                                "SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY"),
                    selected = "FL",
                    multiple = FALSE),
        checkboxInput("countyfiles","County Files",value = FALSE),
        numericInput("xyear", "YEAR", 2022, max = 2022),
        # selectInput("xyear", "YEAR",
        #             choices = c("2020","2022"),
        #             selected = "2022",
        #             multiple = FALSE),
        selectInput("xelection", "ELECTION",
                    choices =  "20221108__fl__general",
                    selected = "20221108__fl__general",
                    multiple = FALSE),
        selectInput("xcounty", "COUNTY",
                    choices = c("Glades","Hendry","Highlands","Lafayette","Miami-Dade","Nassau","Okeechobee","Santa Rosa"),
                    selected = "Miami-Dade",
                    multiple = FALSE),
        selectInput("xoffice", "OFFICE",
                    choices = c("Governor",
                                "U.S. Senate",
                                "U.S. House"),
                    selected = "Governor",
                    multiple = FALSE),
        splitLayout(
            actionButton("addrace", "ADD RACE",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            actionButton("clrrace", "CLEAR",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        selectInput("xraces", "RACES",
                    choices  = c("FL_221108_Governor","FL_221108_U.S. Senate"),
                    selected = "",
                    multiple = TRUE),
        #textInput("xarea", "AREA", value = ""),
        #splitLayout(
            selectInput("xparty1", "PARTY",
                        choices = c("DEM","REP","(all)"),
                        selected = "(all)",
                        multiple = FALSE),
            #selectInput("dist", "DISTRICT", choices = c(""), selected = ""),
        #),
        splitLayout(
            checkboxInput("sumparty","Sum multiparty",value = FALSE),
            checkboxInput("topparty","Top multiparty",value = FALSE)
        ),
        splitLayout(
            textInput("areamod", "AREA modify", value = "#=TOTAL>MERGE"),
            textInput("areafilter", "filter", value = "")
        ),
        splitLayout(
            checkboxInput("showother","Show Other",value = TRUE),
            textInput("namefmt", "namefmt", value = "%s_%1.1s")
        ),
        splitLayout(
            numericInput("minvotes","Min Votes",20,min = 0),
            numericInput("minvotes2","Min Votes2",0,min = 0)
        ),
        selectInput("dist", "DISTRICT", choices = c(""), selected = ""),
        selectInput("units", "Units",
                    choices = c("Count","Percent","Percent ratio"),
                    selected = "Percent",
                    multiple = FALSE),
        selectInput("sortcounty", "Sort Counties",
                    choices = c("COUNTY","AREAS","VOTES","deltaM","deltaMxV","totalM","votesM"),
                    selected = "COUNTY",
                    multiple = FALSE),
        radioButtons("sortcountydir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        # numericInput("xsortcol_1", "Sort Areas (column)", 3),
        # radioButtons("xsortdir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        splitLayout(
            numericInput("xsortcol", "Sort Areas,", 3),
            numericInput("xsortcol2", "Areas2 (col)", 0)
        ),
        splitLayout(
            checkboxInput("xsortdesc","Desc",value = FALSE),
            checkboxInput("xsortdesc2","Desc",value = FALSE)
        ),
        selectInput("party", "Party",
                    choices = c("Democrat","Republican","Margin","Total"),
                    selected = "Margin",
                    multiple = FALSE),
        numericInput("cleanlevel", "Clean Level", 2, min = 0),
        textInput("incl_cand", "Include candidates", value = "1-99"),
        checkboxInput("totall_cand","Total all candidates",value = TRUE),
        textInput("skip_rows", "Skip rows", value = ""),
        textInput("areaname", "Area name", value = " County"),
        checkboxInput("toupper","To Uppercase",value = TRUE),
        checkboxInput("displaydists","Display Districts",value = FALSE),
        checkboxInput("displaytotal","Display TOTAL",value = FALSE),
        checkboxInput("createfiles","Create Data Files",value = FALSE)
    ),
    mainPanel(
        tabsetPanel(id = "tabs", selected = "Areas",
            tabPanel("Counties",
                mainPanel(
                    width = 12,
                    verbatimTextOutput("myTextCounties")
                )
            ),
            tabPanel("Areas",
                     sidebarPanel(
                         width = 2,
                         downloadButton("getcsv","Get CSV"),
                         downloadButton("getexcel","Get Excel")
                     ),
                     mainPanel(
                         width = 12,
                         verbatimTextOutput("myTextAreas")
                     )
            ),
            tabPanel("Area Plot",
                sidebarPanel(
                    width = 3,
                    checkboxInput("xdxplot1","x/dx plot (else x/y)",value = FALSE),
                    splitLayout(
                        checkboxInput("showall1","Show rows",value = FALSE),
                        checkboxInput("top2","Top two",value = FALSE)
                    ),
                    selectInput("label1", "Label type",
                                choices = c("Index","County","CountyID","Area","CNTYVTD"),
                                selected = "Index",
                                multiple = FALSE),
                    textInput("pos1_1", "Position above", value = ""),
                    textInput("pos2_1", "Position right", value = ""),
                    textInput("pos3_1", "Position below", value = ""),
                    textInput("xscale1", "X From,To,Step,Tick", value = ""),
                    textInput("yscale1", "Y From,To,Step,Tick", value = ""),
                    splitLayout(
                        textInput("xlimit","Limit",value = "-5,5"),
                        textInput("xsize1","Size",value = "2"),
                        textInput("xalpha1","Alpha",value = "0.5")
                    ),
                    textInput("areaColor","Color",value = "red3,green3,blue3"),
                    textInput("xparty","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                    checkboxInput("area_x0vote","Exclude 0 votes",value = FALSE),
                    splitLayout(
                        numericInput("areaWidth", "Plot Width", 1000),
                        numericInput("areaHeight", "Plot Height", 800)
                    ),
                    splitLayout(
                        numericInput("plotload", "Load", 1),
                        actionButton("plotsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    plotlyOutput("areaPlot")
                )
            ),
            tabPanel("CVT",
                     sidebarPanel(
                         width = 3,
                         textInput("xscale", "X From,To,Step,Tick", value = ""),
                         textInput("yscale", "Y From,To,Step,Tick", value = ""),
                         textInput("xcolor","Color",value = "red2,blue2,green3,violet,cyan,orange"),
                         textInput("xshape","Shape",value = "3,8,0,1,2,15,16,17"),
                         numericInput("cvt_window","Rolling window",0,min = 0),
                         checkboxInput("cvt_x0vote","Exclude 0 votes",value = FALSE),
                         checkboxInput("votes1000","Votes in 1000s",value = TRUE),
                         checkboxInput("plotbyarea","Plot by Area",value = TRUE),
                         splitLayout(
                             textInput("xpsize_cvt","Point",value = "1"),
                             textInput("xlsize_cvt","Line",value = "1"),
                             textInput("xalpha_cvt","Alpha",value = "0.5")
                         ),
                         textInput("race_indices","Race indices",value = "1"),
                         selectInput("cvt_party", "CVT party",
                                     choices = c("DEM","REP","(both)"),
                                     selected = "REP",
                                     multiple = FALSE),
                         splitLayout(
                             numericInput("plotload", "Load", 1),
                             actionButton("plotsave", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("cvtPlot")
                     )
            ),
            tabPanel("CVTs",
                     sidebarPanel(
                         width = 2,
                         numericInput("cvt_start","Starting index",1,min = 1,step = 9),
                         numericInput("cvt_cols","Number of columns",3,min = 1),
                         numericInput("cvt_rows","Number of rows",3,min = 1)
                         # textInput("xscale", "X From,To,Step,Tick", value = ""),
                         # textInput("yscale", "Y From,To,Step,Tick", value = ""),
                         # textInput("xcolor","Color",value = "blue3,red3,orange,green3,violet"),
                         # textInput("xshape","Shape",value = "3,8,0,1,2,15,16,17"),
                         # numericInput("cvt_window","Rolling window",0,min = 0),
                         # checkboxInput("cvt_x0vote","Exclude 0 votes",value = TRUE),
                         # checkboxInput("votes1000","Votes in 1000s",value = TRUE),
                         # checkboxInput("plotbyarea","Plot by Area",value = TRUE),
                         # splitLayout(
                         #     numericInput("plotload", "Load", 1),
                         #     actionButton("plotsave", "Save")
                         # )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("cvtPlots")
                     )
            ),
            tabPanel("Areas2",
                     sidebarPanel(
                         width = 2,
                         downloadButton("getcsv2","Get CSV"),
                         downloadButton("getexcel2","Get Excel")
                     ),
                     mainPanel(
                         width = 12,
                         verbatimTextOutput("myTextAreas2")
                     )
            ),
            tabPanel("Area Plot2",
                     sidebarPanel(
                         width = 3,
                         checkboxInput("xdxplot2","x/dx plot (else x/y)",value = TRUE),
                         checkboxInput("showall2","Show all labels",value = TRUE),
                         checkboxInput("sizefor2","Size for race 2",value = TRUE),
                         selectInput("label2", "Label type",
                                     choices = c("Index","County","CountyID","Area","CNTYVTD"),
                                     selected = "Index",
                                     multiple = FALSE),
                         textInput("pos1_2", "Position above", value = ""),
                         textInput("pos2_2", "Position right", value = ""),
                         textInput("pos3_2", "Position below", value = ""),
                         textInput("xscale2", "X From,To,Step,Tick", value = ""),
                         textInput("yscale2", "Y From,To,Step,Tick", value = ""),
                         splitLayout(
                             checkboxInput("minmax","Min/Max",value = TRUE),
                             checkboxInput("forcex","X-axis",value = TRUE)
                         ),
                         splitLayout(
                             textInput("xlimit2","Limit",value = "-5,5"),
                             textInput("xalpha2","Alpha",value = "0.5")
                         ),
                         textInput("xcolor2","Color (points)",value = "red3,green3,blue3"),
                         textInput("lcolor2","Color (labels)",value = "red3,green3,blue3"),
                         textInput("ncolor2","Color (lines)",value = "black"),
                         textInput("xparty2","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                         textInput("vrange","Vote Point Range",value = "1,4"),
                         textInput("vtrans","Vote Transform",value = "#log10"),
                         textInput("vbreaks","Vote Breaks", value = ""),
                         textInput("plusnote","Add to title",value = ""),
                         splitLayout(
                             numericInput("plotload2", "Load", 1),
                             actionButton("plotsave2", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("areaPlot2")
                     )
            ),
            tabPanel("Area Plot2s",
                     sidebarPanel(
                         width = 2,
                         numericInput("aplot2_start","Starting index",1,min = 1,step = 9),
                         numericInput("aplot2_cols","Number of columns",3,min = 1),
                         numericInput("aplot2_rows","Number of rows",3,min = 1),
                         textInput("aplot2_counties", "Counties", value =
                                       "#Cameron,Hidalgo,Maverick,Starr,Zapata,Brooks,Jim Hogg,Kenedy,Willacy"),
                         numericInput("aplot2_factor", "Factor (percent)", value = 125)
                     ),
                     mainPanel(
                         width = 9,
                         imageOutput("areaPlot2s")
                     )
            ),
            tabPanel("Area Plot2b",
                     sidebarPanel(
                         width = 3,
                         checkboxInput("showall2b","Show all labels",value = TRUE),
                         selectInput("label2b", "Label type",
                                     choices = c("Index","County","CountyID","Area","CNTYVTD"),
                                     selected = "Index",
                                     multiple = FALSE),
                         textInput("pos1_2b", "Position above", value = ""),
                         textInput("pos2_2b", "Position right", value = ""),
                         textInput("pos3_2b", "Position below", value = ""),
                         textInput("xscale2b", "X From,To,Step,Tick", value = ""),
                         textInput("yscale2b", "Y From,To,Step,Tick", value = ""),
                         splitLayout(
                             textInput("xlimit2b","Limit",value = "-5,5"),
                             textInput("xalpha2b","Alpha",value = "0.5")
                         ),
                         textInput("xcolor2b","Color (points)",value = "red3,green3,blue3"),
                         textInput("lcolor2b","Color (labels)",value = "red3,green3,blue3"),
                         textInput("ncolor2b","Color (lines)",value = "darkgray"),
                         textInput("xparty2b","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                         textInput("vlimitb","Vote Limit (1000s)",value = "0.1,0.5,1,2"),
                         textInput("vshapeb","Vote Shape",value = "1,16,17,15"),
                         textInput("vrange2b","Vote Point Range",value = "1,4"),
                         textInput("vtrans2b","Vote Transform",value = "#log10"),
                         textInput("vbreaks2b","Vote Breaks", value = ""),
                         textInput("plusnoteb","Add to title",value = ""),
                         splitLayout(
                             numericInput("plotload2b", "Load", 1),
                             actionButton("plotsave2b", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("areaPlot2b")
                     )
            ),
            tabPanel("CompareN",
                     sidebarPanel(
                         width = 2,
                         actionButton("checkstate", "Check State",
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         actionButton("checkall", "Check All",
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         downloadButton("getcsvN","Get CSV"),
                         downloadButton("getexcelN","Get Excel"),
                         checkboxInput("bycounty", "By County", value = FALSE),
                         checkboxInput("scalecols", "Scale Columns", value = FALSE),
                         selectInput("partyn", "PartyN",
                                     choices = c("Margin","Total","Dem","Dem+","Rep","Rep+","Other"),
                                     selected = "Margin",
                                     multiple = FALSE),
                         numericInput("xsortcolN", "Sort AreasN", 1),
                         checkboxInput("xsortdescN","Desc",value = TRUE),
                         numericInput("normalizeN", "NormalizeN", 0),
                         textInput("xcolor_n", "Colors (plotN)", "red2,blue2,green2,red3,blue3,green3"),
                         textInput("xshape_n", "Shapes (plotN)", "16,17,15,1,2,0"),
                         textInput("hm_colors", "Colors", "red,green"),
                         textInput("hm_limits", "Limits", "0,0"),
                         textInput("racefmt", "Race Format", "12_10_16"),
                         numericInput("hm_width", "Width", 800),
                         numericInput("hm_height", "Height", 800)
                     ),
                     mainPanel(
                         tabsetPanel(id = "hm_tabs", selected = "PlotN",
                            tabPanel("AreasN",
                                mainPanel(
                                    width = 10,
                                    verbatimTextOutput("myTextAreasN")
                                )
                            ),
                            tabPanel("Heatmap",
                                mainPanel(
                                    width = 10,
                                    imageOutput("heatmap")
                                )
                            ),
                            tabPanel("PlotN",
                                mainPanel(
                                    width = 10,
                                    imageOutput("plotn")
                                )
                            )
                         )
                     )
            ),
            tabPanel("Indicators",
                     sidebarPanel(
                         width = 2,
                         selectInput("indicator2", "Indicator",
                                     choices = c("2BL","C05s","LastC"),
                                     selected = "2BL",
                                     multiple = FALSE),
                         selectInput("indvar2", "Variable(s)",
                                     choices = c("DEM","REP","TOTAL"),
                                     selected = "TOTAL",
                                     multiple = TRUE),
                         numericInput("minprevotes2","Min Votes",10,min = 0),
                         numericInput("minprecints2","Min Precincts",10,min = 0),
                         numericInput("minlimit2","Min Valid",0,min = 0),
                         numericInput("maxlimit2","Max Valid",16.9,min = 0),
                         selectInput("maplimitset2", "Map Limits",
                                     choices = c("Auto set to min,max",
                                                 "Auto set balanced",
                                                 "Use value(s) below"),
                                     selected = "Auto set balanced",
                                     multiple = FALSE),
                         textInput("maplimits2", NULL, value = "-100,100"),
                         textInput("mapcolors2", "Map Colors", value = "RdBu"),
                         numericInput("bigsmall2","Big/Small",0,min = 0),
                         numericInput("decimals2","Decimals",1,min = 0),
                         checkboxInput("inclcounty2","Use County",value = FALSE),
                         checkboxInput("showpcts2","Show Percents",value = FALSE),
                         checkboxInput("showcounts2","Show Counts",value = FALSE)
                     ),
                     mainPanel(
                         tabsetPanel(id = "Table",
                            tabPanel("AreasN",
                                mainPanel(
                                    width = 10,
                                    verbatimTextOutput("myIndicator")
                                )
                            ),
                            tabPanel("Plot",
                                mainPanel(
                                    width = 10,
                                    plotOutput("myIndPlot", width = "800px", height = "500px")
                                )
                            ),
                            tabPanel("Leaflet",
                                mainPanel(
                                    width = 10,
                                    leafletOutput("myLeaflet", height = "800px")
                                )
                            )
                        )
                    )
            ),
            tabPanel("Usage",
                     htmlOutput(outputId = "myUsage")
            )
        ),
        mainPanel(
            
        )
    )
))