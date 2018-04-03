# Required Packages and Functions -----------------------------------------
list.of.packages <- c("shiny", "magrittr", "pracma","ggplot2", "fields", 
                      "biwavelet", "data.table", "stringr", "ape", "DT", 
                      "shinyFiles", "shinyjs", "psych", "ggsignif", "grid", 
                      "dunn.test", "animation")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
library(shiny)
library(magrittr)
library(ggplot2)
library(fields)
library(biwavelet)
library(data.table)
library(stringr)
library(ape)
library(DT)
library(shinyFiles)
library(shinyjs)
library(psych)
library(ggsignif)
library(grid)
library(dunn.test)
library(pracma)
library(animation)
source("functions.R")
###=== end of packages and functions loading ===###
ui <- navbarPage(
  "Wave Analysis",
  ####==== Single Sample ====####
  tabPanel(
    "Single Sample Explore",
    fluidPage(
      useShinyjs(),
      sidebarLayout(
        #### ++Sidebar input and layout####
        sidebarPanel(
          #### ++++01.Data View ####
          conditionalPanel(
            'input.dataset === "Data View"',
            h4("Data Overview"),
            # File Input
            verticalLayout(
              fileInput('S_File.data', 'Upload your wave signal in .csv file:', 
                        accept = c('csv', 'comma-separated-values', '.csv')),
              fileInput('S_File.shape', 'Upload the shape of your samples in .csv file:', 
                        accept = c('csv', 'comma-separated-values', '.csv')),
              fileInput('S_File.loc', 'Upload the location of your samples in .csv file:', 
                        accept = c('csv', 'comma-separated-values', '.csv')),
              splitLayout(
                checkboxInput('S_SelAll', 'Select All IDs', TRUE),
                radioButtons('S_checkGroup', 'Select Group', 
                             c('All Groups' = 0, 'Group 1' = 1, 'Group 2' = 2))
              ),
              selectizeInput("S_SelID", "Select IDs", NULL, multiple = TRUE),
              checkboxInput("S_Mean.centering", "Mean-centering your data", TRUE),
              checkboxInput("S_colname", "Change column name", TRUE),
              textInput("S_FirstCol", "First column name", "Time"),
              textInput("S_SecondCol", "Second column name", "Region"),
              textInput("S_ThirdCol", "Rest column names", "Cell"),
              tags$hr(),
              selectInput("S_unit", label = "Select the frequency unit", 
                          choices = c("kHz", "Hz", "mHz"), 
                          multiple = FALSE),
              textInput("S_ylabel", "Set y title", 
                        "Fouresence Intensity (Gray Value)"),
              textInput("S_xlabe1", "Set x title", "Time (s)"),
              numericInput("S_stimuliTime", 
                           "Set the time point of stimulation", 0),
              withBusyIndicatorUI(
                actionButton("S_analyze", "Analyze", class = "btn-primary")
              ) #_withBusyIndicatorUI()_
            ) #_verticalLayout()_
          ), #_conditionalPanel()_
          #////////////////////////#
          #////end.01 Data View////#
          #////////////////////////#
          #========================#
          #### ++++02.Wave Features ####
          conditionalPanel(
            'input.dataset === "Wave Features"',
            h4("Wave Features"),
            verticalLayout(
              downloadButton('WaveFeatures.csv', 'Download the wave feature results'),
              fluidRow(
                column(6, radioButtons("S_yaxisGrp", "Y-Axis:", c("NULL"="NULL"))),
                column(6, radioButtons("S_xaxisGrp", "x-Axis:", c("NULL"="NULL")))
              ),
              selectizeInput('S_SelID02', 'Select IDs', choices = NULL, multiple = TRUE),
              checkboxInput('S_ResSel', 'Select All IDs', TRUE),
              checkboxGroupInput('S_SelFea', 'Select features', choices = NULL)
            ),
            withMathJax.local(
              helpText("Details of each feature:"),
              helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
              helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
              tags$hr(),
              helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
              tags$hr(),
              helpText("05. Waveform Length (WL): $$WL = \\sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
              tags$hr(),
              helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
              tags$hr(),
              helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
              tags$hr(),
              helpText("08. Mean Power Frequency (MPF): 
                       $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)"),
              tags$hr(),
              helpText(HTML("<p align='justify'> See more details of term 01 to term 08 in Ref.1.")),
              tags$hr(),
              helpText(HTML("09. Index J: 
                            $$J(t) = \\frac{1}{2\\epsilon} \\int_{t-\\epsilon}^{1+\\epsilon} \\sum_{i=1}^{n(\\tau)} |W(\\tau,v_i)|^2 v_i(\\tau)d\\tau $$
                            <p align='justify'> where \\({v_i(\\tau)}\\) is exactly the set of directional local maxima of \\(W\\) along the \\(v\\) axis, at time \\(\\epsilon\\). Since the number
                            of these maxima changes in time, the parameter \\(n\\) is expressed as a function of \\(\\epsilon\\). Integration simply serves to regularize the 
                            index, by avoiding abrupt variations due to discontinuities of frequency paths. See more details in Ref.2.")),
              tags$hr(),
              helpText(HTML("<b>References</b><br />
                            <p align='justify'> 1.Chowdhury RH, Reaz MB, Ali MA, Bakar AA, Chellappan K, Chang TG. 
                            Surface electromyography signal processing and classification techniques. Sensors. 2013 Sep 17;13(9):12431-66.</p>
                            <p align='justify'> 2.Ruffinatti FA, Lovisolo D, Distasi C, Ariano P, Erriquez J, Ferraro M. Calcium signals: 
                            analysis in time and frequency domains. Journal of neuroscience methods. 2011 Aug 15;199(2):310-20.</p>"))
              )
              ), #_conditionalPanel()_
          #///////////////////////#
          #//end.02 Wave Features/#
          #///////////////////////#
          #=======================#
          #### ++++03.Graphic View ####
          conditionalPanel(
            'input.dataset === "Graphic View"',
            h4("Single wave Overview"),
            selectInput("S_SelID03", label = "Select a Single ID", choices = NULL, multiple = FALSE),
            checkboxInput("S_indecator", label = "Show stimuli indicator", value = TRUE),
            tags$hr(),
            withMathJax.local(
              helpText("Details of each feature:"),
              helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
              helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
              tags$hr(),
              helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
              tags$hr(),
              helpText("05. Waveform Length (WL): $$WL = \\sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
              tags$hr(),
              helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
              tags$hr(),
              helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
              tags$hr(),
              helpText("08. Mean Power Frequency (MPF): 
                       $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)"),
              tags$hr(),
              helpText(HTML("<p align='justify'> See more details of term 01 to term 08 in Ref.1.")),
              tags$hr(),
              helpText(HTML("09. Index J: 
                            $$J(t) = \\frac{1}{2\\epsilon} \\int_{t-\\epsilon}^{1+\\epsilon} \\sum_{i=1}^{n(\\tau)} |W(\\tau,v_i)|^2 v_i(\\tau)d\\tau $$
                            <p align='justify'> where \\({v_i(\\tau)}\\) is exactly the set of directional local maxima of \\(W\\) along the \\(v\\) axis, at time \\(\\epsilon\\). Since the number
                            of these maxima changes in time, the parameter \\(n\\) is expressed as a function of \\(\\epsilon\\). Integration simply serves to regularize the 
                            index, by avoiding abrupt variations due to discontinuities of frequency paths. See more details in Ref.2.")),
              tags$hr(),
              helpText(HTML("<b>References</b><br />
                            <p align='justify'> 1.Chowdhury RH, Reaz MB, Ali MA, Bakar AA, Chellappan K, Chang TG. 
                            Surface electromyography signal processing and classification techniques. Sensors. 2013 Sep 17;13(9):12431-66.</p>
                            <p align='justify'> 2.Ruffinatti FA, Lovisolo D, Distasi C, Ariano P, Erriquez J, Ferraro M. Calcium signals: 
                            analysis in time and frequency domains. Journal of neuroscience methods. 2011 Aug 15;199(2):310-20.</p>"))
              )
              ), #_conditionalPanel()_
          #///////////////////////////#
          #////end.03.Graphic View////#
          #///////////////////////////#
          #===========================#
          #### ++++04.Spatial Features ####
          conditionalPanel(
            'input.dataset === "Spatial Features"',
            h4("Spatial Features"),
            verticalLayout(
              checkboxInput("S_labelIDs", "Label", TRUE),
              downloadButton("PseudocolorImages.zip", "Download pseudocolored time sequence images"),
              downloadButton("Video.mp4", "Create a video"),
              uiOutput("S_ui")
            ),
            radioButtons("S_SelFea02","Choose Method", c("NULL"="NULL")),
            withMathJax.local(
              helpText("Details of each feature:"),
              helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
              helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
              tags$hr(),
              helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
              tags$hr(),
              helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
              tags$hr(),
              helpText("05. Waveform Length (WL): $$WL = \\sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
              tags$hr(),
              helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
              tags$hr(),
              helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
              tags$hr(),
              helpText("08. Mean Power Frequency (MPF): 
                       $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)"),
              tags$hr(),
              helpText(HTML("<p align='justify'> See more details of term 01 to term 08 in Ref.1.")),
              tags$hr(),
              helpText(HTML("09. Index J: 
                            $$J(t) = \\frac{1}{2\\epsilon} \\int_{t-\\epsilon}^{1+\\epsilon} \\sum_{i=1}^{n(\\tau)} |W(\\tau,v_i)|^2 v_i(\\tau)d\\tau $$
                            <p align='justify'> where \\({v_i(\\tau)}\\) is exactly the set of directional local maxima of \\(W\\) along the \\(v\\) axis, at time \\(\\epsilon\\). Since the number
                            of these maxima changes in time, the parameter \\(n\\) is expressed as a function of \\(\\epsilon\\). Integration simply serves to regularize the 
                            index, by avoiding abrupt variations due to discontinuities of frequency paths. See more details in Ref.2.")),
              tags$hr(),
              helpText(HTML("<b>References</b><br />
                            <p align='justify'> 1.Chowdhury RH, Reaz MB, Ali MA, Bakar AA, Chellappan K, Chang TG. 
                            Surface electromyography signal processing and classification techniques. Sensors. 2013 Sep 17;13(9):12431-66.</p>
                            <p align='justify'> 2.Ruffinatti FA, Lovisolo D, Distasi C, Ariano P, Erriquez J, Ferraro M. Calcium signals: 
                            analysis in time and frequency domains. Journal of neuroscience methods. 2011 Aug 15;199(2):310-20.</p>"))
              )
              ) #_conditionalPanel()_
          #///////////////////////////#
          #//end.04.Spatial Features//#
          #///////////////////////////#
          #===========================#
          #-----------------------------------#
          #---end. sidebar input and layout---#
          #===================================#
              ), #_sidebarPanel()_
        #### ++The result windows ####
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            #### ++++01.Data View ####
            tabPanel(
              "Data View",
              verticalLayout(
                plotOutput("S_plot01.01"),
                plotOutput("S_plot01.02"),
                plotOutput("S_plot01.03"),
                tags$hr(),
                tableOutput("S_Table01.01")
              )
            ), #_tabPanel()_
            #////////////////////////#
            #////end.01 Data View////#
            #////////////////////////#
            #========================#
            #### ++++02.Wave Features ####
            tabPanel(
              "Wave Features",
              verticalLayout(
                plotOutput("S_plot02.01"),
                tags$hr(),
                splitLayout(
                  plotOutput("S_plot02.02"),
                  plotOutput("S_plot02.03")
                ),
                tags$hr(),
                tags$hr(),
                DT::dataTableOutput('S_table02.01')
              )
            ), #_tabPanel()_
            #////////////////////////#
            #//end.02 Wave Features//#
            #////////////////////////#
            #========================#
            #### ++++03.Graphic View ####
            tabPanel(
              "Graphic View",
              verticalLayout(
                tableOutput("S_table03.01"),
                splitLayout(
                  plotOutput("S_plot03.01"),
                  plotOutput("S_plot03.02")
                ),
                splitLayout(
                  plotOutput("S_plot03.03"),
                  plotOutput("S_plot03.04")
                ),
                plotOutput("S_plot03.05", width = "1500px", height = "2000px")
              )
            ), #_tabPanel()_
            #////////////////////////#
            #//end.03 Graphic View///#
            #////////////////////////#
            #========================#
            #### ++++04.Spatial Features ####
            tabPanel(
              "Spatial Features",
              tableOutput("S_table04.01"),
              splitLayout(
                plotOutput("S_plot04.01"),
                plotOutput("S_plot04.02")
              ),
              tableOutput("S_table04.02")
              
            ) #_tabPanel()_
            #///////////////////////////#
            #//end.04 Spatial Features//#
            #///////////////////////////#
            #===========================#
          ) #_tabsetPanel()_
          #----------------------------#
          #---end.The result Windows---#
          #============================#
        ) #_mainPanel()_
      ) #_sidbarLayout()_
    ) #_fluidPage()_
  ), #_tabPanel()_
  #|||||||||||||||||||||||||||||||#
  #|||||| end.Single Sample ||||||#
  #|||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ####==== Batch Processing ====####
  tabPanel(
    "Batch Processing",
    fluidPage(
      useShinyjs(),
      #### ++Sidebar input and layout ####
      sidebarPanel(
        #### ++++01.Data Input ####
        conditionalPanel(
          'input.dataset2 === "Data Input"',
          h4("Data Input"),
          textInput("B_postfix", "File format", ".csv"),
          splitLayout(
            shinyDirButton('B_directory', 'Folder select', 'Please select a folder'),
            checkboxInput("B_Mean.centering", "Mean-centering your data", TRUE)
          ),
          textInput("B_pat01", "Prefix mark", "TIF"),
          textInput("B_pat02", "Image file number", "00"),
          textInput("B_pat03", "Location file number", "02.Location"),
          textInput("B_FirstCol", "First column Name", "Time (s)"),
          textInput("B_SecondCol", "Second column Name", "Region"),
          textInput("B_ThirdCol", "Rest column Names", "Cell"),
          selectInput("B_Unit", label = "Select the frequency unit", choices = c("kHz", "Hz", "mHz"), multiple = FALSE),
          withBusyIndicatorUI(
            actionButton("B_Ana", "Analyze", class = "btn-primary")
          ),
          tags$hr(),
          verticalLayout(
            shinyDirButton('B_uploadAnaRes', 'Upload a analysis result', 'Please select a folder'),
            downloadButton('AnalysisResults.zip', 'Download the analysis result'),
            helpText(HTML('<p align="justify">Please download those files <b>immediately</b> after hitting the "<b>Analyze</b>" botton</p>'))
          )
        ), #_conditionalPanel()_
        #/////////////////////#
        #//end.01 Data Unput//#
        #/////////////////////#
        #=====================#
        #### ++++02.Statistical Analysis ####
        conditionalPanel(
          'input.dataset2 === "Statistical Analysis"',
          h4("Statistical Analysis"),
          splitLayout(
            numericInput("B_GroupMark", "Select rows and Number", value = 0, min = 0),
            textInput("B_label", "Set Group Name", placeholder = "Type a group label")
          ),
          actionButton("B_Assig", "Assign"),
          actionButton("B_Initi", "Initialize"),
          downloadButton('Group_Set.csv', 'Download Group Setting'),
          tags$hr(),
          fileInput('B_GroupSetInput', 'Upload Group Set', accept=c('csv', '.csv')),
          tags$hr(),
          withBusyIndicatorUI(
            actionButton(
              "B_Sta",
              "Statistical Analysis",
              class = "btn-primary"
            ) #_actionButton()_
          ), #_withBusyIndicatorUI()_
          splitLayout(
            downloadButton('GroupedData.zip', 'Download Grouped Data'),
            checkboxInput("B_Mark", "Add conditaion mark", FALSE)
          ),
          uiOutput("B_ui02")
        ), #_conditionalPanel()_
        #///////////////////////////////#
        #//end.02 Statistical analysis//#
        #///////////////////////////////#
        #===============================#
        #### ++++03.Results ####
        conditionalPanel(
          'input.dataset2 === "Results"',
          h4("Statistical Results"),
          downloadButton('Statistic_Results.zip', 'Download Statistic Results')
        ), #_conditionalPanel()_
        #//////////////////#
        #//end.03.Results//#
        #//////////////////#
        #==================#
        #### ++++04.Plot Output ####
        conditionalPanel(
          'input.dataset2 === "Plot Output"',
          h4("Plot Output"),
          checkboxInput("B_Violin", "Create histogram and mean value", TRUE),
          selectInput("B_level", "Select the view tab", 
                      c("Cell Level", "Region Level", "Moran Index", "Significance of Moran Index", "Comparsion among Groups", "Histogram")
          ),
          uiOutput("B_ui01.side")
        ) #_conditionalPanel()_
        #//////////////////////#
        #//end.04 Plot Output//#
        #//////////////////////#
        #======================#
        #----------------------------------#
        #---end.sidebar input and layout---#
        #==================================#
      ), #_sidebarPanel()_
      #### ++The result windows ####
      mainPanel(
        tabsetPanel(
          id = 'dataset2',
          #### ++++01.Data Input ####
          tabPanel(
            "Data Input",
            verticalLayout(
              DT::dataTableOutput("B_table01.01"),  # B_table01.01, Table 1: Files List
              DT::dataTableOutput("B_table01.02"),  # B_table01.02, Table 2: Wave Features Cell Level
              DT::dataTableOutput("B_table01.03"),  # B_table01.03, Table 3: Wave Features Global Level
              DT::dataTableOutput("B_table01.04"),  # B_table01.04, Table 4: Moran's Index
              DT::dataTableOutput("B_table01.05"),  # B_table01.05, Table 5: Raw Data
              DT::dataTableOutput("B_table01.06")   # B_table01.06, Table 6: Simple IDs
            )
          ), #_tabPanel()_
          #/////////////////////#
          #//end.01 Data Unput//#
          #/////////////////////#
          #=====================#
          #### ++++02.Statistical Analysis ####
          tabPanel(
            "Statistical Analysis",
            verticalLayout(
              DT::dataTableOutput('B_table02.01'),  # B_table02.01, Table 1: Group Setting
              DT::dataTableOutput('B_table02.02'),  # B_table02.02, Table 2 Checking: Wave Features Cell Level
              DT::dataTableOutput('B_table02.03'),  # B_table02.03, Table 3 Checking: Wave Features Global Level
              DT::dataTableOutput('B_table02.04')   # B_table02.04, Table 4 Checking: Moran's Index
            )
          ), #_tabPanel()_
          #///////////////////////////////#
          #//end.02 Statistical analysis//#
          #///////////////////////////////#
          #===============================#
          #### ++++03.Results ####
          tabPanel(
            "Results",
            verticalLayout(
              DT::dataTableOutput('B_table03.01'),  # B_table03.01, Table 1 Summary: Wave Features Cell Level
              DT::dataTableOutput('B_table03.02'),  # B_table03.02, Table 2 Summary: Wave Features Global Level
              DT::dataTableOutput('B_table03.03'),  # B_table03.03, Table 3 Summary: Moran's Index
              DT::dataTableOutput('B_table03.04'),  # B_table03.04, Table 4 Summary: P-value of Moran's Index
              DT::dataTableOutput('B_table03.05')   # B_table03.05, Table 5 Summary: Wave Features Cell Level among groups from cluster analysis
            )
          ), #_tabPanel()_
          #//////////////////#
          #//end.03.Results//#
          #//////////////////#
          #==================#
          #### ++++04.Plot Output ####
          tabPanel(
            "Plot Output",
            uiOutput("B_ui01")
          ) #_tabPanel()_
          #//////////////////////#
          #//end.04 Plot Output//#
          #//////////////////////#
          #======================#
        ) #_tabsetPanel()_
        #----------------------------#
        #---end.The result windows---#
        #============================#
      ) #_mainPanel()_
    ) #_fluidPage()_
  ), #_tabPanel()_
  #||||||||||||||||||||||||||||||||||#
  #|||||| end.Batch Processing ||||||#
  #||||||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ####==== Batch ANOVA processing ====####
  tabPanel(
    "Batch ANOVA processing",
    fluidPage(
      useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          #### 01.Data Input ####
          conditionalPanel(
            'input.dataset3 === "Data Input"',
            h4("Data Input"),
            shinyFilesButton('file', 'File select', 'Please select a file', TRUE),
            textInput("TI00", "Name for Output:", "temp"),
            splitLayout(
              textInput("TI01", "Data in cloumn named:", "Tag"), #TI: Text Input
              textInput("TI02", "Name of the dataset:", "Static")
            ),
            textInput("TI03", "The variables are in the column named:", "Label"),
            splitLayout(
              numericInput("NI01", "The start cloumn:", value = 2, min = 1, max = NA, step = 1), #NI: Number Input
              numericInput("NI02", "The end cloumn:", value = 10, min = 1, max = NA, step = 1)
            ),
            sliderInput("NI03", "Significant level:", value = 0.05, min = 0.001, max = 0.1),
            checkboxInput("Violin01", "Create histogram and mean value", TRUE),#alpha level
            withBusyIndicatorUI(
              actionButton("AnB01", "Analyze", class = "btn-primary") #AnB: Analysis Botton
            )
          ),
          #=== 01.End ===#
          #### 02. Statistical Results ####
          conditionalPanel(
            'input.dataset3 === "Results"',
            h4("Statistical Results"),
            downloadButton('SRes', 'Download Statistical Results'),
            helpText(HTML('<p align="justify"> The False Discover Rate (FDR) is controlled using the Benjamini-Yekutieli adjustment (2001), 
                          a step-down procedure appropriate to depenent tests. p-values are ordered from largest to smallest, 
                          and adjusted p-values = max[1, pmC/(m+1-i)], where i indexes the ordering, and the constant C = 1 + 1/2 + . . . + 1/m. 
                          All tests after and including the first to be rejected <b> at the alpha/2 level are rejected</b> <br />
                          <br />
                          <b>Reference</b><br />
                          Benjamini, Y. and Yekutieli, D. (2001) The control of the false discovery rate in multiple testing under dependency. 
                          Annals of Statistics. 29, 1165–1188.</p>'))
            ),
          #=== 02.End ===#
          #### 03. Plot Output ####
          conditionalPanel(
            'input.dataset3 === "Plot"',
            h4("Plot Output")
          )
          #=== 03.End ===#
            ), #--- sidebarPanel End
        #=== 03.End ===#
        #### Main panel parts ####
        mainPanel(
          #### 01. Data Input ####
          tabsetPanel(
            id = 'dataset3',
            tabPanel("Data Input",
                     DT::dataTableOutput('output01'),
                     verbatimTextOutput('filepaths')
            ),
            #=== 01. End ===#
            #### 02. Statistical Results ####
            tabPanel("Results",
                     DT::dataTableOutput('output02')
            ),
            #=== 02. End ===#
            #### 03. Plot Output ####
            tabPanel("Plot",
                     htmlOutput("pdf", class = "custom-li-output")
            )
            #=== 03. End ===#
          ) #--- tabPanel End
        ) #--- mainPanel End
      ) #--- sidebarLayout End
    ) #_fluidPage()_
  ), #_tabPanel()_
  #||||||||||||||||||||||||||||||||||||||#
  #||||| end.Batch ANOVA processing |||||#
  #||||||||||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ####==== About ====####
  tabPanel(
    "About",
    fluidPage(
      useShinyjs()
    ) #_fluidPage()_
  ) #_tabPanel()_
  #|||||||||||||||||||||#
  #||||| end.About |||||#
  #|||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@#
) #_ui<-navbarPage()_
# Shiny server ####
server <- function(input, output, session) {
  # Global variables ####
  options(shiny.maxRequestSize=100*1024^2)
  #////////////////////////////#
  #////end.Global variables////#
  #////////////////////////////#
  #============================#
  #
  ####==== Single Sample ====####
  #
  observe({
    if(is.null(S_res())) {
      rowna <- NULL
      colna <- NULL
      rowna.All <- NULL
    } else if (input$S_checkGroup == 0) {
      rowna <- rownames(S_res()$Result)
      colna <- colnames(S_res()$Result)
      rowna.All <- rownames(S_res()$Result)
    } else if (input$S_checkGroup == 1) {
      rowna <- rownames(S_res()$Result[S_res()$Result$Group == 1, ])
      colna <- colnames(S_res()$Result)
      rowna.All <- rownames(S_res()$Result)
    } else if (input$S_checkGroup == 2) {
      rowna <- rownames(S_res()$Result[S_res()$Result$Group == 2, ])
      colna <- colnames(S_res()$Result)
      rowna.All <- rownames(S_res()$Result)
    }
    #====01.Data View====#
    if (input$S_SelAll == TRUE) {
      updateSelectizeInput(session, "S_SelID",
                           label = "Select IDs",
                           choices = rowna,
                           selected = rowna)
    } else if (input$S_SelAll == FALSE) {
      updateSelectizeInput(session, "S_SelID",
                           label = "Select IDs",
                           choices = rowna,
                           selected = "")
    }
    #====02.Wave Features====#
    cb_options <- list()
    cb_options[colna] <- colna
    updateRadioButtons(session, "S_xaxisGrp",
                       label = "X-Axis",
                       choices = cb_options,
                       selected = cb_options[1])
    updateRadioButtons(session, "S_yaxisGrp",
                       label = "Y-Axis",
                       choices = cb_options,
                       selected = cb_options[1])
    updateCheckboxGroupInput(session, "S_SelFea",
                             label = "Select features",
                             choices = cb_options,
                             selected = cb_options)
    if (input$S_ResSel == TRUE){
      updateSelectizeInput(session, "S_SelID02",
                           label = "Select your interesting IDs",
                           choices = rowna.All,
                           selected = rowna.All)
    }
    else if(input$S_ResSel == FALSE){
      updateSelectizeInput(session, "S_SelID02",
                           label = "Select your interesting IDs",
                           choices = rowna.All,
                           selected = "")
    }
    #====03.Graphic View====#
    updateSelectizeInput(session, "S_SelID03",
                         label = "select a single ID",
                         choices = rowna.All,
                         selected = rowna.All[1])
    #====04.Spatial Features===#
    updateRadioButtons(session, "S_SelFea02",
                       label = "Select a wave feature",
                       choices = cb_options,
                       selected = cb_options[1])
  })                                         # monitor and update the ui input
  ##### ++01.Data View #####
  S_res <- eventReactive(input$S_analyze, {
    withBusyIndicatorServer("S_analyze", {
      if (is.null(input$S_File.data)) {
        stop("Please submit your data")
        return(NULL)
      } else {
        Datai <- read.csv(input$S_File.data$datapath, header = TRUE, sep = ",")
        if(is.null(input$S_File.loc)) {
          Data.L = NULL
        } else if (is.null(input$S_File.loc) == FALSE) {
          Data.L <- read.csv(input$S_File.loc$datapath, header = TRUE, sep = ",")
        }
        if (input$S_unit == "kHz") {
          Unit <- 10^-3
        } else if (input$S_unit == "Hz") {
          Unit <- 1
        } else if (input$S_unit == "mHz") {
          Unit <- 10^3
        }
        res <- WZY.Wumei(Data = Datai, 
                         Unit = Unit,
                         loc = Data.L,
                         MeCe = input$S_Mean.centering,
                         ChangeColName = input$S_colname,
                         FirstCol.name = input$S_FirstCol,
                         SecondCol.name = input$S_SecondCol,
                         RestCol.name = input$S_ThirdCol,
                         AddPrefix = FALSE )

        if(is.null(input$S_File.loc)) {
          res$Result <- rbind(res$G_Result, res$Result)      # Add first sample back
        } else if (is.null(input$S_File.loc) == FALSE) {
          res$Result <- rbind(res$G_Result[1, ], res$Result) # Add first sample back
        }
        group.J <- res$Result$J_index          %>% 
                   dist()                      %>%
                   hclust(, method = "ward.D") %>%
                   cutree(, k =2)                            # Hierarchical cluster analysis based on index J
        res$Result<-cbind(res$Result, Group = group.J)
        res$G_Result[1, ] <- colnames(res$G_Result)
        rownames(res$G_Result)[1] <- "Features"
        return(res)
      }
    }) #_withBusyIndicatorServer({})_
  })         # S_res, Main results
  S_DataShape <- reactive({
    if (is.null(input$S_File.shape))
      return(NULL)
    else
      Data.S<-read.csv(input$S_File.shape$datapath,
                      header = TRUE,
                      sep = ",")
    return(Data.S)
  })                         # File of shape of all samples
  S_plot01.01 <- reactive({
    if (is.null(S_res()) | is.null(input$S_SelID)) {
      return(NULL)
    } else {
      return(ggplot(data = melt(as.data.frame(S_res()$Data), id.vars = 1, 
                                measure.vars = input$S_SelID)) + 
               geom_line(aes(x=melt(as.data.frame(S_res()$Data), id.vars = 1, 
                                    measure.vars = input$S_SelID)[, 1], 
                             y=melt(as.data.frame(S_res()$Data), id.vars = 1, 
                                    measure.vars = input$S_SelID)[, 3],
                             color=melt(as.data.frame(S_res()$Data), id.vars = 1, 
                                        measure.vars = input$S_SelID)[, 2])) +
               labs(x = input$S_xlabe1, y = input$S_ylabel, colour = "IDs") + 
               ylim(min(S_res()$Data[, -1]), max(S_res()$Data[, -1]))+
               theme(axis.text=element_text(size=14), 
                     axis.title=element_text(size=14,face="bold"))
      )
    }
  })                         # S_plot01.01, All wave overview
  S_plot01.02 <- reactive({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      return(plot(hclust(S_res()$matrix, method = "ward.D"),
                  sub = " ",
                  main = "Wave Cluster",
                  ylab = "Dissimilarity Matrix",
                  xlab = " ",
                  hang = -1)
      )
    }
  })                         # S_plot01.02, Wave clust => hclust tree of the dissimilarity matrix
  S_plot01.03 <- reactive({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      df <- S_res()$Data[, -2]
      res <- S_res()$Result[-1, ]
      G1 <- as.vector(rownames(res[res$Group == 1, ]))
      G2 <- as.vector(rownames(res[res$Group == 2, ]))
      Group1<- rowMeans(df[, G1])
      Group2<- rowMeans(df[, G2])
      SD1 <- SD(t(df)[G1,])/sqrt(length(rownames(res[res$Group == 1, ])))
      SD2 <- SD(t(df)[G2,])/sqrt(length(rownames(res[res$Group == 2, ])))
      SD <- c(SD1, SD2)
      Time <- df[, 1]
      data03 <- data.frame(Time = Time, Group1 = Group1, Group2 = Group2)
      mdf03 <- melt(data03, id.vars = "Time", measure.vars = colnames(data03[,-1]))
      gp03<-ggplot(data = mdf03, 
                   aes_string(x="Time", y = "value", color = "variable")) + 
        geom_line() + 
        geom_errorbar(aes(ymin=mdf03$value - SD, ymax=mdf03$value + SD), 
                      position = position_dodge(0.9)) +
        ggtitle("Average curve with SEM error bar")
      return(gp03)
    }
  })                         # S_plot01.02, average curve of group1 and group2
  output$S_plot01.01 <- renderPlot(S_plot01.01())        # S_plot01.01 output
  output$S_plot01.02 <- renderPlot(S_plot01.02())        # S_plot01.02 output
  output$S_plot01.03 <- renderPlot(S_plot01.03())        # S_plot01.02 output
  output$S_Table01.01 <- renderTable({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      rownames = TRUE
      digits = 3
      as.data.frame(S_res()$Data)[, c(colnames(S_res()$Data)[1], input$S_SelID)]
    }
  })              # S_Table01.01 output
  #////////////////////////#
  #////end.01 Data View////#
  #////////////////////////#
  #========================#
  #
  #### ++02.Wave Features ####
  S_plot02.01 <- reactive({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      return(ggplot(data = melt(as.data.frame(S_res()$Result), 
                                id.vars = input$S_yaxisGrp,
                                measure.vars = input$S_xaxisGrp)) + 
               geom_point(aes(x=melt(as.data.frame(S_res()$Result), 
                                     id.vars = input$S_yaxisGrp, 
                                    measure.vars = input$S_xaxisGrp)[, 1], 
                             y=melt(as.data.frame(S_res()$Result), 
                                    id.vars = input$S_yaxisGrp, 
                                    measure.vars = input$S_xaxisGrp)[, 3],
                             color=melt(as.data.frame(S_res()$Result), 
                                        id.vars = input$S_yaxisGrp, 
                                        measure.vars = input$S_xaxisGrp)[, 2])) +
               labs(x = input$S_xaxisGrp, y = input$S_yaxisGrp) + 
               guides(colour=FALSE) +
               theme(axis.text=element_text(size=14), 
                     axis.title=element_text(size=14,face="bold"))
      )
    }
  })                         # S_plot02.01, scatter plot
  S_plot02.02 <- reactive({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      return(ggplot(data = as.data.frame(S_res()$Result), 
                    aes_string(input$S_xaxisGrp)) + 
               geom_histogram() +
               labs(title = "Histogram of X variable") + guides(colour=FALSE) +
               theme(axis.text=element_text(size=14), 
                     axis.title=element_text(size=14,face="bold"))
      )
    }
  })                         # S_plot02.02, x variable histogram
  S_plot02.03 <- reactive({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      return(ggplot(data = as.data.frame(S_res()$Result), 
                    aes_string(input$S_yaxisGrp)) + 
               geom_histogram() +
               labs(title = "Histogram of y variable") + guides(colour=FALSE) +
               theme(axis.text=element_text(size=14), 
                     axis.title=element_text(size=14,face="bold"))
      )
    }
  })                         # S_plot02.03, y variable histogram
  output$S_table02.01 <- DT::renderDataTable({
    if (is.null(S_res())) {
      return(NULL)
    } else {
      DT::datatable(as.data.frame(S_res()$Result[input$S_SelID02, 
                                                 input$S_SelFea])) %>%
      formatRound(columns=c(input$S_SelFea), digits=3)
    }
  })      # S_table2.01 output
  output$S_plot02.01 <- renderPlot(S_plot02.01())        # S_plot02.01 output
  output$S_plot02.02 <- renderPlot(S_plot02.02())        # S_plot02.02 output
  output$S_plot02.03 <- renderPlot(S_plot02.03())        # S_plot02.03 output
  output$WaveFeatures.csv <- downloadHandler(
    filename = function() {
      paste("WaveFeatures", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(S_res()$Result, file, sep = ",")
    }
  )                                                      # S_downloadFeatures 
  #//////////////////////////#
  #///end.02 Wave Features///#
  #//////////////////////////#
  #==========================#
  #
  ##### ++03.Graphic View #####
  S_waveletTransform <- reactive({
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(
        wt(cbind(S_res()$Data[, 1], S_res()$Data[, input$S_SelID03]), do.sig = FALSE)
      )
    }
  })                  # Interesting ID processing
  S_timeSeriesGraph <- reactive({
    if(is.null(S_res())) {
      return(NULL)
    } else if(input$S_indecator == TRUE) {
      return(
        ggplot(as.data.frame(S_res()$Data))+
          geom_line(aes(x = S_res()$Data[, 1], 
                        y = S_res()$Data[, input$S_SelID03])) +
          labs(x = input$S_xlabe1, y = input$S_ylabel, title = input$S_SelID03) +
          ylim(min(S_res()$Data[, -1]), max(S_res()$Data[, -1])) +
          geom_vline(xintercept = input$S_stimuliTime, color = "Red") +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
                axis.text=element_text(size=14), 
                axis.title=element_text(size=14,face="bold"))
      )
    } else if(input$S_indecator == FALSE) {
      return(
        ggplot(as.data.frame(S_res()$Data))+
          geom_line(aes(x = S_res()$Data[, 1], y = S_res()$Data[, input$S_SelID03])) +
          labs(x = input$S_xlabe1, y = input$S_ylabel, title = input$S_SelID03) +
          ylim(min(S_res()$Data[, -1]), max(S_res()$Data[, -1])) +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
                axis.text=element_text(size=14), 
                axis.title=element_text(size=14,face="bold"))
      )
    } 
  })                   # Time Series plot
  S_waveletSpectrum <- reactive({
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(
        plot(S_waveletTransform(), type = "power.corr.norm", 
             main = "Wavelet Spectrum")
      )
    }
  })                   # Wavelet spectrum plot
  S_waveletVariance <- reactive({
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(
        plot(cbind(S_waveletTransform()$period,
                   rowSums(abs(S_waveletTransform()$wave)^2)), type = "l",
             main = "Wavelet Variance", xlab = input$S_xlabe1,
             ylab = "Variance")
      )
    }
  })                   # Wavelet variance plot 
  S_powSpeclDensity <- reactive({
    if(is.null(S_res())) {
      return(NULL)
    } else {
      if (input$S_unit == "kHz") {
        Unit <- 10^-3
      } else if (input$S_unit == "Hz") {
        Unit <- 1
      } else if (input$S_unit == "mHz") {
        Unit <- 10^3
      }
      fftin <- fft(as.data.frame(S_res()$Data)[, input$S_SelID03])
      return(
        wzy.plot.MPF(fftin, sampleSize = length(fftin),
                     timeStep = S_res()$Data[3, 1]-S_res()$Data[2, 1],
                     Unit = Unit, UnitLabel = input$S_unit)
      )
    }
  })                   # Power spectral plot (Fast Fourier Transfrom)
  S_index_Janalysis <- reactive({
    if (input$S_unit == "kHz") {
      Unit <- 10^-3
    } else if (input$S_unit == "Hz") {
      Unit <- 1
    } else if (input$S_unit == "mHz") {
      Unit <- 10^3
    }
    wt.t1<-S_waveletTransform()
    sampling<-NCOL(wt.t1$wave)
    W<-wt.t1$wave
    a<-wt.t1$scale
    v<-5/(2*pi*a)
    v<-v*Unit                                                   # unit factor Hz mHz or MHz
    PSP<-rowSums(abs(W)^2)                                      # Power Spectrum P
    PSP.c<-data.frame(Freq = log2(rev(v)), psp = rev(PSP))
    PSP.gp<-ggplot(PSP.c, aes(x = Freq, y = psp))+geom_line()+
      labs(x = paste("Frequency (", input$S_unit, ")", sep = ""), 
           y = "Power Spectrum P")+
      coord_flip()+scale_y_reverse()+
      scale_x_continuous(breaks = seq(from = min(PSP.c$Freq), to = max(PSP.c$Freq), 
                                      by = max(abs(PSP.c$Freq))*0.10),
                         labels = round(2^(seq(from = min(PSP.c$Freq), 
                                               to = max(PSP.c$Freq), 
                                               by = max(abs(PSP.c$Freq))*0.10)), 
                                        abs(1/mean(PSP.c$Freq))))+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold"), legend.position="none")
    EDE<-colSums(abs(W)^2)                                      # Energy Density E
    EDE.c<-data.frame(Time = wt.t1$xaxis, ede = EDE)
    EDE.gp<-ggplot(EDE.c, aes(x = Time, y = ede))+geom_line()+
      labs(x = input$S_xlabe1, y = "Energy Density E")+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold"), legend.position="none")
    ### indexj
    indexj<-c()
    for(i in 1:sampling) {
      Wpeaks<-findpeaks(abs(W[,i]))[,1]
      Wpeaks<-Wpeaks^2
      vposition<-v[findpeaks(abs(W[,i]))[,2]]
      temp<-sum(Wpeaks*vposition)/2
      indexj<-c(indexj, temp)
    }
    indexj<-movavg(indexj, n = round(sampling*0.05), type = "s") # moveing average
    indexjcurve<-data.frame(Time = wt.t1$xaxis, J = indexj)
    ### indexj.gp
    indexj.gp<-ggplot(indexjcurve, aes(x = Time, y = J))+geom_line()+
      labs(x = input$S_xlabe1, y = "J (activity index)")+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold"))
    ###
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", 
                                     "cyan", "#7FFF7F", "yellow", 
                                     "#FF7F00", "red", "#7F0000"))
    tempmap<-apply(abs(wt.t1$wave), 2, rev)
    rownames(tempmap)<-rev(v)
    colormap <- melt(tempmap)
    colormap[, 1]<-log2(colormap[, 1])
    ### colormap.gp
    colormap.gp <- ggplot(colormap, aes(x=colormap[,2], y=colormap[,1], 
                                        fill = colormap[,3]))+geom_tile()+
      scale_fill_gradientn(colours = jet.colors(7))+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold"), legend.position="none")+
      labs(x = input$S_xlabe1, 
           y = paste("Frequency (", input$S_unit, ")", sep = "")) + 
      scale_y_continuous(breaks = seq(from = min(colormap[,1]), 
                                      to = max(colormap[,1]), 
                                      by = max(abs(colormap[,1]))*0.10),
                         labels = c(round(2^(seq(from = min(colormap[,1]), 
                                                 to = max(colormap[,1]), 
                                                 by = max(abs(colormap[,1]))*0.10)), 
                                          abs(1/mean(colormap[,1])))))+
      scale_x_continuous(breaks=c(1:floor(length(wt.t1$xaxis)/10))*10,
                         labels=wt.t1$xaxis[c(1:floor(length(wt.t1$xaxis)/10))*10])
    ###
    if(input$S_indecator == TRUE) {
      grid.newpage()
      pushViewport(
        viewport(
          layout = grid.layout(
            5, 2, width = unit(c(33, 66), 
                               c("picas", "picas")), 
            heights = unit(c(22, 22, 22, 22, 66), 
                           c("picas", "picas", "picas", "picas", "picas")))))
      print(S_timeSeriesGraph()+
              geom_vline(xintercept = input$S_stimuliTime, color = "Red")+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 1,layout.pos.col = 2))
      print(colormap.gp+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 2,layout.pos.col = 2))
      print(indexj.gp+geom_vline(xintercept = input$S_stimuliTime, color = "Red")+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 3,layout.pos.col = 2))
      print(EDE.gp+geom_vline(xintercept = input$S_stimuliTime, color = "Red")+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 4, layout.pos.col = 2))
      print(colormap.gp+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 5,layout.pos.col = 2))
      print(PSP.gp+
              theme(plot.margin = margin(12, 12, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
    } else if(input$S_indecator == FALSE) {
      grid.newpage()
      pushViewport(
        viewport(
          layout = grid.layout(
            5, 2, width = unit(c(33, 66), 
                               c("picas", "picas")),
            heights = unit(c(22, 22, 22, 22, 66), 
                           c("picas", "picas", "picas", "picas", "picas")))))
      print(S_timeSeriesGraph() +
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 1,layout.pos.col = 2))
      print(colormap.gp+
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 2,layout.pos.col = 2))
      print(indexj.gp +
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 3,layout.pos.col = 2))
      print(EDE.gp +
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 4, layout.pos.col = 2))
      print(colormap.gp +
              theme(plot.margin = margin(12, 0, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 5,layout.pos.col = 2))
      print(PSP.gp +
              theme(plot.margin = margin(12, 12, 12, 0, unit = "pt")), 
            vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
    }

  })                   # Illustration of Index J calculation
  output$S_table03.01 <- renderTable({
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(
        as.data.frame(S_res()$Result)[input$S_SelID03, ]
      )
    }
  })              # S_table03.01 (wave features of interesting ID) output
  output$S_plot03.01 <- renderPlot(S_timeSeriesGraph())  # S_plot03.01 output
  output$S_plot03.02 <- renderPlot(S_waveletSpectrum())  # S_plot03.02 output
  output$S_plot03.03 <- renderPlot(S_powSpeclDensity())  # S_plot03.03 output
  output$S_plot03.04 <- renderPlot(S_waveletVariance())  # S_plot03.04 output
  output$S_plot03.05 <- renderPlot(S_index_Janalysis())  # S_plot03.05 output
  #///////////////////////////#
  #////end.03.Graphic View////#
  #///////////////////////////#
  #===========================#
  #
  #### ++04.Spatial Features ####
  S_plot04.01 <- reactive({# Pseudocolor plot
    if(is.null(S_res()) | is.null(S_res()$Data.L)) {
      return(NULL)
    } else {
      jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                       "#7FFF7F", "yellow", "#FF7F00", "red", 
                                       "#7F0000"))
      res <- S_res()$Result
      spa <- S_DataShape()
      loc <- S_res()$Data.L
      colnames(loc)<-c("id", "x", "y")
      colnames(spa)<-c("id", "x", "y")
      col.res<-colnames(res)
      col.spa<-colnames(spa)
      final<-c()
      aribitrary<-c()
      ln<-length(res[, 1])
      for(i in 1:ln){
        times <- 0
        times <- length(unlist(spa[spa$id == i-1, "x"]))
        aribitrary <- rep(res[i, input$S_SelFea02], times)
        final <- c(final, aribitrary)
      }
      n<-cbind(spa, final)
      colnames(n)<-c("id", "x", "y", input$S_SelFea02)
      n<-as.data.frame(n)
      if(input$S_labelIDs == TRUE){
        gp <- ggplot(n, aes(x=x, y=y))+
          geom_polygon(
            aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
            colour = "black")+
          geom_text(data = loc, aes(x = x, y = y, label = id), 
                    alpha = 1, color = "black")+scale_y_reverse()
      } else if(input$S_labelIDs == FALSE) {
        gp <- ggplot(n, aes(x=x, y=y))+
          geom_polygon(
            aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
            colour = "black")+
          scale_y_reverse()
      }
      gp<- gp + coord_fixed(ratio = 1, xlim = c(0, max(spa[, 2])), 
                            ylim =c(0, max(spa[, 2]))) + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.text = element_blank(), axis.line = element_blank(), 
              axis.title = element_blank(), axis.ticks = element_blank(),
              panel.background = element_blank()) + 
        scale_fill_gradientn(colours = jet.colors(7))
      return(gp);
    }
  })                         # Pseudocolor plot
  S_plot04.02 <- reactive({# Pseudocolor plot without region ROI
    if(is.null(S_res())| is.null(S_res()$Data.L)) {
      return(NULL)
    } else {
      jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                       "#7FFF7F", "yellow", "#FF7F00", "red", 
                                       "#7F0000"))
      res <- S_res()$Result[-1, ]
      spa <- S_DataShape()
      spa <- spa[spa$id > 0, ]
      loc <- S_res()$Data.L[-1, ]
      colnames(loc)<-c("id", "x", "y")
      colnames(spa)<-c("id", "x", "y")
      col.res<-colnames(res)
      col.spa<-colnames(spa)
      final<-c()
      aribitrary<-c()
      ln<-length(res[, 1])
      for(i in 1:ln){
        times <- 0
        times <- length(unlist(spa[spa$id == i, "x"]))
        aribitrary <- rep(res[i, input$S_SelFea02], times)
        final <- c(final, aribitrary)
      }
      n<-cbind(spa, final)
      colnames(n)<-c("id", "x", "y", input$S_SelFea02)
      n<-as.data.frame(n)
      if(input$S_labelIDs == TRUE){
        gp <- ggplot(n, aes(x=x, y=y))+
          geom_polygon(
            aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
            colour = "black")+
          geom_text(data = loc, aes(x = x, y = y, label = id), 
                    alpha = 1, color = "black")+scale_y_reverse()
      } else if(input$S_labelIDs == FALSE) {
        gp <- ggplot(n, aes(x=x, y=y))+
          geom_polygon(
            aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
            colour = "black")+
          scale_y_reverse()
      }
      gp<- gp + coord_fixed(ratio = 1, xlim = c(0, max(S_DataShape()[, 2])), 
                            ylim =c(0, max(S_DataShape()[, 2]))) + 
        geom_polygon(data = S_DataShape()[S_DataShape()$id == 0, ], 
                     aes_string(x="x", y="y", group = "id"), 
                     colour = "black", fill = NA) +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.text = element_blank(), axis.line = element_blank(), 
              axis.title = element_blank(), axis.ticks = element_blank(),
              panel.background = element_blank()) + 
        scale_fill_gradientn(colours = jet.colors(7))
      return(gp);
    }
  })                         # Pseudocolor plot without region ROI
  output$S_ui <- renderUI({# Update the UI condition
    if(is.null(S_res())) {
      sliderInput("S_FPS", label = "FPS: number of images in 1s, 
                  when creating the video", min = 1, 
                  max = 24, step = 1, value = 24)
    } else {
      sliderInput("S_FPS", label = "FPS: number of images in 1s, 
                  when creating the video", min = 1, value = 24,
                  max = length(S_res()$Data[, 1]), step = 1)
    }
  })                         # Update the UI condition
  output$S_table04.01 <- renderTable({# S_table04.01 Spearman's Rho result
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(as.data.frame(S_res()$S_Result))
    }
  })              # S_table04.01 Spearman's Rho result
  output$S_plot04.01 <- renderPlot(S_plot04.01())        # S_plot04.01 output
  output$S_plot04.02 <- renderPlot(S_plot04.02())        # S_plot04.02 output
  output$S_table04.02 <- renderTable({# S_table04.02 Moran's I output
    if(is.null(S_res())) {
      return(NULL)
    } else {
      return(as.data.frame(t(S_res()$G_Result)))
    }
  })              # S_table04.02 Moran's I output
  output$PseudocolorImages.zip <- downloadHandler(
    filename = function() {
      paste("PseudocolorImages", "zip", sep = ".")
    },
    content = function(fname) { # Make Pseudocolor images seriers
      tmpdir <- tempdir()
      setwd(tempdir())
      withProgress(message = 'Saving Images',
                   detail = 'This may take a while...', value = 0, {
                     plots <- list()
                     jet.colors <- colorRampPalette(
                       c("#00007F", "blue", "#007FFF", "cyan", 
                         "#7FFF7F", "yellow", "#FF7F00", "red","#7F0000"))
                     res <- S_res()$Data[, -c(1, 2)] # Remove time column and the data from region
                     spa <- S_DataShape()
                     spa <- spa[spa$id > 0, ]
                     loc <- S_res()$Data.L[-1, ]
                     colnames(loc)<-c("id", "x", "y")
                     colnames(spa)<-c("id", "x", "y")
                     col.res<-colnames(res)
                     col.spa<-colnames(spa)
                     ln<-length(res[1, ])
                     lnT <- length(unlist(res[, 1]))
                     for(j in 1:lnT) {
                       final <- c()
                       aribitrary <- c()
                       n <- NULL
                       for(i in 1:ln){
                         times <- 0
                         times <- length(unlist(spa[spa$id == i, "x"]))
                         aribitrary <- rep(res[j, i], times)
                         final <- c(final, aribitrary)
                       }
                       n<-cbind(spa, final)
                       colnames(n)<-c("id", "x", "y", input$S_SelFea02)
                       n<-as.data.frame(n)
                       if(input$S_labelIDs == TRUE){
                         gp <- ggplot(n, aes(x=x, y=y))+
                           geom_polygon(
                             aes_string(x="x", y="y", group = "id", 
                                        fill = input$S_SelFea02), colour = "black")+
                           geom_text(data = loc, aes(x = x, y = y, label = id), 
                                     alpha = 1, color = "black")+scale_y_reverse()
                       } else if(input$S_labelIDs == FALSE) {
                         gp <- ggplot(n, aes(x=x, y=y))+
                           geom_polygon(
                             aes_string(x="x", y="y", group = "id", 
                                        fill = input$S_SelFea02), colour = "black")+
                           scale_y_reverse()
                       }
                       gp<- gp + coord_fixed(ratio = 1, 
                                             xlim = c(0, max(S_DataShape()[, 2])), 
                                             ylim =c(0, max(S_DataShape()[, 2]))) + 
                         geom_polygon(data = S_DataShape()[S_DataShape()$id == 0, ],
                                      aes_string(x="x", y="y", group = "id"), 
                                      colour = "black", fill = NA) +
                         theme(plot.background = element_blank(), 
                               panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               axis.text = element_blank(), 
                               axis.line = element_blank(), 
                               axis.title = element_blank(), 
                               axis.ticks = element_blank(),
                               panel.background = element_blank()) + 
                         scale_fill_gradientn(colours = jet.colors(7), 
                                              limits = c(min(res), max(res)))
                       plots[[j]] <- gp
                       incProgress(1/((lnT+1)*2))
                     }
                     fs <- c()
                     for(i in 1:lnT){
                       tiff(paste("image", i, ".tif", sep = ""), 
                            compression = "none", width = 600, height = 600, 
                            units = "px", pointsize = 12)
                       plot(plots[[i]])
                       incProgress(1/((lnT+1)*2))
                       dev.off()
                       fs_t <- paste(tmpdir, "/", "image", i, ".tif", sep = "")
                       fs <- c(fs, fs_t)
                     }
                     zip(zipfile=fname, files = fs, flags = "-j")
                     incProgress(1/((lnT+1)*2))
                   })
      file.rename('PseudocolorImages.zip', fname)
    }
  )                                                      # Download Pseudocolor images
  output$Video.mp4 <- downloadHandler(
    filename = function() {
      paste("Video", "mp4", sep = ".")
    },
    content = function(file) {
      withProgress(message = 'Making Video',
                   detail = 'This may take a while...', value = 0, {
        plots <- list()
        jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                         "#7FFF7F", "yellow", "#FF7F00", "red", 
                                         "#7F0000"))
        res <- S_res()$Data[, -c(1, 2)] # Remove time column and the data from region
        spa <- S_DataShape()
        spa <- spa[spa$id > 0, ]
        loc <- S_res()$Data.L[-1, ]
        colnames(loc)<-c("id", "x", "y")
        colnames(spa)<-c("id", "x", "y")
        col.res<-colnames(res)
        col.spa<-colnames(spa)
        ln<-length(res[1, ])
        lnT <- length(unlist(res[, 1]))
        for(j in 1:lnT) {
          final <- c()
          aribitrary <- c()
          n <- NULL
          for(i in 1:ln){
            times <- 0
            times <- length(unlist(spa[spa$id == i, "x"]))
            aribitrary <- rep(res[j, i], times)
            final <- c(final, aribitrary)
          }
          n<-cbind(spa, final)
          colnames(n)<-c("id", "x", "y", input$S_SelFea02)
          n<-as.data.frame(n)
          if(input$S_labelIDs == TRUE){
            gp <- ggplot(n, aes(x=x, y=y))+
              geom_polygon(
                aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
                colour = "black")+
              geom_text(
                data = loc, aes(x = x, y = y, label = id), 
                alpha = 1, color = "black")+scale_y_reverse()
          } else if(input$S_labelIDs == FALSE) {
            gp <- ggplot(n, aes(x=x, y=y))+
              geom_polygon(
                aes_string(x="x", y="y", group = "id", fill = input$S_SelFea02), 
                colour = "black")+
              scale_y_reverse()
          }
          gp<- gp + coord_fixed(ratio = 1, xlim = c(0, max(S_DataShape()[, 2])), 
                                ylim =c(0, max(S_DataShape()[, 2]))) + 
            geom_polygon(data = S_DataShape()[S_DataShape()$id == 0, ], 
                         aes_string(x="x", y="y", group = "id"), 
                         colour = "black", fill = NA) +
            theme(plot.background = element_blank(), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.text = element_blank(), 
                  axis.line = element_blank(), axis.title = element_blank(), 
                  axis.ticks = element_blank(),panel.background = element_blank())+ 
            scale_fill_gradientn(colours = jet.colors(7), 
                                 limits = c(min(res), max(res)))
          plots[[j]] <- gp
          incProgress(1/(lnT+1))
        }
        saveVideo(
          for (k in 1:lnT) {
            print(plots[[k]])
            ani.options(interval = 1/input$S_FPS, autobrowse = FALSE)
          },
          ffmpeg = ani.options("ffmpeg"),
          video.name = "Video.mp4"
        )
        file.rename('Video.mp4', file)
        incProgress(1/(lnT+1))
      })
    }
  )                                                      # Create a video
  #/////////////////////////////#
  #///end.04.Spatial Features///#
  #/////////////////////////////#
  #=============================#
  #
  #||||||||||||||||||||||||||||||#
  #|||||| end.Single Sample |||||#
  #||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ####==== Batch Processing ====####
  #
  volumes <- c('Root'=file.path(getwd(), "/Data"))
  shinyDirChoose(input, 'B_directory', roots=volumes, session = session)
  shinyDirChoose(input, 'B_uploadAnaRes', roots=volumes, session = session)
  values<-reactiveValues()
  values$colnames<-NULL
  values$sampleSize <- NULL
  values$Cells <- NULL
  values$Region <- NULL
  values$B_table01.01 <- NULL
  values$B_table01.02 <- NULL
  values$B_table01.03 <- NULL
  values$B_table01.04 <- NULL
  values$B_table01.05 <- NULL
  values$B_table01.06 <- NULL
  values$B_table02.01 <- NULL
  values$theme_ed <- theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "grey75"),
    axis.ticks = element_line(color = "grey85"),
    panel.grid.major = element_line(color = "grey95", size = 0.2),
    panel.grid.minor = element_line(color = "grey95", size = 0.2),
    legend.key = element_blank())
  #### ++01.Data Input ####
  B_res <- eventReactive(input$B_Ana, {
    withBusyIndicatorServer("B_Ana", {
      if(is.null(values$B_table01.01)) {
        stop("Please select a directory")
        return(NULL)
      } else {
        progress <- Progress$new(min = 0, max = 1)
        progress$set(message = "Batching Processing Ready")
        if(input$B_Unit == "kHz") {
          Unit <- 10^-3
        } else if (input$B_Unit == "Hz") {
          Unit <- 1
        } else if (input$B_Unit == "mHz") {
          Unit <- 10^3
        }
        fl <- values$B_table01.01
        Dir <- parseDirPath(volumes, input$B_directory)
        pat01 <- input$B_pat01
        pat02 <- input$B_pat02
        pat03 <- input$B_pat03
        fl <- fl[unlist(grep(pattern = pat01, fl))]
        prefix_position <- unlist(gregexpr(pattern = pat01, fl))   # gregexpr(), search for matches to argument pattern within each element of a character vector
        prefix <- substr(fl, start = 1, stop = prefix_position-1)  # Extract substrings in a character vector.
        size <- length(unique(prefix))
        progress$set(value = 1/size, 
                     message = paste("Batching Processing", "(1/", size, ")", 
                                     sep = ""))
        res <- list()
        res$Results <- data.frame()
        res$GResult <- data.frame()
        res$Spatial <- data.frame()
        ids <- c()
        id <- c()
        InUseName <- paste(prefix[1], pat01, pat02, ".csv", sep = "")
        InUseName02 <- paste(prefix[1], pat01, pat03, ".csv", sep = "")
        file01 <- read.csv(paste(Dir, "/", InUseName, sep = ""), 
                           header = TRUE, sep = ",")
        ncol <- NCOL(file01)
        label <- c()
        for (i in 1:ncol) {
          if(i == 1) {
            label[1] <- input$B_FirstCol
          } else if(i == 2) {
            label[i] <- str_c("S", prefix[1], input$B_SecondCol)
          } else {
            label[i] <- str_c("S", prefix[1], input$B_ThirdCol, " ", i-2)
          } 
        } # Change the columan names
        colnames(file01) <- label
        file02 <- read.csv(paste(Dir, "/", InUseName02, sep = ""), 
                           header = TRUE, sep = ",")
        rawBatchData <- file01
        rawBatchData.cells <- file01[, -2]
        rawBatchData.region <- file01[, 1:2]
        res00 <- WZY.Wumei(Data = file01, loc = file02, Unit = Unit, 
                           MeCe = input$B_Mean.centering, AddPrefix = FALSE,
                           ChangeColName = FALSE, ProRep = FALSE)
        rownames(res00$Result) <- label[-c(1, 2)]
        rownames(res00$G_Result) <- c(label[2], str_c("S", prefix[1], "Moran Index"),
                                      str_c("S", prefix[1], "P-value of Moran Index"))
        ids <- c(ids, prefix[1])
        #
        res$Results <- rbind(res$Result, res00$Result)
        res$Results <- cbind(res$Result, id = rep(prefix[1], length(rownames(res$Results))))
        res$Spatial <- rbind(res$Spatial, res00$G_Result[c(2, 3), ])
        res$Spatial <- cbind(res$Spatial, id = rep(prefix[1], 2))
        res00$G_Result <- cbind(res00$G_Result[-c(2,3), ], res00$S_Result)
        res$GResult <- rbind(res$GResult, res00$G_Result[1, ])
        res$GResult <- cbind(res$GResult, id = prefix[1])
        prefix <- prefix[! prefix %in% prefix[1]]
        while(length(prefix) > 1) {
          progress$inc(amount = 1/size, 
                       message = paste("Batching Processing", "(", 
                                       size - length(unique(prefix)), "/", size, ")", 
                                       sep = ""))
          id <- c()
          InUseName <- paste(prefix[1], pat01, pat02, ".csv", sep = "")
          InUseName02 <- paste(prefix[1], pat01, pat03, ".csv", sep = "")
          file01 <- read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, 
                             sep = ",")
          ncol <- NCOL(file01)
          label <- c()
          for (i in 1:ncol) {
            if(i == 1) {
              label[1] <- input$B_FirstCol
            } else if(i == 2) {
              label[i] <- str_c("S", prefix[1], input$B_SecondCol)
            } else {
              label[i] <- str_c("S", prefix[1], input$B_ThirdCol, " ", i-2)
            } 
          } # Change the columan names
          colnames(file01) <- label
          file02 <- read.csv(paste(Dir, "/", InUseName02, sep = ""), 
                             header = TRUE, sep = ",")
          rawBatchData <- cbind(rawBatchData, file01[, -1])
          rawBatchData.cells <- cbind(rawBatchData.cells, file01[, -2])
          rawBatchData.region <- cbind(rawBatchData.region, file01[, 2])
          res00 <- WZY.Wumei(Data = file01, loc = file02, Unit = Unit, 
                             MeCe = input$B_Mean.centering, AddPrefix = FALSE,
                             ChangeColName = FALSE, ProRep = FALSE)
          rownames(res00$Result) <- label[-c(1, 2)]
          rownames(res00$G_Result) <- c(label[2], str_c("S", prefix[1], "Moran Index"),
                                        str_c("S", prefix[1], "P-value of Moran Index"))
          ids <- c(ids, prefix[1])
          res00$Result <- cbind(res00$Result, id = rep(prefix[1], length(rownames(res00$Result))))
          res00$Spatial <- cbind(res00$Spatial, id = rep(prefix[1], 2))
          res00$G_Result <- cbind(res00$G_Result, id =  rep(prefix[1], 3))
          res$Results <- rbind(res$Result, res00$Result)
          res$Spatial <- rbind(res$Spatial, res00$G_Result[c(2, 3), ])
          res00$G_Result <- cbind(res00$G_Result[-c(2,3), ], res00$S_Result)
          res$GResult <- rbind(res$GResult, res00$G_Result[1, ])
          prefix <- prefix[! prefix %in% prefix[1]]
        }
        isolate(values$sampleSize <- as.numeric(size))
        isolate(values$cells <- rawBatchData.cells)
        colnames(rawBatchData.region)[2:(length(ids)+1)]<-ids
        isolate(values$region <- rawBatchData.region)
        res$rawBatchData <- rawBatchData
        res$ids <- ids
        isolate(values$colnames <- colnames(rawBatchData))
        progress$set(value = 1, message = "=====Completed!=====")
        Sys.sleep(1)
        progress$close()
        return(res)
      } # if/else for data checking
    }) # _withBusyIndicatorServer()_
  })             # B_res, main results for batching processing
  observeEvent(input$B_directory, {
    B_table01.01 <- list.files(path = parseDirPath(volumes, input$B_directory),
                               all.files = FALSE)
    B_table01.01 <- grep(pattern = input$B_postfix, B_table01.01, value = TRUE)
    B_table01.01 <- sort(B_table01.01)
    No.<-c(1:length(B_table01.01))
    B_table01.01 <- cbind(No., data = B_table01.01)
    values$B_table01.01<-B_table01.01
  })
  observeEvent(input$B_uploadAnaRes, {
    B_table01.01<-read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                 "/Table_1FileList.csv", sep = ""), 
                           header=TRUE, sep=",")
    values$B_table01.01<-B_table01.01
  })
  observe({
  if(is.null(input$B_uploadAnaRes) && is.null(input$B_directory)){
    values$B_table01.01 <- NULL
    }
  })                                         
  observe({
    if(is.null(input$B_uploadAnaRes) && ! is.null(B_res())){
      B_table01.02 <- B_res()$Results
      B_table01.03 <- B_res()$GResult
      B_table01.04 <- B_res()$Spatial
      B_table01.05 <- B_res()$rawBatchData
      B_table01.06 <- data.frame(SampleID = B_res()$ids, Group = NA, Label = NA)
      B_table01.02 <- cbind(SampleID = rownames(B_table01.02), B_table01.02)
      B_table01.03 <- cbind(SampleID = rownames(B_table01.03), B_table01.03)
      B_table01.04 <- cbind(SampleID = rownames(B_table01.04), B_table01.04)
    } else if(! is.null(input$B_uploadAnaRes)){
      B_table01.02 <- read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                   "/Table_2Results.csv", sep = ""), 
                             header=TRUE, sep=",")
      rownames(B_table01.02) <- as.character(B_table01.02$SampleID)
      B_table01.03 <- read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                   "/Table_3GResult.csv", sep = ""), 
                             header=TRUE, sep=",")
      rownames(B_table01.03) <- as.character(B_table01.03$SampleID)
      B_table01.04 <- read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                   "/Table_4Spatial.csv", sep = ""), 
                             header=TRUE, sep=",")
      rownames(B_table01.04) <- as.character(B_table01.04$SampleID)
      B_table01.05 <- read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                   "/Table_5RawData.csv", sep = ""), 
                             header=TRUE, sep=",")
      B_table01.06 <- read.csv(paste(parseDirPath(volumes, input$B_uploadAnaRes), 
                                   "/Table_6IDs.csv", sep = ""), 
                             header=TRUE, sep=",")
    }
    isolate({
      values$B_table01.02<-B_table01.02
      values$B_table01.03<-B_table01.03
      values$B_table01.04<-B_table01.04
      values$B_table01.05<-B_table01.05
      values$B_table01.06<-B_table01.06
    })
  })                                         # Other tables updata
  output$B_table01.01 <- DT::renderDataTable(            # B_table01.01, Table 1: Files List
    values$B_table01.01,
    caption = 'Table 1: Files List',
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$B_table01.02 <- DT::renderDataTable(            # B_table01.02, Table 2: Wave Features Cell Level
    values$B_table01.02,
    caption = 'Table 2: Wave Features Cell Level',
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$B_table01.03 <- DT::renderDataTable(            # B_table01.03, Table 3: Wave Features Global Level
    values$B_table01.03,
    caption = 'Table 3: Wave Features Global Level',
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$B_table01.04 <- DT::renderDataTable(            # B_table01.04, Table 4: Moran's Index
    values$B_table01.04,
    caption = "Table 4: Moran's Index" ,
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$B_table01.05 <- DT::renderDataTable(            # B_table01.05, Table 5: Raw Data
    values$B_table01.05,
    caption = "Table 5: Raw Data" ,
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$B_table01.06 <- DT::renderDataTable(            # B_table01.06, Table 6: Simple IDs
    values$B_table01.06,
    caption = "Table 6: Simple IDs" ,
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$AnalysisResults.zip <- downloadHandler(         # Download all tables
    filename = function() {
      paste("AnalysisResults", "zip", sep = ".")
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      write.csv(values$B_table01.01, file = "Table_1FileList.csv", row.names = FALSE)
      write.csv(values$B_table01.02, file = "Table_2Results.csv", row.names = FALSE)
      write.csv(values$B_table01.03, file = "Table_3GResult.csv", row.names = FALSE)
      write.csv(values$B_table01.04, file = "Table_4Spatial.csv", row.names = FALSE)
      write.csv(values$B_table01.05, file = "Table_5RawData.csv", row.names = FALSE)
      write.csv(values$B_table01.06, file = "Table_6IDs.csv", row.names = FALSE)
      write.csv(values$cells, file = "Table_5RawData_Cells.csv", row.names = FALSE)
      write.csv(values$region, file = "Table_5RawData_Global.csv", row.names = FALSE)
      fs <- c(paste(tmpdir,"/Table_1FileList.csv", sep = ""),
              paste(tmpdir,"/Table_2Results.csv", sep = ""),
              paste(tmpdir,"/Table_3GResult.csv", sep = ""),
              paste(tmpdir,"/Table_4Spatial.csv", sep = ""),
              paste(tmpdir,"/Table_5RawData.csv", sep = ""),
              paste(tmpdir,"/Table_6IDs.csv", sep = ""),
              paste(tmpdir,"/Table_5RawData_Cells.csv", sep = ""),
              paste(tmpdir,"/Table_5RawData_Global.csv", sep = ""))
      zip(zipfile=fname, files = fs, flags = "-j")
    }
  )
  #///////////////////////#
  #///end.01 Data Unput///#
  #///////////////////////#
  #=======================#
  #
  #### ++02.Statistical Analysis ####
  observeEvent(input$B_Sta, {
    withBusyIndicatorServer("B_Sta", {
      if(is.null(values$B_table01.01) | is.null(values$B_table01.02) |
         is.null(values$B_table01.03) | is.null(values$B_table01.04) | 
         is.null(values$B_table01.05) | is.null(values$B_table01.06) |
         is.na(values$B_table02.01$Group)) {
        return(NULL)
      } else {
        # Cluster analysis based on Index J
        groups <- cutree(
          hclust(
            dist(
              c(values$B_table01.02$J_index)
            ), #_dist()_
            method = "ward.D"
          ), #_hclust()_
          k = 2
        ) #==== groups <- cutree() ====#
        G1.mean <- mean(
          c(values$B_table01.02$J_index)[groups == 1]
        )
        G2.mean <- mean(
          c(values$B_table01.02$J_index)[groups == 2]
        )
        if(G1.mean > G2.mean) {
          groups[groups == 2] <- 0
          groups[groups == 1] <- 1
        } else if(G1.mean < G2.mean) {
          groups[groups == 1] <- 0
          groups[groups == 2] <- 1
        } else {
          stop("Index J cluster error")
        }
        Label.C <- c()
        Label.S <- c()
        Tag.C <- c()
        Tag.S <- c()
        ratio <- c()
        for (i in 1:dim(values$B_table02.01)[1]) {
          ratio <- c(
            ratio,
            (length(
              groups[values$B_table01.02$id == values$B_table02.01$SampleID[i]]
              [groups[values$B_table01.02$id == values$B_table02.01$SampleID[i]] == 1]
            )/length(
              values$B_table01.02$id
              [ values$B_table01.02$id == values$B_table02.01$SampleID[i]] # values$B_table01.02$id
            ))*100
          )
          Label.C <- c(
            Label.C,
            as.vector(
              rep(
                values$B_table02.01$Label[i],
                length(
                  values$B_table01.02$id[
                    values$B_table01.02$id == values$B_table02.01$SampleID[i]
                    ] # values$B_table01.02$id
                ) #_length()_
              ) #_rep()_
            ) #_as.vector()_
          ) #==== Label.C <- c() ====#
          Tag.C <- c(
            Tag.C,
            as.vector(
              rep(
                values$B_table02.01$Group[i],
                length(
                  values$B_table01.02$id[
                    values$B_table01.02$id == values$B_table02.01$SampleID[i]
                    ] # values$B_table01.02$id
                ) #_length()_
              ) #_rep()_
            ) #_as.vector()_
          ) #==== Tag.C <- c() ====#
          Label.S <- c(
            Label.S,
            as.vector(
              rep(
                values$B_table02.01$Label[i], 2
              ) #_rep()_
            ) #_as.vector()_
          ) #==== Label.C <- c() ====#
          Tag.S <- c(
            Tag.S,
            as.vector(
              rep(
                values$B_table02.01$Group[i], 2
              ) #_rep()_
            ) #_as.vector()_
          ) #==== Tag.C <- c() ====#
        }                # Assign Label, Tag to each ID at each level
        values$B_table02.02 <- cbind(values$B_table01.02, 
                                     Label = Label.C, Tag = Tag.C, Group = groups)
        values$B_table02.03 <- cbind(values$B_table01.03, Ratio = ratio,
                                     Label = values$B_table02.01$Label,
                                     Tag = values$B_table02.01$Group)
        values$B_table02.04 <- cbind(values$B_table01.04, Label = Label.S, Tag = Tag.S)
        #    Independent 2-gtoup Mann-Whitney U test see here: 
        # https://www.statmethods.net/stats/nonparametric.html
        Inv.Col.CS <- c(2:10)                                         # Set the investigation column for Cell level and Moran's index results
        Inv.Col.G <- c(2:12, 14)                                      # Set the investigation column for Global level results
        group.n <- length(unique(values$B_table02.01$Group))
        # Statistic Results for cell level wave features
        P.value <- c()
        statistic <- c()
        out <- data.frame()
        summary <- NULL
        for (i in Inv.Col.CS) {
          test <- wilcox.test(as.numeric(values$B_table02.02[, i]) ~ values$B_table02.02$Tag)
          P.value <- c(P.value, test$p.value)
          statistic <- c(statistic, test$statistic)
        }
        summary <- describeBy(values$B_table02.02[, Inv.Col.CS], values$B_table02.02$Label)
        for (i in 1:group.n) {
          out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
        }
        out <- rbind(out, P.value = P.value, Statistic = statistic)
        values$B_table03.01 <- cbind(Terms = rownames(out), out)
        # Statistic Results for global level wave features
        P.value <- c()
        statistic <- c()
        out <- data.frame()
        summary <- NULL
        for (i in Inv.Col.G) {
          test <- wilcox.test(as.numeric(values$B_table02.03[, i]) ~ values$B_table02.03$Tag)
          P.value <- c(P.value, test$p.value)
          statistic <- c(statistic, test$statistic)
        }
        summary <- describeBy(values$B_table02.03[, Inv.Col.G], values$B_table02.03$Label)
        for (i in 1:group.n) {
          out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
        }
        out <- rbind(out, P.value = P.value, Statistic = statistic)
        values$B_table03.02 <- cbind(Terms = rownames(out), out)
        # Separate the P-value and Moran's Index
        MI <- c(1:(length(values$B_table02.04[, 1])/2))*2-1           # Array for Moran's Index 
        PMI <- c(1:(length(values$B_table02.04[, 1])/2))*2            # Array for P-value of Moran's Index
        # Statistic Results for Moran's Index
        values$B_table03.03<- values$B_table02.04[MI, ]
        P.value <- c()
        statistic <- c()
        out <- data.frame()
        summary <- NULL
        for (i in Inv.Col.CS) {
          test <- wilcox.test(as.numeric(values$B_table03.03[, i]) ~ values$B_table03.03$Tag)
          P.value <- c(P.value, test$p.value)
          statistic <- c(statistic, test$statistic)
        }
        summary <- describeBy(values$B_table02.03[, Inv.Col.CS], values$B_table03.03$Label)
        for (i in 1:group.n) {
          out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
        }
        out <- rbind(out, P.value = P.value, Statistic = statistic)
        values$B_table03.03 <- cbind(Terms = rownames(out), out)
        # Statistic Results for P-value of Moran's Index
        values$B_table03.04<- values$B_table02.04[PMI, ]
        P.value <- c()
        statistic <- c()
        out <- data.frame()
        summary <- NULL
        for (i in Inv.Col.CS) {
          test <- wilcox.test(as.numeric(values$B_table03.04[, i]) ~ values$B_table03.04$Tag)
          P.value <- c(P.value, test$p.value)
          statistic <- c(statistic, test$statistic)
        }
        summary <- describeBy(values$B_table03.04[, Inv.Col.CS], values$B_table03.04$Label)
        for (i in 1:group.n) {
          out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
        }
        out <- rbind(out, P.value = P.value, Statistic = statistic)
        values$B_table03.04 <- cbind(Terms = rownames(out), out)
        # Statistic Results among groups
        values$B_table03.04<- values$B_table02.04[PMI, ]
        P.value <- c()
        statistic <- c()
        out <- data.frame()
        summary <- NULL
        GroupName <- paste(values$B_table02.02$Label, values$B_table02.02$Group, sep = "")
        for (i in Inv.Col.CS) {
          test<-dunn.test(as.numeric(values$B_table02.02[, i]), GroupName, kw = FALSE, method = "bh")
          if (i == 2) {
            P.value <- data.frame(test$comparisons, test$P.adjusted)
          } else {
            P.value <- cbind(P.value, test$P.adjusted)
          }
        }
        summary <- describeBy(values$B_table02.02[, Inv.Col.CS], GroupName)
        for (i in 1:((group.n)*2)) {
          out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
        }
        out <- cbind(Terms = rownames(out), out)
        colnames(P.value) <- colnames(out)
        out <- rbind(out, P.value = P.value)
        values$B_table03.05 <- out
      }
    })
  })
  observe({
    if(is.null(input$B_GroupSetInput)) {
      isolate(values$B_table02.01 <- data.frame(SampleID = NA, 
                                                Group = NA, Label = NA))
    } else {
      GroupSet <- read.csv(input$B_GroupSetInput$datapath, header = TRUE, sep = ",")
      values$B_table02.01 <- GroupSet
    }
  })
  observeEvent(input$B_Assig, {
    seRow <- input$B_table02.01_rows_selected
    isolate({
      values$B_table02.01[seRow, 2] <- as.numeric(input$B_GroupMark)
      values$B_table02.01[seRow, 3] <- input$B_label
    })
  })
  observeEvent(input$B_Initi, {
    isolate({
      values$B_table02.01 <- values$B_table01.06
    })
  })
  output$Group_Set.csv<-downloadHandler(
    filename = "Group_Set.csv",
    content = function(file){
      write.csv(values$B_table02.01, file, row.names = FALSE)
    }
  )
  output$B_table02.01 <- DT::renderDataTable(
    values$B_table02.01,
    caption = "Table 1: Group Setting",
    rownames = FALSE,
    options = list(
      lengthChange = FALSE,
      pageLength = values$sampleSize,
      searching = FALSE
    )
  )
  output$B_table02.02 <- DT::renderDataTable(
    values$B_table02.02,
    caption = "Table 2 Checking: Wave Features Cell Level",
    rownames = FALSE
  )
  output$B_table02.03 <- DT::renderDataTable(
    values$B_table02.03,
    caption = "Table 3 Checking: Wave Features Global Level",
    rownames = FALSE
  )
  output$B_table02.04 <- DT::renderDataTable(
    values$B_table02.04,
    caption = "Table 4 Checking: Moran's Index",
    rownames = FALSE
  )
  output$GroupedData.zip <- downloadHandler(             # Download grouped data
    filename = function() {
      paste("GroupedData", "zip", sep = ".")
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      if(input$B_Mark == FALSE) {
        write.csv(values$B_table02.02, file = "Table_2ResultsG.csv", row.names = FALSE)
        write.csv(values$B_table02.03, file = "Table_3GResultG.csv", row.names = FALSE)
        write.csv(values$B_table02.04, file = "Table_4SpatialG.csv", row.names = FALSE)
      } else if(input$B_Mark == TRUE) {
        write.csv(cbind(values$B_table02.02, Condition = rep(input$B_MarkCo, length(values$B_table02.02[, 1])), 
                        Condition.Label = paste(rep(input$B_MarkCo, length(values$B_table02.02[, 1])), values$B_table02.02$Label, sep = ".")), 
                  file = "Table_2ResultsG.csv", row.names = FALSE)
        write.csv(cbind(values$B_table02.03, Condition = rep(input$B_MarkCo, length(values$B_table02.03[, 1])), 
                        Condition.Label = paste(rep(input$B_MarkCo, length(values$B_table02.03[, 1])), values$B_table02.03$Label, sep = ".")), 
                  file = "Table_3GResultG.csv", row.names = FALSE)
        write.csv(cbind(values$B_table02.04, Condition = rep(input$B_MarkCo, length(values$B_table02.04[, 1])), 
                        Condition.Label = paste(rep(input$B_MarkCo, length(values$B_table02.04[, 1])), values$B_table02.04$Label, sep = ".")), 
                  file = "Table_4SpatialG.csv", row.names = FALSE)
      }
      fs <- c(paste(tmpdir,"/Table_2ResultsG.csv", sep = ""),
              paste(tmpdir,"/Table_3GResultG.csv", sep = ""),
              paste(tmpdir,"/Table_4SpatialG.csv", sep = ""))
      zip(zipfile=fname, files = fs, flags = "-j")
    }
  )
  output$B_ui02 <- renderUI({
    if(input$B_Mark == FALSE) {
      return(NULL)
    } else if(input$B_Mark == TRUE) {
      textInput("B_MarkCo", "Enter the mark", value = "Drug1")
    }
  })
  #/////////////////////////////////#
  #///end.02 Statistical analysis///#
  #/////////////////////////////////#
  #=================================#
  #
  #### ++03.Results ####
  output$B_table03.01 <- DT::renderDataTable(
    values$B_table03.01,
    caption = "Table 1 Summary: Wave Features Cell Level",
    rownames = FALSE
  )
  output$B_table03.02 <- DT::renderDataTable(
    values$B_table03.02,
    caption = "Table 2 Summary: Wave Features Global Level",
    rownames = FALSE
  )
  output$B_table03.03 <- DT::renderDataTable(
    values$B_table03.03,
    caption = "Table 3 Summary: Moran's Index",
    rownames = FALSE
  )
  output$B_table03.04 <- DT::renderDataTable(
    values$B_table03.04,
    caption = "Table 4 Summary: P-value of Moran's Index",
    rownames = FALSE
  )
  output$B_table03.05 <- DT::renderDataTable(
    values$B_table03.05,
    caption = "Table 5 Summary: Wave Features Cell Level among groups by cluster analysis",
    rownames = FALSE
  )
  output$Statistic_Results.zip <- downloadHandler(       # Download statistic results
    filename = function() {
      paste("Statistic_Results", "zip", sep = ".")
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      write.csv(values$B_table03.01, file = "Table_1ResultsSta.csv", row.names = FALSE)
      write.csv(values$B_table03.02, file = "Table_2GResultSta.csv", row.names = FALSE)
      write.csv(values$B_table03.03, file = "Table_3MoransISta.csv", row.names = FALSE)
      write.csv(values$B_table03.04, file = "Table_4PMoranISta.csv", row.names = FALSE)
      write.csv(values$B_table03.05, file = "Table_5ResAmGrSta.csv", row.names = FALSE)
      fs <- c(paste(tmpdir,"/Table_1ResultsSta.csv", sep = ""),
              paste(tmpdir,"/Table_2GResultSta.csv", sep = ""),
              paste(tmpdir,"/Table_3MoransISta.csv", sep = ""),
              paste(tmpdir,"/Table_4PMoranISta.csv", sep = ""),
              paste(tmpdir,"/Table_5ResAmGrSta.csv", sep = ""))
      zip(zipfile=fname, files = fs, flags = "-j")
    }
  )
  #////////////////////#
  #///end.03.Results///#
  #////////////////////#
  #====================#
  #
  #### ++04.Plot Output ####
  B_plotBox04.01<-reactive({
    o<-values$B_table02.02 #input data
    Inv.Col.CS <- c(2:10)
    GroupLabel<-"Tag" #to sort
    GroupTag<-"Label" #to identify
    Pvalue<-round(as.numeric(values$B_table03.01["P.value", Inv.Col.CS]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Index J")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- length(unique(Inv.Col.CS)) #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.99"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], Inv.Col.CS[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"), legend.position="none")+ylim(NA, max(df$y100)*1.15) +
        values$theme_ed
      if(input$B_Violin == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, Inv.Col.CS[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }) 
  B_plotBox04.02<-reactive({
    o<-values$B_table02.03 #input data
    Inv.Col.G <- c(2:12, 14) 
    GroupLabel<-"Tag" #to sort
    GroupTag<-"Label" #to identify
    Pvalue<-round(as.numeric(values$B_table03.02["P.value", c(2:13)]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", 
                    input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit", 
                    "Arbitrary Unit", "Arbitrary Unit", "Percentage")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", 
                    "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", 
                    "Index J", "Spearman's Rho", "P-value of Spearman's Rho", "Ratio")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- length(unique(Inv.Col.G)) #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.99"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], Inv.Col.G[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"), legend.position="none")+ylim(NA, max(df$y100)*1.15) +
        values$theme_ed
      if(input$B_Violin == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, Inv.Col.G[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }) 
  B_plotBox04.03<-reactive({
    MI <- c(1:(length(values$B_table02.04[, 1])/2))*2-1           # Array for Moran's Index 
    o<-values$B_table02.04[MI, ] #input data
    Inv.Col.CS <- c(2:10)
    GroupLabel<-"Tag" #to sort
    GroupTag<-"Label" #to identify
    Pvalue<-round(as.numeric(values$B_table03.02["P.value", c(2:13)]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", 
                    input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit", 
                    "Arbitrary Unit", "Arbitrary Unit", "Percentage")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", 
                    "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", 
                    "Index J", "Spearman's Rho", "P-value of Spearman's Rho", "Ratio")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- length(unique(Inv.Col.CS)) #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.99"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], Inv.Col.CS[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"), legend.position="none")+ylim(NA, max(df$y100)*1.15) +
        values$theme_ed
      if(input$B_Violin == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, Inv.Col.CS[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }) 
  B_plotBox04.04<-reactive({
    PMI <- c(1:(length(values$B_table02.04[, 1])/2))*2             # Array for P-value of Moran's Index 
    o<-values$B_table02.04[PMI, ] #input data
    Inv.Col.CS <- c(2:10)
    GroupLabel<-"Tag" #to sort
    GroupTag<-"Label" #to identify
    Pvalue<-round(as.numeric(values$B_table03.02["P.value", c(2:13)]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", 
                    input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit", 
                    "Arbitrary Unit", "Arbitrary Unit", "Percentage")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", 
                    "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", 
                    "Index J", "Spearman's Rho", "P-value of Spearman's Rho", "Ratio")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- length(unique(Inv.Col.CS)) #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.99"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], Inv.Col.CS[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) + geom_hline( yintercept = 0.05, color = "Red") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"), legend.position="none")+ylim(NA, max(df$y100)*1.15) +
        values$theme_ed
      if(input$B_Violin == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, Inv.Col.CS[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }) 
  B_plotBox04.05<-reactive({
    RES<-values$B_table03.05
    position<-c(1.17, 1.29, 1.05, 1.41, 1.17, 1.29)
    xmin <- c(0.75, 0.75, 1.30, 0.75, 1.27, 1.77)
    xmax <- c(1.23, 1.73, 1.70, 2.25, 2.25, 2.25)
    o<-values$B_table02.02 #input data
    start<-2
    end<-10
    GroupLabel<-"Tag" #to sort
    GroupTagL1<-"Label" #to identify at level 1
    GroupTagL2<-"Group" #to identify at level 2
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Index J")
    dfxL1<-c()
    for(i in 1: length(o[, GroupTagL1])) {
      if(i == 1) {
        j<-1
        dfxL1<-c(dfxL1, as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i]), dfxL1)))) {
        j<-j+1
        dfxL1[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i])
      }
    }
    dfxL2<-c()
    for(i in 1: length(o[, GroupTagL2])) {
      if(i == 1) {
        j<-1
        dfxL2<-c(dfxL2, as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i]), dfxL2)))) {
        j<-j+1
        dfxL2[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      signif<-c()
      for(n in 53:58) {
        anno6 <-if(RES[n,q[i]]>=0.9999){
          "P>0.999"
        } else if(RES[n,q[i]]>0.025){
          paste("P>", format(floor(RES[n, q[i]]*1000)/1000, digits = 3), sep = "") 
        } else if(RES[n,q[i]]<0.025 && RES[n,q[i]]>0.001){
          "*"
        } else if(RES[n,q[i]]<0.001 && RES[n,q[i]]>0.0001){
          "**"
        }else if(RES[n,q[i]]<0.0001 && RES[n,q[i]]>0.00001){
          "***"
        } else {"****"}
        anno<- geom_signif(annotation=formatC(anno6, digits=2),
                           y_position=max(o[, q[i]])*position[n-52], xmin=xmin[n-52], xmax=xmax[n-52], textsize = 5, 
                           tip_length = c(0,0))
        signif<-c(signif, anno)
      }
      x3<-c(0.75, 1.25, 1.75, 2.25)
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfxL1)) {
        for(h in 1:length(dfxL2)) {
          temp<-as.numeric(o[o[GroupTagL1] == dfxL1[j], ][o[o[GroupTagL1] == dfxL1[j], ][GroupTagL2] == dfxL2[h], q[i]])
          y0<-c(y0, min(temp))
          y25<-c(y25, quantile(temp, 0.25))
          y50<-c(y50, median(temp))
          y75<-c(y75, quantile(temp, 0.75))
          y100<-c(y100, max(temp))
          ymean<-c(ymean, mean(temp))
        }
      }
      df<-data.frame(x=rep(dfxL1, each=2), x2=rep(dfxL2, 2), x3=x3, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100, colour = factor(df$x2)),
                     stat = "identity", position=position_dodge(width = 1.01), varwidth = TRUE) +signif[1:6]+
        labs(y = YaxisNames[i], title = TitleNames[i] , colour = "Group by\n Index J cluster") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))+ylim(NA, max(df$y100)*1.43) + 
        values$theme_ed
      if(input$B_Violin == TRUE) {
        gp<-gp+geom_violin(data = o, position=position_dodge(width = 1.01),
                           aes(x=o[, GroupTagL1], y= o[, q[i]], alpha = 0.3, colour = factor(o[, GroupTagL2])))+
          geom_point(aes(x=x3, y=ymean, colour = factor(x2)))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  })
  B_plotBox04.06<-reactive({
    o <- values$B_table02.02
    start<-2
    end<-10
    cols<-2
    q<-c(start:end)
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$B_FirstCol, "Gray Level", input$B_Unit, "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Index J")
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      gp<-ggplot(o, aes(x = o[, q[i]], fill = o$Label))+geom_histogram(position="dodge")+
        labs(y = "Count", x = YaxisNames[i], title = TitleNames[i])+
        guides(fill=guide_legend(title="Group"))+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold")) +
        values$theme_ed
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  })
  output$B_plotBox04.01 <- renderPlot(B_plotBox04.01()) #Cell level
  output$B_plotBox04.02 <- renderPlot(B_plotBox04.02()) #Region level
  output$B_plotBox04.03 <- renderPlot(B_plotBox04.03()) #Moran index for each wave feature
  output$B_plotBox04.04 <- renderPlot(B_plotBox04.04()) #P value of Moran index for each wave
  output$B_plotBox04.05 <- renderPlot(B_plotBox04.05()) #Comparsion among groups
  output$B_plotBox04.06 <- renderPlot(B_plotBox04.06()) #Histogram
  output$B_ui01.side <- renderUI({
    switch(input$B_Violin,
           "Cell Level" = NULL,
           "Region Level" = NULL,
           "Moran Index" = NULL,
           "Significance of Moran Index" = NULL,
           "Comparsion among Groups" = NULL,
           "Histogram" = NULL
    )
  })
  output$B_ui01 <- renderUI({
    if(is.null(values$B_table01.01) | is.null(values$B_table01.02) |
       is.null(values$B_table01.03) | is.null(values$B_table01.04) | 
       is.null(values$B_table01.05) | is.null(values$B_table01.06) |
       is.na(values$B_table02.01$Group)) {
      return(NULL)
    } else {
      switch(input$B_level,
             "Cell Level" = list(
               tags$h4("Wave feature for each cell"),
               tags$hr(),
               plotOutput("B_plotBox04.01", width = "800px", height = "800px")
             ),
             "Region Level" = list(
               tags$h4("Wave feature for Region"),
               tags$hr(),
               plotOutput("B_plotBox04.02", width = "800px", height = "1067px")
             ),
             "Moran Index" = list(
               tags$h4("Moran index for each wave feature"),
               tags$hr(),
               plotOutput("B_plotBox04.03", width = "800px", height = "800px")
             ),
             "Significance of Moran Index" = list(
               tags$h4("P value of Moran index for each wave feature"),
               tags$hr(),
               plotOutput("B_plotBox04.04", width = "800px", height = "800px")
             ),
             "Comparsion among Groups" = list(
               tags$h4("Comparsion among Groups"),
               tags$hr(),
               plotOutput("B_plotBox04.05", width = "800px", height = "1067px")
             ),
             "Histogram" = list(
               tags$h4("Histogram"),
               tags$hr(),
               plotOutput("B_plotBox04.06", width = "1200px", height = "1300px")
             )
      )
    }
  })
  #////////////////////////#
  #///end.04 Plot Output///#
  #////////////////////////#
  #========================#
  #
  #|||||||||||||||||||||||||||||||||#
  #|||||| end.Batch Processing |||||#
  #|||||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ####==== Batch ANOVA Processing ====####
  values$Groups <- NULL                                 #How many groups in the dataset
  shinyFileChoose(input, 'file', roots=volumes, session=session, restrictions=system.file(package='base'))
  BDC <- reactive({
    if (is.null(input$file))
      return(NULL) 
    else
      FileNo <- length(c(parseFilePaths(volumes, input$file)$datapath))
    data <- as.vector(parseFilePaths(volumes, input$file)$datapath)
    ResBind<-c()
    for(i in 1:FileNo) {
      ResBind0<-read.csv(data[i], header=TRUE, sep = ",")
      ResBind <- rbind(ResBind, ResBind0)
    }
    isolate(values$Wdir<-sub(pattern = parseFilePaths(volumes, input$file)$name[1], 
                             replacement = "", 
                             x = parseFilePaths(volumes, input$file)$datapath[1], fixed = TRUE))
    return(ResBind)
  })                                #BDC: Batch Data Combination
  SRes <- eventReactive(input$AnB01, {
    withBusyIndicatorServer("AnB01",{
      if(is.null(input$file)) {
        stop("Please input files first")
      }
      data <- BDC()
      if(input$TI01 != "NA" && input$TI02 != "NA") {
        data <- data[data[, input$TI01] == input$TI02, ]
      }
      KWtest <- c("Kruskal-Walls rank sum test")                                                                       #Kruskal-Walls rank sum test
      DuTest <- c()                                                                                                    #Dunn's test of multiple comparisons
      GSD <- describeBy(data[, input$NI01 : input$NI02], data[, input$TI03])                                           #GSD: General Statistical Describe
      group.n <- length(GSD)                                                                                           #Amount of group in GSD
      isolate(values$group.names<-names(GSD))
      if(group.n <3 ) {
        stop("At least three groups")
      }
      isolate(values$Groups<-group.n)                                                                                  #pass value to the indepented variable
      GSDout <- data.frame()                                                                                           #GSDout: General Statistical Describe for Output
      for(i in 1: group.n) {
        GSDout <- rbind(GSDout, as.data.frame(t(as.data.frame(GSD[i]))), stringsAsFactors=FALSE)
      }
      GSDout <- cbind(rep(names(GSD), each = 13),                                                                      #Repeat each element 13 times
                      GSDout, 
                      stringsAsFactors=FALSE
      )
      colnames(GSDout) <- c("Comparisons", colnames(data[input$NI01:input$NI02]))
      for(i in input$NI01 : input$NI02) {
        KWtest0 <- kruskal.test(data[, i] ~ data[, input$TI03])                                                        #Kruskal-Walls rank sum test
        KWtest0 <- as.numeric(KWtest0$p.value)
        KWtest <- c(KWtest, KWtest0)
        DuTest0 <- dunn.test(as.numeric(data[, i]), data[, input$TI03], kw = FALSE, method = "by", alpha = input$NI03) #Dunn's test of multiple comparisons
        if (i == input$NI01) {
          DuTest <- data.frame(DuTest0$comparisons, DuTest0$P.adjusted, stringsAsFactors=FALSE)
        } else {
          DuTest <- cbind(DuTest, DuTest0$P.adjusted, stringsAsFactors=FALSE)
        }
      }
      SRes<-rbind(DuTest, KWtest, stringsAsFactors=FALSE)
      colnames(SRes) <- c("Comparisons", colnames(data[input$NI01:input$NI02]))
      SRes <- rbind(SRes, GSDout, stringsAsFactors=FALSE)
      #Follow part is to output the pdf file for plot the results
      Data <- data
      RES <- SRes 
      comparN <- factorial(values$Groups)/(2*factorial(values$Groups-2))                                               #How many combinations in Dunn's test
      xmax<-xmax(values$Groups)
      xmin<-xmin(values$Groups)
      pos<-position(values$Groups)
      end<-(input$NI02-input$NI01+2)
      dfx<-as.character(values$group.names)
      pdf(paste(values$Wdir, input$TI00, ".pdf", sep = ""))                                                            #PDF write start
      for(i in 2:end){
        gp<-NULL
        seq<-c(input$NI01 : input$NI02)
        GeomAnnoSet <- c()
        for(j in 1:comparN){
          pvalue<-as.numeric(round(as.numeric(RES[j,i]), 10))
          anno0 <- if(pvalue >= 0.9999) {
            "P>0.999"
          } else if(pvalue> input$NI03/2) {
            paste("P>", format(floor(as.numeric(RES[j, i])*1000)/1000, digits = 3), sep = "")
          } else if(pvalue>as.numeric(decimallength(decimalplaces(input$NI03))) && pvalue< as.numeric(input$NI03/2)) {
            "*"
          } else if(pvalue>as.numeric(decimallength(decimalplaces(input$NI03)+1)) && pvalue< as.numeric(decimallength(decimalplaces(input$NI03)))) {
            "**"
          } else if(pvalue>as.numeric(decimallength(decimalplaces(input$NI03)+2)) && pvalue< as.numeric(decimallength(decimalplaces(input$NI03)+1))) {
            "***"
          } else { "****" }
          p<-as.numeric(seq[(i-1)])
          anno <- geom_signif(annotations = c(anno0),
                              y_position = as.numeric(max(Data[, p]))*pos[j], xmin = xmin[j]+0.1, xmax = xmax[j]-0.1, textsize = 5,
                              tip_length = c(0,0)
          )
          GeomAnnoSet <- c(GeomAnnoSet, anno)
        } #single items finished & annotation finished
        q<-as.numeric(seq[(i-1)])
        n<-i-1
        #convert the original data for customize the boxplot
        y0<-c()
        y25<-c()
        y50<-c()
        y75<-c()
        y100<-c()
        ymean<-c()
        for(i02 in 1:length(dfx)) {
          temp<-Data[Data[, input$TI03] == dfx[i02], q]
          y0<-c(y0, min(temp))
          y25<-c(y25, quantile(temp, 0.25))
          y50<-c(y50, median(temp))
          y75<-c(y75, quantile(temp, 0.75))
          y100<-c(y100, max(temp))
          ymean<-c(ymean, mean(temp))
        }
        df1<-data.frame(
          x=dfx,
          y0=y0,
          y25=y25,
          y50=y50,
          y75=y75,
          y100=y100,
          ymean=ymean
        )
        #convert the original data for customize the boxplot
        gp <- ggplot(df1, aes(x = x, y = y100))+
          geom_boxplot(position = position_dodge(1), aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100, colour = factor(x)),
                       stat = "identity")+GeomAnnoSet+
          labs(colour = colnames(Data)[q])+
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
                axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text=element_text(size=14), 
                axis.title=element_text(size=14,face="bold")) + ylim(NA, max(Data[, q])*(max(pos)+0.05))
        if(input$Violin01 == TRUE) {
          gp<-gp+geom_violin(data = Data, aes(x=Data[, input$TI03], y= Data[, q], alpha = 0.3))+
            geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
        }
        print(gp)
      }                                                                                             #entire measurements finished $ Graph finished
      dev.off()#PDF write end
      file.copy(paste(values$Wdir, input$TI00, ".pdf", sep = ""), "www/temp.pdf", overwrite = TRUE)
      #Multi-plot in loop end
      return(SRes)
    })#--- withBusyIndicatorServer End
  })             #SRes: Statistical Results
  IFO01 <- observe({
    if(is.null(SRes()))
      return(NULL)
    else
      return(NULL)
  })                               #IFO: Interface Optimize
  output$filepaths <- renderPrint({parseFilePaths(volumes, input$file)$datapath[1]})
  output$output01 <- DT::renderDataTable(
    if (is.null(input$file))
      return(NULL) 
    else
      format(BDC(), digits = 3, scientific = FALSE),
    caption = 'Combined dataset'
  )
  output$output02 <- DT::renderDataTable(
    if (is.null(BDC()))
      return(NULL)
    else
      SRes()
  )
  output$SRes<-downloadHandler(
    filename = input$TI00,
    content = function(file){
      write.csv(SRes(), file = paste(values$Wdir, input$TI00, ".csv", sep = ""), row.names = TRUE)
    }
  )
  output$pdf <- renderUI({
    if(is.null(SRes())){
      return(NULL)
    } else {
      tags$iframe(style="height:800px; width:800px; scrolling=yes", src="temp.pdf", sep = "")
    }
  })
  #||||||||||||||||||||||||||||||||||||||#
  #|||||| end.Batch ANOVA Processing|||||#
  #||||||||||||||||||||||||||||||||||||||#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  ###### Global Setting ######
  options(scipen = 5)
  session$onSessionEnded(stopApp)
  #//////////////////////////#
  #////end.Global Setting////#
  #//////////////////////////#
  #==========================#
} #_Server<-function(input, output, session){}_
shinyApp(ui = ui, server = server)