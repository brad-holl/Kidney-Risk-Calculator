library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

source("./global.R")


dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Main Menu") |>
    tagAppendChild(
      div(
        "Kidney Transplant Risk Calculator",
        style = "
      display: block;
      font-size: 2em;
      margin-block-start: 0.1em;
      color: white;
      margin-right: 50%",
        align = "right"
      ),
      .cssSelector = "nav"
    ),
  dashboardSidebar(
    #---------------#
    # SIDE BAR MENU #
    #---------------#
    sidebarMenu(
      id = "sidebar",
      menuItem(text = div(
        icon("dashboard"), HTML("&nbsp;&nbsp;Dashboard")),
        tabName = "dashboard"),
      menuItem(text = div(
        icon("database"), HTML("&nbsp;&nbsp;Database")),
        tabName = "database"),
      menuItem(text = div(
        icon("user-plus"), HTML("&nbsp;&nbsp;Add New Patient")),
        tabName = "addpatient"),
      menuItem(text = div(
        icon("calculator"), HTML("&nbsp;&nbsp;Risk Score Analysis")),
        tabName = "riskcalc"),
      menuItem(text = div(
        icon("magnifying-glass-chart"), HTML("&nbsp;&nbsp;Adult v.s. Pediatric")),
        tabName = "patient_comp")
    )
  ),
  
  #----------------#
  # DASHBOARD BODY #
  #----------------#
  dashboardBody(tabItems(
    # DASHBOARD PAGE #
    tabItem(
      "dashboard",
      title = "Dashboard",
      h1("Why Choose Our Kidney Transplant Risk Calculator?"),
      fluidRow(
        title = "description",
        div(
          style = "padding: 0 20px; margin-bottom: 10px; font-weight: bold;",
          HTML(
            "Rejection occurs when a patient's body reacts to foreign proteins in a transplanted kidney. 
            Each patient has unique medical history and circumstances that can affect their immune system's response. 
            Pediatric cases have additional complexities like immature immune systems, size mismatch, and medication adherence. 
            Using adult cases to judge pediatric patients in kidney transplantation can be misleading.
            How can we quickly determine if patients from different age groups will experience kidney rejection after transplantation?<br>
            Our Kidney Transplant Risk Calculator offers a convenient solution for medical scientists! 
            Whether it's an adult or a child, our calculator instantly analyzes gene expression data to assess the risk of transplant rejection. 
            It also allows for quick comparison between adult and pediatric cases. 
            By uploading a patient's gene expression data, you can assess the risk associated with kidney transplantation more effectively."
          )
        ),
        div(
          style = "padding: 0 20px; ",
          HTML(
            "The dashboard page is a brief overview of all data and showing difference between adult and pediatric patients,
            including numbers of adult and pediatric patients and their average risk respectively. 
            The table of recently added patients will show 6 recently added patients from database.
            Also allow medical scientists to check for top 50 defining genes if intersted in. <br>
            To use our risk calculator, please go to the Add New Patient page."
          )
        )
      ),
      h1("Overview of All Patients Data"),
      fluidRow(
        box(
          width = 12, 
          h2("Acute Rejection v.s. Stable"), 
          p("This pie chart below shows you the proportion of stable and rejected patients. 
            If you want to know specific numbers, hover the mouse over the part you want to check on the pie chart."),
          plotlyOutput('piechart')
        )
      ),
      fluidRow(
        fluidRow(
        style = "padding: 0 30px; margin-bottom: 10px;",
        h1("Numbers of patients currently")),
        valueBoxOutput('n_patients'), # total number of patients
        valueBoxOutput('a_patients'), # total number of adult patients
        valueBoxOutput('p_patients') # total number of pediatric patients
      ),
      fluidRow(
        fluidRow(
          style = "padding: 0 30px; margin-bottom: 10px;",
          h1("Average risk scores for Adult and Pediatric Patients")),
        # average risk scores
        valueBoxOutput(width = 6,'a_score'),
        valueBoxOutput(width = 6,'p_score')
      ),
      fluidRow(box(
        h2("Recently added patients"),
        dataTableOutput('recentlyAddedTable')
      ),
      box(
        h2("Top 50 defining genes"), 
        tabsetPanel(
          id = "tabs",
          tabPanel("Adult", div(style = "height: 300px; overflow: scroll;",
                                dataTableOutput('top50TableA'))
                   ),
          tabPanel("Pediatric", div(style = "height: 300px; overflow: scroll;",
                   dataTableOutput('top50TableP'))
                   )
        )
      )
      )
    ),
    
    
    # ADD NEW PATIENT PAGE #
    tabItem(
      "addpatient",
      fluidRow(style = "padding: 0 20px",
               h1("Add New Patient")),
      fluidRow(
        title = "description",
        div(
          style = "padding: 0 20px; margin-bottom: 20px;",
          HTML(
            "The Add New Patient page supports the entry of new patient information and the evaluation of new patient
            using a risk assessment model trained by our development team. \n
            
            The patient ID input and age category selection are required. You have to enter all the information in order to proceed.
            Patient ID can be alphabets, numbers or mixed format of alphabets and numbers.
            It is important to note that you have to select the correct age category. Wrong category will result in incorrect risk assessment score.
            You also need to upload a CSV file that contains the patient's gene expression data. We do not accept other file formats.\n

            Please click on the Calculate Risk Score button below after you entered all the required information.
            The tab will be redirect to the Risk Score Analysis, which shows the result of evaluation."
          )
        )
      ),
      fluidRow(style = "padding: 0 20px; margin-top: 20px;",
               div(
                 align = "center",
                 column(
                   width = 5,
                   offset = 1,
                   textInput("pID", "Enter Patient ID")
                 ),
                 column(
                   width = 5,
                   offset = 0,
                   fileInput("pfile", "Upload CSV file")
                 )
               )),
      fluidRow(style = "padding: 0 20px;",
               div(
                 width = 12,
                 align = "center",
                 radioButtons("category", "Age Categories:", choices = c("Adult", "Pediatric"))
               )),
      fluidRow(
        style = "padding: 0 20px; margin-top: 20px;",
        column(
          width = 6,
          offset = 3,
          div(
            style = "text-align: center;",
            actionButton(
              "calculate_button", 
              "Calculate Risk Score",
              style = "background-color: #3d389f; color: white; width: 80%;"
            )
          )
        )
      )
    ),
    
    # RISK SCORE PAGE #
    tabItem(
      "riskcalc",
      uiOutput("conditionalTabContent")
    ),
    
    # DATABASE PAGE #
    tabItem("database",
            h1("Database Overview"),
            div(
              style = "margin-bottom: 10px;",
              HTML(
                "The Database page contains the database of our application, which is divided into two categories: adult and pediatric. 
              The search box located in the upper right corner allows you to search for kidney transplant results and risk
              scores for any patient within these categories. You can also sort the table by clicking on the column name. 
              If you need to view a large amount of data, you can use the Show Entries button in the upper left corner to select
              the desired number of entries to display.
              "
              )
            ),
            fluidRow(
              box(
                width = 6,
                h2("Adult Database"),
                p("This table displays all data for the adult patients in our database. 
                  It includes the patient ID, a category ensuring they are adults, 
                  the outcome (whether it will be rejection or stable after kidney transplantation), 
                  and the risk score predicted by our risk calculator."),
                dataTableOutput("databaseA_table")
              ),
              box(
                width = 6,
                h2("Pediatric Database"),
                p("This table displays all data for the pediatric patients in our database. 
                  It includes the patient ID, a category ensuring they are pediatric patients, 
                  the outcome (whether it will be rejection or stable after kidney transplantation), 
                  and the risk score predicted by our risk calculator."),
                dataTableOutput("databaseP_table")
              )
            )
    ),
    
    
    # COMPARISON PAGE #
    tabItem(
      "patient_comp",
      h1("Patient Comparison"),
      div(
        style = "margin-bottom: 10px;",
        HTML(
          "By default, the Adult v.s. Pediatric page displays the average risk score for each age group and a boxplot depicting the top 5 genes. 
          By using the dropdown menu, you can select specific patients, and their corresponding information, 
          including outcome and risk score, are displayed alongside their gene expression data simultaneously."
        )
      ),
      fluidRow(box(
        width = 6,
        selectInput(
          "analysis_adult_id",
          "Select Adult Patient:",
          choices = c("All", databaseA$patient_id)
        )
      ),
      fluidRow(box(
        width = 6,
        selectInput(
          "analysis_pediatric_id",
          "Select Pediatric Patient:",
          choices = c("All", databaseP$patient_id)
        )
      ))),
      fluidRow(#box(width = 6, title = "Patient Plots"),
        fluidRow(column(width=12, 
                        valueBoxOutput("adult_outcome", width = 3), 
                        valueBoxOutput("adult_score", width = 3),
                        valueBoxOutput("pediatric_outcome", width = 3),
                        valueBoxOutput("pediatric_score", width = 3)))
      ),
      fluidRow(
        column(width = 6,
               box(
                 width = "100%",
                 title = "Top 5 Genes - Adult",
                 div(
                   style = "margin-bottom: 10px;",
                   HTML(
                     "This is a boxplot of the top 5 genes based on adult patients, 
                         with the patients expression of these genes being overlayed as indicated by the orange diamonds."
                   )
                 ),
                 plotlyOutput("analysis_plot_a"))
               ),
        fluidRow(
          column(width = 6,
                 box(
                   width = "100%",
                   title = "Top 5 Genes - Pediatric",
                   div(
                     style = "margin-bottom: 10px;",
                     HTML(
                       "This is a boxplot of the top 5 genes based on pediatric patients, 
                         with the patients expression of these genes being overlayed as indicated by the orange diamonds."
                     )
                   ),
                   plotlyOutput("analysis_plot_p"))
          )
        )
        ),
    )

  )) #dashboardBody(tabItems(
)