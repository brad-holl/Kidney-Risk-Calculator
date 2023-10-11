library(shiny)

source("./global.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  observe({
    pID <- isolate(input$pID)
    button <- isolate(input$calculate_button)
    if (is.null(pID) || !button) {
      output$conditionalTabContent <- renderUI({
        fluidPage(
          h1("Please add a new patient to proceed")
        )
      })
    } 
  })
  
  #------------------#
  # dashboard outputs#
  #------------------#
  
  output$n_patients <- renderValueBox({
    valueBox(dim(database_all_patients)[1], "Total number of patients")
  })
  
  output$p_patients <- renderValueBox({
    valueBox(dim(databaseP)[1],
             "Number of Pediatric patients")
  })
  
  output$a_patients <- renderValueBox({
    valueBox(dim(databaseA)[1], 
             "Number of Adult patients")
  })
  
  output$a_score <- renderValueBox({
    valueBox(
      value = round((sum(databaseA$risk_score) / dim(databaseA)[1]), 2),
      subtitle = "Average Risk Score of Adult patients",
      color = "blue",
      icon = icon("user")
    )
  })
  
  output$p_score <- renderValueBox({
    valueBox(
      value = round((sum(databaseP$risk_score) / dim(databaseP)[1]), 2),
      subtitle = "Average Risk Score of Pediatric patients",
      color = "yellow",
      icon = icon("child")
    )
  })
  
  output$recentlyAddedTable <- renderDataTable({
    datatable(tail(database_all_patients))
  })
  
  output$top50TableA <- renderDataTable({
    datatable(top50TableA)
  })
  
  output$top50TableP <- renderDataTable({
    datatable(top50TableP)
  })
  
  output$piechart <- renderPlotly({
    plot_ly(
      sunburst_df,
      type = "sunburst",
      labels =  ~ labels,
      parents =  ~ parents,
      values =  ~ values,
      branchvalues = "total"
    )
    
  })
  
  #-----------------#
  # Data set ouTputs #
  #-----------------#
  output$dataset <- renderDataTable(
    datatable(database_all_patients, selection = "single"))
  
  # observeEvent(input$dataset_rows_selected, {
  #   patient_category <- database_all_patients[input$dataset_rows_selected, ]$Category
  #   ifelse(is.na(patient_category), updateTabItems(session, "tabs", "pedaitric")),)
  # })
  
  #--------------------#
  # Upload new patient #
  #--------------------#
  
  # create empty variable to store new uploaded file
  new_file <- NULL
  
  # check if ID is blank or file not uploaded when click on button
  observeEvent(input$calculate_button, {
    if (is.null(input$pID) || input$pID == "") {
      showNotification("You have to fill in the patient ID.", type = "error")
    } else if (is.null(input$category)) {
      showNotification("You must choose an age category for the patient.", type = "error")
    } else if (is.null(input$pfile$name)) {
      showNotification("You must upload a file before calculation.", type = "error")
      #} else if (tools::file_ext(input$pfile$name) != "csv") {
      #  showNotification("Invalid file type. Please upload a CSV file.", type = "error")
    } else {
      # when all conditions are met
      
      output$conditionalTabContent <- renderUI({
        fluidPage(
          fluidRow(
            column(
              width = 12,
              style = "padding: 0 20px; position: relative;",
              h1(textOutput("title_patientID")),
              div(
                style = "position: absolute; top: 0; right: 20px;",
                actionButton("upload_button", "Upload Patient Data",
                             style = "background-color: #3d389f; color: white;")
              )
            )
          ),
          fluidRow(
            title = "description",
            div(
              id = "description",
              style = "padding: 0 20px; margin-bottom: 20px;",
              HTML("The Risk Score Analysis page shows the prediction outcome of patient you add. 
                   It will determine the risk of a kidney transplant based on the patient's gene expression. <br>
                   The title will show patient ID you entered; First box below showing age category of this patient. 
                   Other two box shows the outcome and risk score predicted by our models.
                   The clinical tips are provided as a quick advice for medical scientists.
                   Please note that risk score is scaled from 0 to 100, which indicates the rejection probability of the kidney transplant.<br>
                   If you want to upload this patient to the database, please click on the Upload Patient Data button at the top right corner.
                   After uploaded, app will be refreshed. You can then use the patient comparison function in Adult v.s. Pediatric page.")
            )
          ),
          fluidRow(
            valueBoxOutput("age_cate"),
            valueBoxOutput("risk_score"),
            valueBoxOutput("outcome")
          ),
          fluidRow(
            valueBoxOutput("CA", width = 12),
            div(
              id = "comment",
              style = "padding: 0 20px; margin-bottom: 20px;",
              tags$div(
                "* If you are not familiar with kidney transplant rejection or your research profession is not related with kidney transplant, Check this article ",
                tags$a(href="https://www.kidneyfund.org/kidney-donation-and-transplant/life-after-transplant-rejection-prevention-and-healthy-tips/kidney-rejection-after-transplant#:~:text=Rejection%20can%20damage%20your%20kidney,not%20found%20and%20treated%20early.", 
                       "Kidney rejection after transplant"),
                " for a quick overview."
              )
            )
          ),fluidRow(
            column(width = 12,
                   box(
                     id = "plot_box",
                     width = "100%",
                     h2("Boxplot - Top 5 Genes"),
                     div(
                       style = "margin-bottom: 10px;",
                       HTML(
                         "This is a boxplot of the top 5 genes based on the patient's age category, 
                         with the patients expression of these genes being overlayed as indicated by the orange diamonds."
                       )
                     ),
                     plotlyOutput("analysis_plot")
                   )
            )
          )
        )
      })
      
      new_prediction <- NULL
      new_risk_score <- NULL
      
      new_file <- read.csv(input$pfile$datapath)
      
      row_names <- new_file[, 1]
      rownames(new_file) <- row_names
      new_patient_raw <- new_file[, -1, drop = FALSE]
      
      new_patient <- t(new_patient_raw)
      new_patient_df <- as.data.frame(new_patient_raw)
      
      #-----------------#
      #   Risk score    #
      #-----------------#
      
      # title
      output$title_patientID <- renderText({
        paste("Risk Score Analysis of Patient", input$pID)
      })
      
      # Age category
      output$age_cate <- renderValueBox({
        category <- input$category
        color_class <- ifelse(category == "Adult", "blue", "yellow")
        
        valueBox(
          value = category,
          "Age category",
          icon = icon("child"),
          color = color_class,
          width = NULL  # Reset width to default
        )
      })
      
      # Distinguish age categories
      if (input$category == "Adult") {
        new_prediction <- predict(svm_fit, newdata = new_patient, probability = TRUE)
        # read rejection probabilities
        if (tolower(colnames(as.data.frame(attr(new_prediction, "probabilities")))[1]) == "rejection") {
          new_risk_score <- round(attr(new_prediction, "probabilities")[1] * 100, 2)
        }else {
          new_risk_score <- round(attr(new_prediction, "probabilities")[2] * 100, 2)
        }
      } else {
        new_prediction <- predict(svm_fit_P, newdata = new_patient, probability = TRUE)
        # read rejection probabilities
        if (tolower(colnames(as.data.frame(attr(new_prediction, "probabilities")))[1]) == "rejection") {
          new_risk_score <- round(attr(new_prediction, "probabilities")[1] * 100, 2)
        } else {
          new_risk_score <- round(attr(new_prediction, "probabilities")[2] * 100, 2)
        }
      }
      
      # Risk score
      output$risk_score <- renderValueBox({
        risk_score <- new_risk_score
        color_class <- ifelse(risk_score <= 30, "green",
                              ifelse(risk_score <= 60, "orange", "red"))
        icon_class <- ifelse(risk_score <= 30, "thumbs-up", "exclamation-triangle")
        
        valueBox(
          value = sprintf("%.2f", risk_score),
          subtitle = "Risk Score",
          icon = icon(icon_class),
          color = color_class
        )
      })
      
      # Outcome
      output$outcome <- renderValueBox({
        prediction <- new_prediction
        outcome <- prediction[1]
        color_class <- ifelse(outcome == "Stable", "green","red")
        icon_class <- ifelse(outcome == "Stable", "check","times")
        valueBox(
          value = outcome,
          "Predicted Outcome",
          color = color_class,
          icon = icon(icon_class)
        )
      })
      
      
      # Clinical advice
      output$CA <- renderValueBox({
        prediction <- new_prediction
        outcome <- prediction[1]
        if (outcome == "Stable") {
          valueBox(
            subtitle = 'This patient has a high probability of stable kidney transplantation. However, this could be a result of the patient having poor health choices or not doing regular follow-ups; so it is crucial to inform the patient of medical advice.',
            value = "Clinical Tips",
            icon = icon("check"),
            color = "green",
            width = 12
          )
        } else {
          valueBox(
            subtitle = 'This patient is likely to be rejected if he gets a kidney transplant. Please be careful, this could lead to complications like some long-term side-effects for the patient such as cardiovascular disease.',
            value = "Clinical Tips",
            icon = icon("times"),
            color = "red",
            width = 12
          )
        }
      })
      
      # boxplot
      output$analysis_plot <- renderPlotly({
        if (input$category == "Adult") {
          
          # Filter
          new_patient_filtered <- new_patient_df[genesA, ]
          subset_dataA_temp <- cbind(subset_dataA, new_patient_filtered)
          colnames(subset_dataA_temp)[colnames(subset_dataA_temp) == "new_patient_filtered"] <- input$pID
          
          dataA_temp <- reshape2::melt(subset_dataA_temp, varnames = c("Gene", "Sample"), value.name = "Expression")
          
          p <- ggplot(dataA_temp, aes(x = Gene, y = Expression)) +
            geom_boxplot() +
            labs(title = "Boxplot - Top 5 Genes in Adult Dataset",
                 x = "Genes",
                 y = "Expression") +
            theme_minimal()
          
          patient <- dataA_temp[dataA_temp$Sample==input$pID, ]
          p <- p + geom_point(data=patient, aes(x=Gene, y=Expression, fill=input$pID), size=3, shape=23) + labs(fill="Patient")
          
          p
          
        } else {
          
          new_patient_filtered <- new_patient_df[genesP, ]
          subset_dataP_temp <- cbind(subset_dataP, new_patient_filtered)
          colnames(subset_dataP_temp)[colnames(subset_dataP_temp) == "new_patient_filtered"] <- input$pID
          
          dataP_temp <- reshape2::melt(subset_dataP_temp, varnames = c("Gene", "Sample"), value.name = "Expression")
          
          p <- ggplot(dataP_temp, aes(x = Gene, y = Expression)) +
            geom_boxplot() +
            labs(title = "Boxplot - Top 5 Genes in Pediatric Dataset",
                 x = "Genes",
                 y = "Expression") +
            theme_minimal()
          
          patient <- dataP_temp[dataP_temp$Sample==input$pID,]
          p <- p + geom_point(data=patient, aes(x=Gene, y=Expression, fill=input$pID), size=3, shape=23) + labs(fill="Patient")
          
          p
        }
      })
      
      # Jump to risk score page when loading finished
      updateTabItems(session, "sidebar", "riskcalc")
      
    }
  })
  
  # Upload/Add patient
  
  observeEvent(input$upload_button, {
    
    calculating <<- TRUE
    
    if (is.null(input$pID) || input$pID == "") {
      showNotification("You have to fill in the patient ID.", type = "error")
    } else if (is.null(input$category)) {
      showNotification("You must choose an age category for the patient.", type = "error")
    } else if (is.null(input$pfile$name)) {
      showNotification("You must upload a file before calculation.", type = "error")
      #} else if (tools::file_ext(input$pfile$name) != "csv") {
      #  showNotification("Invalid file type. Please upload a CSV file.", type = "error")
    } else {
      # when all conditions are met
      
      new_prediction <- NULL
      new_risk_score <- NULL
      
      new_file <- read.csv(input$pfile$datapath)
      # new_file <- read.csv("patient_example_Adult.csv")
      # new_file <- read.csv("patient_example_Pediatric.csv")
      
      row_names <- new_file[, 1]
      rownames(new_file) <- row_names
      new_patient <- new_file[, -1, drop = FALSE]
      
      new_patient <- t(new_patient)
      
      if (!is.na(match(input$pID, as.character(database_all_patients$patient_id)))) {
        showNotification("Patient ID already exists.", type = "error")
      } else {
        #ADD new patient to df
        if (input$category == "Adult") {
          #Adult
          new_prediction <- predict(svm_fit, newdata = new_patient, probability = TRUE)
          new_risk_score <- round(attr(new_prediction, "probabilities")[1] * 100, 2)
          patient_entry <- data.frame(
            patient_id = as.character(input$pID),
            age_category = input$category,
            outcome = as.character(new_prediction[1]),
            risk_score = new_risk_score
          )
          databaseA <<- rbind(databaseA, patient_entry)
          database_all_patients <<- rbind(database_all_patients, patient_entry)
          
          
        } else {
          #Pediatric
          new_prediction <- predict(svm_fit_P, newdata = new_patient, probability = TRUE)
          new_risk_score <- round(attr(new_prediction, "probabilities")[1] * 100, 2)
          patient_entry <- data.frame(
            patient_id = as.character(input$pID),
            age_category = input$category,
            outcome = as.character(new_prediction[1]),
            risk_score = new_risk_score
          )
          
          databaseP <<- rbind(databaseP, patient_entry)
          database_all_patients <<- rbind(database_all_patients, patient_entry)
          
        }
        
        # refresh table
        session$reload()
        
      }
    }
  })
  
  # refresh selectinput
  observe({
    updateSelectInput(
      session,
      "analysis_adult_id",
      choices = c("All", databaseA$patient_id)
    )
    
    updateSelectInput(
      session,
      "analysis_pediatric_id",
      choices = c("All", databaseP$patient_id)
    )
  })
  
  #-----------------#
  # Analysis Output #
  #-----------------#
  output$analysis_plot_p <- renderPlotly({
    p <- ggplot(dataP, aes(x = Gene, y = Expression)) +
      geom_boxplot() +
      labs(title = "Boxplot - Top 5 Genes in Pediatric Dataset",
           x = "Genes",
           y = "Expression") +
      theme_minimal()
    
    if (input$analysis_pediatric_id == "All") {
      p 
    } else {
      pediatric_patient <- dataP[dataP$Sample==input$analysis_pediatric_id,]
      
      p + geom_point(data=pediatric_patient, aes(x=Gene, y=Expression, fill=input$analysis_pediatric_id), size=3, shape=23) + labs(fill="Patient")
      
    }
  })
  
  output$analysis_plot_a <- renderPlotly({
    p <- ggplot(dataA, aes(x = Gene, y = Expression)) +
      geom_boxplot() +
      labs(title = "Boxplot - Top 5 Genes in Adult Dataset",
           x = "Genes",
           y = "Expression") +
      theme_minimal()
    
    if (input$analysis_adult_id == "All") {
      p 
    } else{
      adult_patient <- dataA[dataA$Sample==input$analysis_adult_id,]
      
      p + geom_point(data=adult_patient, aes(x=Gene, y=Expression, fill=input$analysis_adult_id), size=3, shape=23) + labs(fill="Patient")
      
    }
  })
  
  # compare outcome
  output$pediatric_outcome <- renderValueBox({
    if (input$analysis_pediatric_id == "All") {
      valueBox("N/A",
               "Pediatric Outcome")
    }else {
      outcome <- databaseP$outcome[databaseP$patient_id == input$analysis_pediatric_id]
      icon_class <- ifelse(outcome == "Stable", "check","times")
      color_class <- ifelse(outcome == "Stable", "green","red")
      valueBox(outcome,
               "Pediatric Outcome",
               color = color_class,
               icon = icon(icon_class)
      )
    }
  })
  
  output$adult_outcome <- renderValueBox({
    if (input$analysis_adult_id == "All") {
      valueBox("N/A",
               "Adult Outcome")
    }else {
      outcome <- databaseA$outcome[databaseA$patient_id == input$analysis_adult_id]
      icon_class <- ifelse(outcome == "Stable", "check","times")
      color_class <- ifelse(outcome == "Stable", "green","red")
      valueBox(outcome, 
               "Adult Outcome",
               color = color_class,
               icon = icon(icon_class)
      )
    }
  })
  
  
  # compare risk score
  output$pediatric_score <- renderValueBox({
    if (input$analysis_pediatric_id == "All") {
      valueBox(
        value = round((sum(databaseP$risk_score) / dim(databaseP)[1]), 2),
        subtitle = "Average Risk Score of Pediatric patients",
        color = "yellow",
        icon = icon("child")
      )
    }else {
      riskscore <- databaseP$risk_score[databaseP$patient_id == input$analysis_pediatric_id]
      valueBox(riskscore,
               "Pediatric Risk Score",
               color = "yellow",
               icon = icon("child")
      )
    }
  })
  
  output$adult_score <- renderValueBox({
    if (input$analysis_adult_id == "All") {
      valueBox(
        value = round((sum(databaseA$risk_score) / dim(databaseA)[1]), 2),
        subtitle = "Average Risk Score of Adult patients",
        color = "blue",
        icon = icon("user")
      )
    }else {
      riskscore <- databaseA$risk_score[databaseA$patient_id == input$analysis_adult_id]
      valueBox(riskscore, 
               "Adult Risk Score",
               color = "blue",
               icon = icon("user")
      )
    }
  })
  
  
  #-----------------#
  #  Database Page  #
  #-----------------#
  
  
  output$databaseA_table <- renderDataTable({
    databaseA
  })
  
  output$databaseP_table <- renderDataTable({
    databaseP
  })
  
  
}