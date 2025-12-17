# PicoGreen Assay Analysis Shiny Application
# Save this as app.R and run with shiny::runApp()

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(plotly)
library(openxlsx)

# Function to parse a single PicoGreen file
parse_picogreen_file <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Extract metadata
  software_version <- str_extract(lines[grep("Software Version", lines)], "\\d+\\.\\d+\\.\\d+")
  plate_number <- str_extract(lines[grep("Plate Number", lines)], "Plate \\d+")
  date_time <- paste(
    str_extract(lines[grep("^Date", lines)], "\\d{2}/\\d{2}/\\d{4}"),
    str_extract(lines[grep("^Time", lines)], "\\d{2}:\\d{2}:\\d{2}")
  )
  
  # Find the results section
  results_start <- which(str_detect(lines, "^Results"))
  if (length(results_start) == 0) {
    results_start <- which(str_detect(lines, "^\\s*1\\s+2\\s+3"))
  }
  
  # Parse the 96-well plate data
  plate_data <- data.frame()
  row_letters <- LETTERS[1:8]
  
  for (i in seq_along(row_letters)) {
    row_line_idx <- results_start + i
    if (row_line_idx <= length(lines)) {
      row_data <- str_split(str_trim(lines[row_line_idx]), "\\s+")[[1]]
      if (length(row_data) >= 13) {
        row_letter <- row_data[1]
        values <- as.numeric(row_data[2:13])
        
        for (j in 1:12) {
          plate_data <- rbind(plate_data, data.frame(
            File = basename(file_path),
            Plate = ifelse(is.na(plate_number), "Plate 1", plate_number),
            DateTime = date_time,
            Row = row_letter,
            Column = j,
            Well = paste0(row_letter, j),
            Fluorescence = values[j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(plate_data)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "PicoGreen Assay Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Import", tabName = "import", icon = icon("upload")),
      menuItem("Well Mapping", tabName = "mapping", icon = icon("table")),
      menuItem("Standard Curve", tabName = "curve", icon = icon("chart-line")),
      menuItem("Results", tabName = "results", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # File Import Tab
      tabItem(tabName = "import",
        fluidRow(
          box(
            title = "File Upload", status = "primary", solidHeader = TRUE, width = 4,
            fileInput("files", "Choose PicoGreen Files",
                      multiple = TRUE,
                      accept = c(".txt", ".xpt")),
            br(),
            actionButton("load_files", "Load Files", class = "btn-primary btn-block"),
            br(),
            downloadButton("download_parsed", "Download Parsed Data", class = "btn-info btn-block")
          ),
          box(
            title = "File Status", status = "info", solidHeader = TRUE, width = 8,
            verbatimTextOutput("file_status")
          )
        ),
        
        fluidRow(
          box(
            title = "Parsed Data", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("parsed_data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Plate Heatmap", status = "warning", solidHeader = TRUE, width = 12,
            plotlyOutput("plate_heatmap", height = "400px")
          )
        )
      ),
      
      # Well Mapping Tab
      tabItem(tabName = "mapping",
        fluidRow(
          box(
            title = "Dilution Settings", status = "primary", solidHeader = TRUE, width = 4,
            numericInput("sample_volume", "Sample Volume (μL):", value = 1, min = 0.1, max = 100, step = 0.1),
            numericInput("buffer_volume", "Buffer Volume (μL):", value = 99, min = 0, max = 1000),
            numericInput("reagent_volume", "PicoGreen Volume (μL):", value = 100, min = 0, max = 1000),
            br(),
            h4("Total Volume:"),
            textOutput("total_volume"),
            br(),
            h4("Dilution Factor:"),
            textOutput("dilution_factor_display")
          ),
          
          box(
            title = "Mapping Actions", status = "info", solidHeader = TRUE, width = 4,
            fileInput("mapping_file", "Import Mapping (.xlsx)", accept = ".xlsx"),
            actionButton("load_mapping", "Load Mapping", class = "btn-info btn-block"),
            br(),
            downloadButton("download_mapping", "Export Mapping", class = "btn-success btn-block"),
            br(),
            actionButton("reset_mapping", "Reset All Mappings", class = "btn-warning btn-block")
          ),
          
          box(
            title = "Quick Edit", status = "warning", solidHeader = TRUE, width = 4,
            selectInput("quick_type", "Well Type:", choices = c("Sample", "Standard", "Empty")),
            conditionalPanel(
              condition = "input.quick_type == 'Standard'",
              numericInput("standard_conc", "Concentration (ng/μL):", value = 0, min = 0, step = 0.1)
            ),
            br(),
            actionButton("apply_quick", "Apply to Selected Wells", class = "btn-primary btn-block"),
            br(),
            p("Select rows in the table below, then click apply.", class = "text-muted")
          )
        ),
        
        fluidRow(
          box(
            title = "Well Mapping Table", status = "success", solidHeader = TRUE, width = 12,
            p("Click on rows to select, then use Quick Edit above. You can also edit cells directly."),
            DT::dataTableOutput("mapping_table")
          )
        )
      ),
      
      # Standard Curve Tab
      tabItem(tabName = "curve",
        fluidRow(
          box(
            title = "Curve Settings", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("curve_type", "Curve Type:", 
                        choices = c("Linear" = "linear", 
                                   "Polynomial (2nd order)" = "poly2",
                                   "Polynomial (3rd order)" = "poly3")),
            br(),
            actionButton("generate_curve", "Generate Standard Curve", class = "btn-primary btn-block"),
            br(),
            h4("Curve Statistics"),
            verbatimTextOutput("curve_stats")
          ),
          
          box(
            title = "Standards Data", status = "info", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("standards_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Standard Curve Plot", status = "success", solidHeader = TRUE, width = 12,
            plotlyOutput("standard_curve_plot", height = "500px")
          )
        )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Calculation Settings", status = "primary", solidHeader = TRUE, width = 4,
            numericInput("detection_limit", "Detection Limit (fluorescence):", 
                         value = 100, min = 0),
            br(),
            actionButton("calculate_concs", "Calculate Concentrations", class = "btn-primary btn-block"),
            br(),
            downloadButton("download_results", "Download Results", class = "btn-success btn-block")
          ),
          
          box(
            title = "Summary Statistics", status = "info", solidHeader = TRUE, width = 8,
            verbatimTextOutput("summary_stats")
          )
        ),
        
        fluidRow(
          box(
            title = "Sample Concentrations", status = "success", solidHeader = TRUE, width = 12,
            p("Concentrations are corrected for dilution factor."),
            DT::dataTableOutput("results_table")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    parsed_data = NULL,
    well_mappings = NULL,
    standards_data = NULL,
    curve_model = NULL,
    r_squared = NULL,
    final_results = NULL
  )
  
  # File processing ----
  observeEvent(input$load_files, {
    req(input$files)
    
    all_data <- data.frame()
    
    withProgress(message = 'Loading files...', value = 0, {
      for (i in 1:nrow(input$files)) {
        incProgress(1/nrow(input$files), detail = paste("Processing file", i))
        file_data <- parse_picogreen_file(input$files$datapath[i])
        all_data <- rbind(all_data, file_data)
      }
    })
    
    values$parsed_data <- all_data
    showNotification("Files loaded successfully!", type = "message")
  })
  
  # File status output
  output$file_status <- renderText({
    if (is.null(input$files)) {
      "No files selected"
    } else if (is.null(values$parsed_data)) {
      paste("Files selected:", nrow(input$files), "- Click 'Load Files' to process")
    } else {
      paste("Files processed:", nrow(input$files), 
            "\nTotal wells:", nrow(values$parsed_data),
            "\nPlates:", length(unique(values$parsed_data$Plate)))
    }
  })
  
  # Display parsed data
  output$parsed_data_table <- DT::renderDataTable({
    req(values$parsed_data)
    DT::datatable(values$parsed_data, 
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Create plate heatmap
  output$plate_heatmap <- renderPlotly({
    req(values$parsed_data)
    
    # Take first file/plate for visualization
    plot_data <- values$parsed_data %>%
      filter(File == first(File)) %>%
      mutate(
        Row_num = match(Row, LETTERS),
        Column_num = Column
      )
    
    p <- ggplot(plot_data, aes(x = Column_num, y = Row_num, fill = Fluorescence, 
                               text = paste("Well:", Well, "<br>Fluorescence:", Fluorescence))) +
      geom_tile() +
      scale_fill_viridis_c(name = "Fluorescence") +
      scale_x_continuous(breaks = 1:12, labels = 1:12) +
      scale_y_continuous(breaks = 1:8, labels = rev(LETTERS[1:8])) +
      labs(title = "96-Well Plate Fluorescence Heatmap", x = "Column", y = "Row") +
      theme_minimal() +
      coord_fixed()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Download parsed data
  output$download_parsed <- downloadHandler(
    filename = function() {
      paste("picogreen_parsed_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$parsed_data)
      openxlsx::write.xlsx(values$parsed_data, file)
    }
  )
  
  # Well mapping ----
  
  # Initialize mapping data when parsed data is available
  observe({
    req(values$parsed_data)
    if (is.null(values$well_mappings)) {
      unique_wells <- values$parsed_data %>%
        select(File, Plate, Well) %>%
        distinct() %>%
        mutate(
          Type = "Sample",
          Concentration_ng_uL = NA,
          Sample_ID = paste("Sample", row_number())
        )
      
      values$well_mappings <- unique_wells
    }
  })
  
  # Calculate total volume and dilution factor
  output$total_volume <- renderText({
    total <- input$sample_volume + input$buffer_volume + input$reagent_volume
    paste(total, "μL")
  })
  
  output$dilution_factor_display <- renderText({
    total <- input$sample_volume + input$buffer_volume + input$reagent_volume
    dilution_factor <- total / input$sample_volume
    paste(round(dilution_factor, 1), "x")
  })
  
  # Display mapping table
  output$mapping_table <- DT::renderDataTable({
    req(values$well_mappings)
    DT::datatable(values$well_mappings, 
                  selection = "multiple",
                  editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Handle cell edits
  observeEvent(input$mapping_table_cell_edit, {
    info <- input$mapping_table_cell_edit
    i <- info$row
    j <- info$col + 1  # DT uses 0-based indexing
    v <- info$value
    
    req(values$well_mappings)
    values$well_mappings[i, j] <- v
    
    # If changing to Standard, ensure concentration is numeric
    if (j == 4 && values$well_mappings$Type[i] == "Standard") {
      values$well_mappings$Concentration_ng_uL[i] <- as.numeric(v)
    }
    
    # If changing type away from Standard, clear concentration
    if (j == 4 && values$well_mappings$Type[i] != "Standard") {
      values$well_mappings$Concentration_ng_uL[i] <- NA
    }
  })
  
  # Apply quick actions to selected wells
  observeEvent(input$apply_quick, {
    req(input$mapping_table_rows_selected, values$well_mappings)
    
    selected_rows <- input$mapping_table_rows_selected
    
    values$well_mappings$Type[selected_rows] <- input$quick_type
    
    if (input$quick_type == "Standard") {
      values$well_mappings$Concentration_ng_uL[selected_rows] <- input$standard_conc
    } else {
      values$well_mappings$Concentration_ng_uL[selected_rows] <- NA
    }
    
    showNotification(paste("Updated", length(selected_rows), "wells"), type = "message")
  })
  
  # Load mapping from file
  observeEvent(input$load_mapping, {
    req(input$mapping_file)
    
    tryCatch({
      imported_mapping <- openxlsx::read.xlsx(input$mapping_file$datapath)
      values$well_mappings <- imported_mapping
      showNotification("Mapping loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error loading mapping file", type = "error")
    })
  })
  
  # Download mapping
  output$download_mapping <- downloadHandler(
    filename = function() {
      paste("well_mapping_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$well_mappings)
      openxlsx::write.xlsx(values$well_mappings, file)
    }
  )
  
  # Reset mappings
  observeEvent(input$reset_mapping, {
    req(values$parsed_data)
    
    unique_wells <- values$parsed_data %>%
      select(File, Plate, Well) %>%
      distinct() %>%
      mutate(
        Type = "Sample",
        Concentration_ng_uL = NA,
        Sample_ID = paste("Sample", row_number())
      )
    
    values$well_mappings <- unique_wells
    showNotification("Mappings reset", type = "warning")
  })
  
  # Standard curve ----
  
  # Generate standard curve
  observeEvent(input$generate_curve, {
    req(values$parsed_data, values$well_mappings)
    
    # Merge data with mappings
    merged_data <- values$parsed_data %>%
      left_join(values$well_mappings, by = c("File", "Plate", "Well"))
    
    # Filter for standards
    standards <- merged_data %>%
      filter(Type == "Standard" & !is.na(Concentration_ng_uL)) %>%
      group_by(Concentration_ng_uL) %>%
      summarise(
        Mean_Fluorescence = mean(Fluorescence, na.rm = TRUE),
        SD_Fluorescence = sd(Fluorescence, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>%
      mutate(SE_Fluorescence = SD_Fluorescence / sqrt(N))
    
    values$standards_data <- standards
    
    if (nrow(standards) >= 2) {
      # Fit curve based on selection
      if (input$curve_type == "linear") {
        model <- lm(Mean_Fluorescence ~ Concentration_ng_uL, data = standards)
      } else if (input$curve_type == "poly2") {
        model <- lm(Mean_Fluorescence ~ poly(Concentration_ng_uL, 2), data = standards)
      } else if (input$curve_type == "poly3") {
        model <- lm(Mean_Fluorescence ~ poly(Concentration_ng_uL, 3), data = standards)
      }
      
      values$curve_model <- model
      values$r_squared <- summary(model)$r.squared
      
      showNotification("Standard curve generated!", type = "message")
    } else {
      showNotification("Need at least 2 standard concentrations", type = "error")
    }
  })
  
  # Display curve statistics
  output$curve_stats <- renderText({
    if (!is.null(values$curve_model)) {
      paste(
        "R-squared:", round(values$r_squared, 4), "\n",
        "Curve Type:", input$curve_type, "\n",
        "Standard Points:", nrow(values$standards_data)
      )
    } else {
      "Generate curve to see statistics"
    }
  })
  
  # Plot standard curve
  output$standard_curve_plot <- renderPlotly({
    req(values$standards_data)
    
    p <- ggplot(values$standards_data, aes(x = Concentration_ng_uL, y = Mean_Fluorescence)) +
      geom_point(size = 3, color = "blue") +
      geom_errorbar(aes(ymin = Mean_Fluorescence - SE_Fluorescence, 
                       ymax = Mean_Fluorescence + SE_Fluorescence), 
                   width = 0.1, color = "blue") +
      labs(x = "Concentration (ng/μL)", y = "Fluorescence", 
           title = "Standard Curve") +
      theme_minimal()
    
    # Add fitted curve if model exists
    if (!is.null(values$curve_model)) {
      conc_range <- seq(min(values$standards_data$Concentration_ng_uL), 
                       max(values$standards_data$Concentration_ng_uL), 
                       length.out = 100)
      
      pred_data <- data.frame(Concentration_ng_uL = conc_range)
      pred_fluor <- predict(values$curve_model, pred_data)
      curve_data <- data.frame(Concentration_ng_uL = conc_range, Fluorescence = pred_fluor)
      
      p <- p + geom_line(data = curve_data, aes(x = Concentration_ng_uL, y = Fluorescence), 
                        color = "red", size = 1)
    }
    
    ggplotly(p)
  })
  
  # Display standards table
  output$standards_table <- DT::renderDataTable({
    req(values$standards_data)
    DT::datatable(values$standards_data, options = list(pageLength = 10)) %>%
      formatRound(c("Mean_Fluorescence", "SD_Fluorescence", "SE_Fluorescence"), 2)
  })
  
  # Results ----
  
  # Calculate sample concentrations
  observeEvent(input$calculate_concs, {
    req(values$curve_model, values$parsed_data, values$well_mappings)
    
    # Merge data with mappings
    merged_data <- values$parsed_data %>%
      left_join(values$well_mappings, by = c("File", "Plate", "Well"))
    
    # Filter for samples
    samples <- merged_data %>%
      filter(Type == "Sample")
    
    # Predict concentrations using the standard curve
    if (input$curve_type == "linear") {
      # For linear: solve y = mx + b for x
      coeffs <- coef(values$curve_model)
      samples$Conc_in_well <- (samples$Fluorescence - coeffs[1]) / coeffs[2]
    } else {
      # For polynomial curves, we need to solve numerically
      # This is a simplified approach - you might need more sophisticated methods
      samples$Conc_in_well <- sapply(samples$Fluorescence, function(fluor) {
        # Find concentration that gives this fluorescence
        objective <- function(conc) {
          pred_data <- data.frame(Concentration_ng_uL = conc)
          pred_fluor <- predict(values$curve_model, pred_data)
          (pred_fluor - fluor)^2
        }
        
        # Search within the range of standards
        conc_range <- range(values$standards_data$Concentration_ng_uL)
        optimize(objective, conc_range)$minimum
      })
    }
    
    # Calculate actual sample concentration accounting for dilution
    total_volume <- input$sample_volume + input$buffer_volume + input$reagent_volume
    dilution_factor <- total_volume / input$sample_volume
    
    samples <- samples %>%
      mutate(
        Conc_in_well = pmax(0, Conc_in_well),  # No negative concentrations
        Original_Concentration_ng_uL = Conc_in_well * dilution_factor,
        Below_Detection_Limit = Fluorescence < input$detection_limit,
        QC_Flag = case_when(
          Below_Detection_Limit ~ "Below Detection Limit",
          Fluorescence > max(values$standards_data$Mean_Fluorescence, na.rm = TRUE) ~ "Above Curve Range",
          TRUE ~ "OK"
        )
      ) %>%
      select(File, Plate, Well, Sample_ID, Fluorescence, Conc_in_well, 
             Original_Concentration_ng_uL, QC_Flag)
    
    values$final_results <- samples
    showNotification("Concentrations calculated!", type = "message")
  })
  
  # Display results table
  output$results_table <- DT::renderDataTable({
    req(values$final_results)
    DT::datatable(values$final_results, 
                  options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(c("Conc_in_well", "Original_Concentration_ng_uL"), 2)
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    req(values$final_results)
    
    results <- values$final_results
    ok_results <- results %>% filter(QC_Flag == "OK")
    
    if (nrow(ok_results) > 0) {
      paste(
        "Total Samples:", nrow(results), "\n",
        "Samples with QC = OK:", nrow(ok_results), "\n",
        "Below Detection Limit:", sum(results$QC_Flag == "Below Detection Limit"), "\n",
        "Above Curve Range:", sum(results$QC_Flag == "Above Curve Range"), "\n",
        "Mean Concentration (OK samples):", round(mean(ok_results$Original_Concentration_ng_uL, na.rm = TRUE), 2), "ng/μL\n",
        "Median Concentration (OK samples):", round(median(ok_results$Original_Concentration_ng_uL, na.rm = TRUE), 2), "ng/μL\n",
        "Range:", round(min(ok_results$Original_Concentration_ng_uL, na.rm = TRUE), 2), "-", 
        round(max(ok_results$Original_Concentration_ng_uL, na.rm = TRUE), 2), "ng/μL"
      )
    } else {
      "No samples with OK QC status"
    }
  })
  
  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste("picogreen_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$final_results)
      
      # Create workbook with multiple sheets
      wb <- openxlsx::createWorkbook()
      
      # Results sheet
      openxlsx::addWorksheet(wb, "Results")
      openxlsx::writeData(wb, "Results", values$final_results)
      
      # Standards sheet
      if (!is.null(values$standards_data)) {
        openxlsx::addWorksheet(wb, "Standards")
        openxlsx::writeData(wb, "Standards", values$standards_data)
      }
      
      # Settings sheet
      total_volume <- input$sample_volume + input$buffer_volume + input$reagent_volume
      dilution_factor <- total_volume / input$sample_volume
      
      settings <- data.frame(
        Parameter = c("Sample Volume (μL)", "Buffer Volume (μL)", 
                     "PicoGreen Volume (μL)", "Total Volume (μL)",
                     "Dilution Factor", "Curve Type", "R-squared"),
        Value = c(input$sample_volume, input$buffer_volume, 
                 input$reagent_volume, total_volume,
                 round(dilution_factor, 1),
                 input$curve_type, 
                 ifelse(!is.null(values$r_squared), 
                       round(values$r_squared, 4), "N/A"))
      )
      openxlsx::addWorksheet(wb, "Settings")
      openxlsx::writeData(wb, "Settings", settings)
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)