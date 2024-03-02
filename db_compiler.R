# Install required packages if not already installed
# install.packages(c("shiny", "shinydashboard", "shinyFiles", "readxl", "RSQLite", "dpylr", "shinyalert", "shinyjs", "lubridate"))


library(shiny)
library(shinydashboard)
library(shinyFiles)
library(readxl)
library(RSQLite)
library(dplyr)
library(shinyalert)
library(shinyjs)
library(lubridate)

# BTS colourscheme for later
navy <- "#173e61"
grey <- "#cad1da"
green <- "#7fbf44"
blue <- "#0c4c94"

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Landauer Database Creator"),
  dashboardSidebar(
    sidebarMenu(
      HTML('<div style="white-space: normal; margin-left: 10px;">Manually Search for the Directory containing the Landauer Excel files using the button below.</div>'),
      shinyDirButton("folderBtn", "Search Landauer File Directory", "Please select a folder", callback = "folderCallback"),
      
      # New file input for .db files
      HTML('<div style="white-space: normal; margin-left: 10px;">If the database already exists, the file can be searched via directory using button below.</div>'),
      HTML('<div style="white-space: normal; margin-left: 20px;">--IMPORTANT--</div>'),
      HTML('<div style="white-space: normal; margin-left: 10px;">- Make sure to have a good time. =)</div>'),
      fileInput("dbFile", "Select .db File", accept = ".db"),
      
      HTML('<div style="white-space: normal; margin-left: 10px;">Manually Choose the Directory for saving the database using the button below.</div>'),
      shinyDirButton("saveLocationBtn", "Choose Save Location", "Please select a folder for saving", callback = "saveLocationCallback"),
      
      menuItem("BTS SDMS", tabName = "upload")
    )
  ),
  dashboardBody(
    tabItems(
      # Upload Data tab
      tabItem(
        tabName = "upload",
        h2("Landauer Data Uploader"),
        t("This compiler is designed to compile Landauer Excel files only. .csv or other filetypes will not be recognised."),
        textInput("folder", tags$label("Enter Directory Path Containing Excel Files", class = "my-custom-label"), 
                  placeholder = "C:/Your/Landauer/Excel/Directory"),
        t("If compiling multiple folders with Landauer files, choose the parent directory and the compiler will search subsequent daughter folders."),
        textInput("dbname", tags$label("Enter Database Name", class = "my-custom-label"), placeholder = "e.g. PAH_SDMS"),
        t("Choosing an exisiting file name will update the exisiting database with the new files.  It will replace any duplicates with the most recent upload.  i.e. period, staff member and badge if matched will be overwritten with new upload."),
        textInput("save_location", tags$label("Enter Save Location", class = "my-custom-label"), placeholder = "C:/Your/Database/Directory"),
        actionButton("uploadBtn", "Upload and Create Database", class = "my-custom-label"),
        # Display selected folder path
        shinyjs::useShinyjs(),
        tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/shinyjs/2.0.0/shinyjs.min.js")),
        tags$script('
          $(document).on("shiny:connected", function() {
            Shiny.setInputValue("resetFolderDialog", true);
          });

          Shiny.addCustomMessageHandler("resetFolderDialog", function(message) {
            shinyjs.modalDialog({
              title: "Selected Folder",
              message: message,
              size: "m"
            });
          });
        '),
        tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('fileError', function(message) {
        shinyjs.modalDialog({
          title: 'Error Reading File',
          message: 'An error occurred while reading the file:<br>' + message.file + '<br><br>' + message.error,
          size: 'm'
        });
      });
    });
  ")),
        tags$style(HTML("
  .my-custom-label {
    font-size: 16px; /* You can adjust the size as needed */
    font-weight: bold; /* You can add other styles as needed */
    /* Add any other styles you want */
  }
")),
        tags$script(HTML('
          Shiny.addCustomMessageHandler("resetPlaceholder", function(id, placeholder) {
            var input = $("#" + id);
            input.attr("placeholder", placeholder);
            input.css("color", "#999999");
          });

          Shiny.addCustomMessageHandler("folderCallback", function(path) {
            Shiny.setInputValue("resetFolderDialog", path);
          });
        '))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  volumes <- getVolumes()()
  shinyDirChoose(input, "folderBtn", roots = volumes, session = session)
  shinyDirChoose(input, "saveLocationBtn", roots = volumes, session = session)
  
  failed_files <- reactiveVal(character())
  
  
  #set maximum file size using the upload option
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  observeEvent(input$folderBtn, {
    # Your folder selection logic goes here
    folder_path <- parseDirPath(volumes, input$folderBtn)
    print(paste("Selected folder path:", folder_path))
    
    # Update the text box with the selected folder path
    updateTextInput(session, "folder", value = folder_path)
    
    # Enable the text input
    shinyjs::enable("folder")
  })
  
  observeEvent(input$dbFile, {
    req(input$dbFile)
    
    if (!is.null(input$dbFile$name) && input$dbFile$name != "") {
      # the .db file handling logic 
      file_path <- input$dbFile$datapath
      
      # Update the "Enter Database Name" text box with the selected file name
      updateTextInput(session, "dbname", value = tools::file_path_sans_ext(basename(file_path)))
      
      # Enable the text input
      shinyjs::enable("dbname")
      
      # Show a message that an existing database file will be used as a base
      shinyalert::shinyalert(
        title = "Existing Database Selected",
        text = paste("An existing database file has been selected. It will be used as a foundation for the new database."),
        type = "info"
      )
      
      # Store the path of the existing database file for later use
      session$userData$existingDbPath <- file_path
    }
  })
  
  observeEvent(input$saveLocationBtn, {
    # Your save location selection logic goes here
    save_location_path <- parseDirPath(volumes, input$saveLocationBtn)
    print(paste("Selected save location path:", save_location_path))
    
    # Update the text box with the selected save location path
    updateTextInput(session, "save_location", value = save_location_path)
    
    # Enable the text input
    shinyjs::enable("save_location")
  })
  
  observeEvent(input$uploadBtn, {
    req(input$folder, input$dbname, input$save_location)
    
    # Get a list of all Excel files in the specified folder and its subdirectories
    folder_path <- input$folder
    excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
    
    # Initialize an empty list to store data from valid files
    data <- list()
    for (file in excel_files) {
      tryCatch(
        {
          # Attempt to read the Excel file
          file_data <- read_excel(file, col_names = FALSE, skip = 1) %>%
            select(10, 1,3, 7, 13, 15, 16, 24:29, 17:19, 20) %>%
            distinct()
          
          # Append the data to the list
          data <- c(data, list(file_data))
        },
        error = function(e) {
          # Handle the error (file cannot be opened)
          runjs(sprintf("shinyjs.fileError('%s', '%s')", file, e$message))
          failed_files(c(failed_files(), file))
        }
      )
    }
    
    # Display a message with files that could not be added to the database
    if (length(failed_files()) > 0) {
      shinyalert::shinyalert(
        title = "Error Reading Files",
        text = paste("The following files could not be added to the database, Please take note of these files and check they are not open or in use and reattach to the database:", paste(failed_files(), collapse = ", ")),
        type = "error"
      )
    }
    
    # Reset the list of failed files
    failed_files(character())
    
    # Define column names
    column_names <- c("Staff_Member","Account_Number","Series_Code","Participant_Number", "Badge_Position",
                      "Period_Begin_Date", "Period_End_Date","YTD_Effective_Doseective_Dose_DDE_mSv", "YTD_Collar_Dose_LDE_mSv", "YTD_SDE"," Lifetime_DDE_mSv", 
                      "Lifetime_LDE", "Life_SDE", "Current DDE", "Current LDE", 
                      "Current SDE", "Nuetron","Department","Period")
    
    # Create an SQLite connection with the specified database name and save location
    db_name <- paste(input$save_location, "/", input$dbname, ".db", sep = "")
    conn <- dbConnect(SQLite(), db_name)
    
    # Use the existing database as a base if it has been selected
    if (!is.null(session$userData$existingDbPath)) {
      # Read the existing database into a temporary table
      existing_db_conn <- dbConnect(SQLite(), session$userData$existingDbPath)
      existing_table_names <- dbListTables(existing_db_conn)
      
      for (table_name in existing_table_names) {
        # Read the data from the existing table
        existing_data <- dbReadTable(existing_db_conn, table_name)
        
        # Append the data to the corresponding table in the new database
        if (dbExistsTable(conn, table_name)) {
          dbWriteTable(conn, table_name, existing_data, append = TRUE, row.names = FALSE)
        } else {
          dbWriteTable(conn, table_name, existing_data, append = FALSE, row.names = FALSE)
        }
      }
      
      # Close the connection to the existing database
      dbDisconnect(existing_db_conn)
      
      # Display a success message about using the existing database as a base
      shinyalert::shinyalert(
        title = "Database Base Created",
        text = paste("An existing database file has been used as a base. You can now add new data to it."),
        type = "success"
      )
      
      # Reset the existing database path in the user data
      session$userData$existingDbPath <- NULL
    }
    
    # Split data into separate data frames based on the first 4 characters in column 15
    split_data <- lapply(data, function(df) {
      # Extract year and month from column 15 (Period Begin Date)
      year_month <- format(as.Date(df[[7]]), "%Y %B")  # Format as "YYYY Month"
      
      # Split the data based on the extracted "Year Month" value
      split(df, year_month)
    })
    
    # Split data into separate data frames based on the first 4 characters in column 15 (alternative split)
    split_data_alternative <- lapply(data, function(df) {
      split(df, substr(df[[7]], 1, 4))  # Split based on the first 4 characters in column 15
    })
    
    
    
   
    # Define a function to process the data and update the database
    process_data <- function(data, conn, column_names) {
      # Iterate through data and create or amend tables for each year
      for (i in seq_along(data)) {
        for (key in names(data[[i]])) {
          table_name <- paste(key, sep = "_")
          
         
          
          # Create new table with the updated column names
          new_table <- data.frame(data[[i]][[key]])
          colnames(new_table) <- c("Staff_Member", "Account_Number", "Series_Code", "Participant_Number", "Badge_Position",
                                   "Period_Begin_Date", "Period_End_Date", "YTD_Effective_Doseective_Dose_DDE_mSv", "YTD_Collar_Dose_LDE_mSv",
                                   "YTD_SDE", "Lifetime_DDE_mSv", "Lifetime_LDE", "Life_SDE", "Current DDE", "Current LDE",
                                   "Current SDE", "Nuetron")
          
          
        
        # Add a new column "Department" based on the Account_Number values using the provided key
        new_table$Department <- case_when(
          new_table$'Account_Number' %in% c(731246, 204383) ~ "Nuclear Medicine",
          new_table$'Account_Number' %in% c(204749) ~ "Pathology",
          new_table$'Account_Number' %in% c(731358, 204777) ~ "Cardiology",
          new_table$'Account_Number' %in% c(731360, 205184) ~ "Gastroenterology",
          new_table$'Account_Number' %in% c(731370, 205185) ~ "BTS Physicists/Technicians",
          new_table$'Account_Number' %in% c(731371, 205186) ~ "Operating Theatres",
          new_table$'Account_Number' %in% c(731387, 205187) ~ "Radiology",
          new_table$'Account_Number' %in% c(731368, 205188) ~ "Vascular",
          new_table$'Account_Number' %in% c(719835,719840,719841,719842) ~ "Oral Health",
          TRUE ~ NA_character_ )
          
          # Add a new column "Hospital" based on the Account_Number values using the provided key
          new_table$Hospital <- case_when(
            new_table$'Account_Number' %in% c(731246, 204383,204749,731358, 204777,731360, 205184,731370, 205185,731371, 205186,731387, 205187,731368, 205188) ~ "PAH",
            new_table$'Account_Number' %in% c(719835) ~ "Logan Central",
            new_table$'Account_Number' %in% c(719840) ~ "Woolloongabba",
            new_table$'Account_Number' %in% c(719841) ~ "Kingston",
            new_table$'Account_Number' %in% c(719842) ~ "Wynnum",
            TRUE ~ NA_character_)
         
          
        
        # Add new temporary columns based on Badge_Position
        new_table$Chest_temp <- ifelse(new_table$'Badge_Position' == 'CHEST', new_table$`Current DDE`, NA)
        new_table$Waist_temp <- ifelse(new_table$'Badge_Position' == 'WAIST', new_table$`Current DDE`, NA)
        new_table$'Collar_LDE_temp' <- ifelse(new_table$'Badge_Position' == 'COLLAR', new_table$`Current LDE`, NA)
        new_table$'Collar_DDE_temp' <- ifelse(new_table$'Badge_Position' == 'COLLAR', new_table$`Current DDE`, NA)
        new_table$Fetal_temp <- ifelse(new_table$'Badge_Position' == 'FETAL', new_table$`Current LDE`, NA)
        new_table$'Assign_Landauer_temp' <- ifelse(new_table$'Badge_Position' == 'Assign', new_table$`Current DDE`, NA)
        new_table$Effective_Dose_temp <- ifelse(new_table$'Badge_Position' == 'CHEST', new_table$`Current DDE`, NA)
        # Convert M to 0 for permanent columns
        new_table$Chest <- as.numeric(ifelse(new_table$Chest_temp == 'M', 0, as.character(new_table$Chest_temp)))
        new_table$Waist <- as.numeric(ifelse(new_table$Waist_temp == 'M', 0, as.character(new_table$Waist_temp)))
        new_table$Collar_Calc <- as.numeric(ifelse(new_table$`Collar_DDE_temp` == 'M', 0, as.character(new_table$`Collar_DDE_temp`)))
        new_table$Collar_LDE <- as.numeric(ifelse(new_table$`Collar_LDE_temp` == 'M', 0, as.character(new_table$`Collar_LDE_temp`)))
        new_table$Fetal <- as.numeric(ifelse(new_table$Fetal_temp == 'M', 0, as.character(new_table$Fetal_temp)))
        new_table$Assign_Landauer <- as.numeric(ifelse(new_table$Assign_Landauer_temp == 'M', 0, as.character(new_table$Assign_Landauer_temp)))
        new_table$Effective_Dose <- as.numeric(ifelse(new_table$Effective_Dose_temp == 'M', 0, as.character(new_table$Effective_Dose_temp)))
        
        # Remove the Badge_Position column
        new_table <- new_table %>% select(-'Badge_Position')
        
       
        # Add a new column "Period" based on Period_Begin_Date and Period_End_Date
        new_table$Period <- ifelse(month(new_table$'Period_Begin_Date') == month(new_table$'Period_End_Date'),
                                   paste(year(new_table$'Period_Begin_Date'), "-", month.name[month(new_table$'Period_Begin_Date')]),
                                   paste(year(new_table$'Period_Begin_Date'), "-", month.name[month(new_table$'Period_Begin_Date')], "to", month.name[month(new_table$'Period_End_Date')]))
        
       
       
        # Fill null values in specific columns within each group
        columns_to_fill <- c(
          "YTD_Effective_Doseective_Dose_DDE_mSv", "YTD_Collar_Dose_LDE_mSv", "YTD_SDE",
          "Lifetime_DDE_mSv", "Lifetime_LDE", "Life_SDE",
          "Chest", "Waist", "Collar_LDE","Collar_Calc", "Fetal", "Assign_Landauer", "Effective_Dose","Chest_temp","Waist_temp","Collar_LDE_temp","Fetal_temp","Effective_Dose_temp","Collar_DDE_temp","Assign_Landauer_temp"
        )
        for (col in columns_to_fill) {
          new_table[[col]] <- ave(new_table[[col]], new_table$'Participant_Number', new_table$Period, FUN = function(x) {
            ifelse(is.na(x), x[!is.na(x)][1], x)
          })
        }
        
        # Add a new column "Assign (Calc)"
        new_table <- new_table %>%
          rowwise() %>%
          mutate(
            'Assign_Calculated' = if (!is.na(Waist) & !is.na(Collar_Calc)) {
              1.5 * Waist+ 0.04 * Collar_Calc
            } else if (is.na(Waist) & !is.na(Chest) & !is.na(Collar_Calc)) {
              1.5 * Chest + 0.04 * Collar_Calc
            } else if (is.na(Waist) & is.na(Chest) & !is.na(Collar_Calc)) {
              0.3 * Collar_Calc
            }else if (!is.na(Waist) & is.na(Chest) & is.na(Collar_Calc)) {
              1.5 * Waist
            }else {
              NA_real_
            }
          ) %>%
          ungroup()  # Remove the rowwise grouping
        
        # Add a new column "Effective_Dose"
        new_table <- new_table %>%
          rowwise() %>%
          mutate(
            `Effective_Dose` = if (is.na(Waist) & !is.na(Chest) & is.na(Collar_Calc)) {
              1*Chest
            } else if (!is.na(Waist) & is.na(Collar_Calc)& !is.na(Chest)) {
              1*Chest
            } else if (!is.na(Waist) & !is.na(Collar_Calc)) {
              1.5 * Waist + 0.04 * Collar_Calc
            } else if (is.na(Waist) & !is.na(Chest) & !is.na(Collar_Calc)) {
              1.5 * Chest + 0.04 * Collar_Calc
            } else if (is.na(Waist) & is.na(Chest) & !is.na(Collar_Calc)) {
              0.3 * Collar_Calc
            }else if (!is.na(Waist) & is.na(Chest) & is.na(Collar_Calc)) {
              1.5 * Waist
            }else {
              NA_real_
            }
          ) %>%
          ungroup()  # Remove the rowwise grouping
        
      
        # Remove the residual unused columns 
        new_table <- new_table %>% select(-'Collar_Calc')
        new_table <- new_table %>% select(-'Chest_temp')
        new_table <- new_table %>% select(-'Waist_temp')
        new_table <- new_table %>% select(-'Effective_Dose_temp')
        new_table <- new_table %>% select(-'Fetal_temp')
        new_table <- new_table %>% select(-'Collar_DDE_temp')
        new_table <- new_table %>% select(-'Collar_LDE_temp')
        new_table <- new_table %>% select(-'Assign_Landauer_temp')
        new_table <- new_table %>% select(-'Current DDE')
        new_table <- new_table %>% select(-'Current LDE')
        new_table <- new_table %>% select(-'Current SDE')
        
        # Reorder columns with "Staff_Member" as the first column
        new_table <- new_table %>%
          select('Staff_Member', 'Department', 'Hospital', 'Series_Code', 'Period','Chest','Waist','Collar_LDE','Fetal','Effective_Dose','Assign_Landauer','Assign_Calculated'
                 , everything())
       
        # fix column "Participant_Number" with only the first 5 numbers
        new_table <- new_table %>%
          mutate(Participant_Number = substr(as.character(Participant_Number), 1, 5))
        
        
       
        
        # Check if the table already exists in the database
        if (dbExistsTable(conn, table_name)) {
          # If the table exists, append new records to it
          dbWriteTable(conn, table_name, new_table, append = TRUE, row.names = FALSE)
          
          # Remove duplicate rows based on specified columns and keep the latest entry
          query <- sprintf('DELETE FROM "%s" WHERE rowid NOT IN (SELECT MAX(rowid) FROM "%s" GROUP BY Participant_Number, "Participant_Number", "Period")', table_name, table_name)
          dbExecute(conn, query)
        } else {
          # If the table does not exist, create it and write the new table to the database
          dbWriteTable(conn, table_name, new_table, append = FALSE, row.names = FALSE)
        }
        }
      }
    }
    
    
    
    # Call the function for split_data
    process_data(split_data, conn, column_names)
    
    # Call the function for split_data_alternative
    process_data(split_data_alternative, conn, column_names)
    
    
   
    
      # Close the SQLite connection
      dbDisconnect(conn)
      
      # Reopen the SQLite connection
      conn <- dbConnect(SQLite(), db_name)
      
      # Sort the entire database by Period_End_Date, "Department", and Staff_Member
      query_sort <- 'SELECT * FROM sqlite_master WHERE type="table";'
      tables <- dbGetQuery(conn, query_sort)$tbl_name
      for (table_name in tables) {
        query_sort <- sprintf('SELECT * FROM "%s" ORDER BY Period_End_Date DESC, "Department" ASC, Staff_Member ASC;', table_name)
        sorted_data <- dbGetQuery(conn, query_sort)
        dbWriteTable(conn, table_name, sorted_data, append = FALSE, overwrite = TRUE, row.names = FALSE)
      }
      
      # Close the SQLite connection again
      dbDisconnect(conn)
      
      
   
    # Display a success message to the user
        shinyalert::shinyalert(title = "Database Created",
                           text = paste("Data from all Landauer Excel files have been successfully uploaded and saved to the database:", db_name),
                           type = "success")
    
  })
  
  
}



shinyApp(ui, server)
