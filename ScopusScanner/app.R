#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# This variable defines whether the app shows as stateful
local_run <- FALSE
data_loaded <- FALSE


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(glue)
library(tidytext)
library(wordcloud2)
library(gghighlight)
library(DT)
#library(DTedit)
#library(dplyrExtras)
library(writexl)
library(logger)
library(rclipboard)

# prepare the fixed data ----
categories <- NULL
raw <- NULL 
if(file.exists(here::here("categories.csv"))){
    categories <- read_csv(here::here("categories.csv")) %>% pull(CATEGORY)
} else {
    categories <- c("Information Visualization", "HCI & Evaluation", "Narrative Structures", 
                    "Frameworks", "Effects & Biases", "Areas of Application")
}

if(file.exists(here::here("database.rds"))) {
    raw <- read_rds(here::here("database.rds"))
    data_loaded <- TRUE
} else {
    raw <- data.frame(selected = c(NA), UT = c("DUMMYID"))
    oldnames <- names(raw)
    for (cat in categories) {
        raw <- raw %>% add_column(cat = rep(NA, 1))
    }
    
    names(raw) <- c(oldnames, categories)
    raw <- as_tibble(raw)
}


ids <- raw$UT %>% unique()

# function to format author strings
str_format_authors <- function(str){
    str %>% str_split(";") %>% map(str_to_title) %>% unlist() %>% paste(collapse = "; ")
}


#options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 30*1024^2)

# UI ----
ui <- dashboardPage(
    
    dashboardHeader(title = "Scopus Browser"),
    dashboardSidebar(sidebarMenu(id = "tabs",
        menuItem("Start", tabName = "start", icon = icon("th")),
        #menuItem("Tags", tabName = "tags", icon = icon("tags")),
        menuItem("Viewer", tabName = "viewer", icon = icon("dashboard")),
        menuItem("Search", tabName = "search", icon = icon("search")),
        menuItem("Export", tabName = "export", icon = icon("download")),
        hr(),
        infoBoxOutput("infobox_paper_count")
    )),
    dashboardBody(
        # UI start page ----
        tabItems(
            tabItem(tabName = "start",
                    box(width = 12, title = "Instructions", collapsible = T, status = "primary", solidHeader = TRUE,
                        h3("Welcome to the the Scopus browser"),
                        strong(glue("This tool helps analyzing data exported from Scopus. It works a little bit like Tinder."),
                            ), br(),
                        p(glue("By presenting a quick overview over the content and both citation and publication year,
                                making a decision on whether or not to include a paper in further analysis can be made.
                                By clicking on 'Accept' or 'Ignore' you can quickly decide whether 
                                or not to use a paper in a later analysis stage.")
                          ),
                        p(glue("By creating categories, you can also assign tags to individual papers. 
                                You can always, store and continue your current progress by saving your current progress in an 
                                RDS File, which can be uploaded here in the right box.
                               ")
                          ),
                        p(glue("In the end you can export your decisions in a readble Excel file or reuse your RDS file to
                                  look for PDF files on Scopus, Google Scholar, or Sci-Hub.
                               ")
                          )
                        ),
            box(width = 6, status = "info",
                        title = "A) New Project",
                        p("Upload a Scopus CSV export file. It is best to include all fields when exporting in Scopus."),
                        fileInput("filecsv", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        ),
                    box(width = 6, status = "info",
                        title = "2) Continue Project",
                        p("Upload a previously saved RDS file from Scopus Browser"),
                        fileInput("filerds", "Choose RDS File",
                                  multiple = FALSE,
                                  accept = c(".rds")),
                    )
                    
            ),
            # TODO::::::::::::UI tags page ----
            #tabItem(tabName = "tags",
            #        fluidRow(
            #            box(width = 12, title = "Setup Tags", status = "primary", 
            #                uiOutput('mytags')
            #            )
            #        )
            #),
            # UI viewer page ----
            tabItem(tabName = "viewer",
                fluidRow(
                    box(width = 9, status = "primary",
                        rclipboardSetup(),
                        plotOutput("eval", height = "250px", click = "plot1_click"),
                        uiOutput("ui_select_state"),
                        uiOutput("ui_body")
                        ),
                    
                    box(width = 3, status = "info",
                        title = "Controls",
                        #inputs
                        uiOutput("ui_select_paper"),
                        checkboxInput("hidecomplete", "Hide completed papers", value = TRUE),
                        hr(),
                        checkboxGroupInput("tags", "Add tags", choices = categories),
                        actionButton("ignorebtn", "Ignore", icon = icon("remove"), style="color: #fff; background-color: #B22222; border-color: #CD5C5C"),
                        actionButton("acceptbtn", "Accept", icon = icon("plus"), style="color: #fff; background-color: #006400; border-color: #228B22"),
                        br(),br(),
                        uiOutput("local_save"),
                        downloadButton("downloadrds2", "Save progress", icon = icon("download")),
                        br(), br(),
                        uiOutput("bibtex")
                        
                        
                    )
                ),
                fluidRow(
                    box(width = 12, title="Wordcloud from Abstract (tf-idf)",
                        uiOutput("wordcloud")),
                    box(width = 12, 
                        title = "PDF Lookup - external website", status = "info",
                        uiOutput("iframe"))
                )
            ),
            # UI search page ----
            tabItem(tabName = "search",
                    h2("Search all files"),
                    DTOutput("alldata")
            ),
            # UI export page ----
            tabItem(tabName = "export",
                    box(width = 12, title = "Exporting", 
                        status = "primary", solidHeader = TRUE, 
                        h4("Choose your desired export."),
                        box(width = 4, title = "Export to RDS", status = "info",
                            p("Use this file to continue your process later"),
                            downloadButton("downloadrds", "Download RDS", icon = icon("download")),
                            ),
                        box(width = 4, title = "Export to Excel", status = "info",
                            p("Use this function to export your selected papers in a readable format to use in Microsoft Excel."),
                            downloadButton(outputId = "download_xls_default", label = "Download XLSX", icon = icon("download")),
                        ),
                        box(width = 4, title = "Export to Excel (detailed)", status = "info",
                            p("Use this function to export your data to use in Microsoft Excel and conigure the output to your liking."),
                            checkboxInput("export_selected_only", "Export selected only?"),
                            downloadButton(outputId = "download_xls", label = "Download XLSX", icon = icon("download")),
                            br(),br(),
                            uiOutput("ui_export_fields"),
                        )
                    )
            )
        )
    )
)



















# Server ----
server <- function(input, output, session) {
    
    rv <- reactiveValues(clicks = 0, idx = 1, reload = 0)
    
    #mydata <- data.frame(CATEGORY = character(), 
     #                    used = factor(levels = c("yes", "YES")), 
      #                   stringsAsFactors = FALSE)
    
    ##### Callback functions.
    my.insert.callback <- function(data, row) {
        # 'data' contains the dataframe *after* the row has been inserted/added
        # 'row' is the row number where data has been inserted
        categories <<- rbind(categories, data[row,])
        # in this case, 'mydata' should just be the same as 'data'
        return(categories)
    }
    
    my.update.callback <- function(data, olddata, row) {
        # 'data' contains the dataframe *after* the row has been updated
        # 'row' is the row number where data has been updated
        # 'olddata' is the previous version of the data
        categories[row,] <<- data[row,]
        # in this case, 'mydata' should just be the same as 'data'
        return(categories)
    }
    
    my.delete.callback <- function(data, row) {
        # 'data' contains the dataframe *before* the row has been deleted
        # 'row' is the row number where data is to be deleted
        categories <<- categories[-row,]
        # in this case, 'mydata' should just be the same as data[-c(row),]
        return(categories)
    }
    
    ##### Create the DTedit object
    # DTedit::dtedit(input, output,
    #                name = 'mytags',
    #                thedata = categories,
    #                edit.cols = c('CATEGORY', "used"),
    #                edit.label.cols = c('CATEGORY', "used"),
    #                #input.types = c(notes='textAreaInput'),
    #                view.cols = c('CATEGORY', "used"),
    #                callback.update = my.update.callback,
    #                callback.insert = my.insert.callback,
    #                callback.delete = my.delete.callback)
    
    
    
    #print(mydata)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # ui: infobox papers ----
    output$infobox_paper_count <- renderInfoBox({
        rv$clicks
        if(data_loaded) {
            return (infoBox("test", nrow(raw), subtitle = "Loaded papers", color = "blue"))
        } else {
            return (infoBox("test", "0", subtitle = "Loaded papers", color = "blue"))
        }
        
    })
    
    # event: file upload ----
    observeEvent(input$filecsv, {
        
        req(input$filecsv)
        log_info("event: file upload CSV ───────────────────────────┐")
        
        df <- NULL
        withProgress(message = 'Loading data', value = 0,{
            incProgress(0.3, detail = "Converting file...")
            df <- bibliometrix::convert2df(input$filecsv$datapath, dbsource = "scopus", format = "csv")
            incProgress(0.6, detail = "Adding columns...")
            
            df <- df %>% add_column(selected = rep(NA, nrow(df)))
            oldnames <- names(df)
            for (cat in categories) {
                df <- df %>% add_column(cat = rep(NA, nrow(df)))
            }
            incProgress(0.7, detail = "Adding sorting categories...")
            names(df) <- c(oldnames, categories)
        
            raw <<- as_tibble(df)
            incProgress(0.9, detail = "Analyzing text-frequencies...")
            
            word_data()
            incProgress(1, detail = "Updating internal data ...")
        })
        log_info("event: file uploaded ───────────────────────────┘")
        data_loaded <<- TRUE
        rv$clicks <- rv$clicks + 1
        rv$reload <- rv$reload + 1
        updateTabItems(session, "tabs", "viewer")
    })
    
    
    # event: file upload RDS----
    observeEvent(input$filerds, {
        
        req(input$filerds)
        log_info("event: file upload ───────────────────────────┐")
        
        df <- NULL
        withProgress(message = 'Loading data', value = 0,{
            incProgress(0.3, detail = "loading file...")
            
            raw <<- read_rds(input$filerds$datapath)
            incProgress(0.9, detail = "Analyzing text-frequencies...")
            
            word_data()
            incProgress(1, detail = "Updating internal data ...")
        })
        log_info("event: file uploaded ───────────────────────────┘")
        rv$clicks <- rv$clicks + 1
        rv$reload <- rv$reload + 1
        data_loaded <<- TRUE
        updateTabItems(session, "tabs", "viewer")
    })
    
    
    
    
    # react: publication data ----    
    pub_data <- reactive({
        log_info("- react: pub_data()")
        rv$clicks # watch changes
        
        if(input$hidecomplete) {
            return( raw %>% filter(is.na(selected)))
        } else {
            return(raw)
        }
    })
    
    # react: current row ----
    current_row <- reactive({
        log_info("- react: current_row()")
        rv$clicks # watch for changes
        row_data <- pub_data()
        if(is.null(row_data)) {
            return(NULL)            
        }
        row_data[rv$idx,]
    })
    
    # react: returns the currently selected ID ----
    current_ID <- reactive({
        log_info("- react: current_ID()")
        index <- rv$idx
        id <- pub_data()[index,]$UT
        id
    })
    
    # react: max_idx
    max_idx <- reactive({
        log_info("- react: max_idx()")
        nrow(pub_data())
    })
    
    # event: hide complete
    observeEvent(input$hidecomplete,{
        log_info("event: hidecomplete")
        rv$clicks <- rv$clicks + 1
    }, ignoreNULL = FALSE)
    
    
    
    # output-ui: selection generation ----
    output$ui_select_paper <- renderUI({
            open <- raw %>% filter(!is.na(selected)) %>% count()
            
            div(
                progressBar(title = "Completion", id = "progress", value = open/nrow(raw)*100, display_pct = TRUE), br(),
                actionButton("firstitem", "", icon = icon("fast-backward")),
                actionButton("previtem", "", icon = icon("step-backward")),
                span(paste(rv$idx, "/", max_idx())),
                actionButton("nextitem", "", icon = icon("step-forward")),
                actionButton("lastitem", "", icon = icon("fast-forward")),
                
            )
        })

    
    # event: UI-updater ----
    observeEvent(rv$clicks, {
        log_info("event: refresh data ───────────────────────────┐")
        if(nrow(pub_data())==0){
            updateCheckboxInput(session, "hidecomplete", value = FALSE)
        }
        # ensure bounds
        if(rv$idx < 1) rv$idx <- 1
        if(rv$idx > max_idx()) rv$idx <- max_idx()
        
        values <- current_row() %>% select(!!categories)
        v <- names(values)[values[1,] %>% unlist()]
        
        updateCheckboxGroupInput(session, inputId = "tags", selected = v)
        
        
     
        log_info("event: refresh data ───────────────────────────┘")
    })
    
    
    # event: select next ----
    observeEvent(input$nextitem,{
        log_info("event: next btn ───────────────────────────┐")
        rv$clicks <- rv$clicks + 1
        if( rv$idx < max_idx()) {
            rv$idx <- rv$idx + 1
        }
        log_info("event: next btn ───────────────────────────┘")
    })
    
    # event: select prev ----
    observeEvent(input$previtem,{
        log_info("event: prev btn ───────────────────────────┐")
        rv$clicks <- rv$clicks + 1
        if(rv$idx > 1) {
            rv$idx <- rv$idx - 1    
        }
        log_info("event: prev btn ───────────────────────────┘")
    })
    
    # event: first item ----
    observeEvent(input$firstitem,{
        log_info("event: first btn ───────────────────────────┐")
        rv$clicks <- rv$clicks + 1
        rv$idx <- 1
        log_info("event: first btn ───────────────────────────┘")
    })
    
    # event: last item ----
    observeEvent(input$lastitem,{
        log_info("event: last btn ───────────────────────────┐")
        rv$clicks <- rv$clicks + 1
        rv$idx <- max_idx()
        log_info("event: last btn ───────────────────────────┘")
    })
    
    
    
    # event: change a tag ----
    observeEvent(input$tags, {
        
        log_info("event: tags clicked ───────────────────────────┐")
        id <- current_ID()
        
        log_debug(glue("Should assign {input$tags} to {id}."))
        
        df <- raw
        # for all tags
        for (tag in categories) {
            log_debug(glue("... Checking {tag}"))
            if (tag %in% input$tags) { # check whether it is set
                log_debug(glue("   ... found"))
                df <- df %>% mutate({{tag}} := ifelse(UT==id, TRUE, .data[[tag]])) # if {{tag}} is set in input then change column {{tag}} elso do nothing 
            } else {
                log_debug(glue("   ... not found"))
                df <- df %>% mutate({{tag}} := ifelse(UT==id, FALSE, .data[[tag]]))

            }
        }

        raw <<- df
        rv$clicks <- rv$clicks + 1
        log_info("event: tags clicked ───────────────────────────┘")
    }, ignoreNULL = FALSE)
    
    
    # event: accept ----
    observeEvent(input$acceptbtn, {
        log_info("event: accept clicked ───────────────────────────┐")

        id <- current_ID()
        
        df <- raw
        df <- df %>% mutate(selected = ifelse(UT==id, TRUE, selected))
        raw <<- df
        
        log_debug(glue("event: accept - changed item...{id}"))
        rv$clicks <- rv$clicks + 1
        log_info("event: accept clicked ───────────────────────────┘")
    })
    
    # event: ignore ----
    observeEvent(input$ignorebtn, {
        log_info("event: ignore clicked ───────────────────────────┐")
        
        id <- current_ID()
        
        df <- raw
        df <- df %>% mutate(selected = ifelse(UT==id, FALSE, selected))
        raw <<- df
        log_debug(glue("event: ignore - changed item...{id}"))
        rv$clicks <- rv$clicks + 1
        log_info("event: ignore clicked ───────────────────────────┘")
    })
    
    
    
    
    # react: word_data for word clouds ----
    word_data <- reactive({
        rv$reload
        log_info("- react: word cloud")
        paper_words <- raw %>% select(UT, AB) %>% unnest_tokens(word, AB) %>% count(UT, word, sort = TRUE)
        total_words <- paper_words %>% group_by(UT) %>% 
            summarize(total = sum(n))
        paper_words <- left_join(paper_words, total_words)
        paper_words <- paper_words %>% bind_tf_idf(word, UT, n) %>% arrange(desc(tf_idf))
        
        paper_words
    })
    
    

    
    # output-ui: show whether current version is selected or not
    output$ui_select_state <- renderUI({
        log_info("ui: state change ───────────────────────────┐")
        res <- NULL
        rw <- current_row()
        if(!is.null(rw)){  
            if(nrow(rw)>0){
                if(!is.na(rw$selected)){
                    if(rw$selected){
                        res <- p(span(paste("Selected as used"), style="color: green;"), icon("plus"))
                    } else {
                        res <- p(span(paste("Selected as irrelevant"), style="color: red;"), icon("remove"))
                    }
                }
            }
        }
        log_info("ui: state change ───────────────────────────┘")
        res
    })

    # ui: clipboard button TODO ----
    output$bibtex <- renderUI({
        req(data_loaded)
        req(current_row())
        
        row_data <- current_row()
        if(nrow(row_data) > 0) {
            txt <- RefManageR::GetBibEntryWithDOI(row_data$DI[1]) %>% RefManageR::toBiblatex()
            
            rclipButton("clipbtn", "Copy bibtex code", paste0(txt, collapse = "\n"), icon("clipboard"))
        }
        
    })
    
    # output: MAIN BODY ----
    output$ui_body <- renderUI({
        rv$clicks
        if(data_loaded){
            log_info("ui: body ───────────────────────────┐")
            res <- NULL
            if(!is.null(pub_data())){
                req(current_row())
                row_data <- current_row()
        
               res <- div(
                    p("Cited", strong(row_data$TC), " times. From ", row_data$PY, align = "center"),
                    tags$a(href=row_data$URL, "Link at Scopus", target="_blank"),
                    span(" - "),
                    a(href = glue("https://scholar.google.com/scholar?q={row_data$TI}"), "Google Scholar", target="_blank", title = "Search at Google Scholar"),
                    span(" - "),
                    a(href = glue("https://sci-hub.tw/{row_data$DI}"), target="_blank", "Sci-Hub", title = glue("https://sci-hub.tw/{row_data$DI}")),
                    h4(str_to_title(row_data$SO)),
                    h1(str_to_sentence(row_data$TI)),
                    h4(str_format_authors(row_data$AU)),
                    p(str_to_sentence(row_data$AB)),
                    h4(strong(str_to_lower(row_data$DE)))
                )
            }
            log_info("ui: body ───────────────────────────┘")
            return(res)
        } else {
            showModal(modalDialog(
                title = "You have not loaded any data!",
                "Please go to the start page to load data"
            ))
            return(box(title = "No data loaded", width = 12, solidHeader = T,
                status = "warning", h3("Please load data first on the 'start' tab.", align = "center")))
        }
    })
    
    
    # output: wordcloud ----
    output$wordcloud <- renderUI({
        rv$clicks
        rv$reload
        req(data_loaded)
        log_info("ui: wordcloud ───────────────────────────┐")
        res <- NULL
        
        if(nrow(pub_data())>0) {
            row_data <- current_row()
            
           if(nrow(row_data) > 0){  
                res <- div(width = "100%",
                    word_data() %>% 
                        filter(UT == row_data$UT) %>% select(word, tf_idf) %>% 
                        wordcloud2(minRotation = 0, maxRotation = 0, fontFamily = "Arial", color = "random-dark")
                )
           }
        }
        log_info("ui: wordcloud ───────────────────────────┘")
        res
    })

    
    # output: iframe SCIHUB ----
    output$iframe <- renderUI({
        rv$clicks
        req(data_loaded)
        log_info("ui: iframe ───────────────────────────┐")
        row_data <- current_row()
        my_test <- NULL
        if(nrow(row_data)>0) {
            my_test <- tags$iframe(src=glue("https://sci-hub.tw/{row_data$DI}"), height=600, width="100%")
            
        }
        log_info("ui: iframe ───────────────────────────┘")
        my_test
    })
    
    # output: GGPlot output ----    
    output$eval <- renderPlot({
        rv$clicks
        req(data_loaded)
        log_info("ui: plot ───────────────────────────┐")
        p <- NULL
        if(nrow(pub_data())>0){
            
            row_data <- current_row()
            #row_data 
            limits <- c(1, NA)
            highlight <- NULL
            if(nrow(row_data)>0) {
                highlight <- gghighlight(UT == row_data$UT, label_key = SR_FULL, label_params = list(size = 4, box.padding = 0.5, ylim  = limits))
            }
    
            jit <- 0.3
            
            pj <- position_jitter(
                width = jit,
                height = jit,
                seed = 123
            )
            
            
            p <- pub_data() %>% 
                ggplot() + 
                aes(x = PY) +
                aes(y = TC) +
                aes(size = TC) +
                geom_point(alpha = 0.5, position = pj) +
                highlight + 
                labs(title = "Paper in relation to corpus", x = "Year", y = "Times cites (log-scale -1 )", caption = glue("jittered with {jit}"), size = "Citations") +
                theme_bw() +
                scale_y_continuous(trans = scales::pseudo_log_trans()) +
                #scale_x_date()
                NULL
        }
        log_info("ui: plot ───────────────────────────┘")
        p
    })
    
    # event: plot clicks ----
    observeEvent(input$plot1_click,{
        #print(input$plot1_click)
        #df <- nearPoints(pub_data(), input$plot1_click, yvar = "TC", addDist = TRUE)
        #print(df)
    })
    
    
    # output: table output ----
    output$alldata <- renderDT({
        rv$clicks
        if(!data_loaded){
            showModal(modalDialog(
                title = "You have not loaded any data!",
                "Please go to the start page to load data"
            ))
        }
        req(data_loaded)
        log_info("Render alldata table")
        raw %>% 
            select(title = TI, AU, PY, SO, DE, UT) %>% 
            mutate(title = str_to_sentence(title)) %>% 
            mutate(SO = str_to_title(SO)) %>% 
            mutate(DE = str_to_lower(DE)) %>% 
            rowwise() %>% mutate(AU = str_format_authors(AU))
    })
    
    
    
    # ui: save button local ----
    output$local_save <- renderUI({
        if(local_run) {
            log_debug("... should show save button")
            return(actionButton("savebtn", "Save progress locally", icon = icon("save")))
        } 
        NULL
    })
    
    
    # event: save btn ----
    observeEvent(input$savebtn, {
        log_info("Writing to disk..")
        write_rds(raw, here::here("database.rds"))
        showModal(modalDialog(
            title = "Data was saved",
            "Click anywhere or press ESC to continue",
            easyClose = TRUE,
            footer = NULL
        ))
        log_info("File written to disk.")
    })
   
    # Export functions ----
    # . output-ui: export xls ----
    output$ui_export_fields <- renderUI({
        log_info("Generating export UI")
        cs <- names(raw)
        shinyWidgets::pickerInput("export_fields",
                           label = "Choose fields to export", 
                           choices = cs, 
                           selected = cs,
                           multiple = TRUE, 
                           options = list(`actions-box` = TRUE))
    })
    
    # . react: export data ----
    edata <- reactive({
        res <- raw %>% select(input$export_fields)
        if(input$export_selected_only) {
            res <- res %>% filter(selected == TRUE)
        }
        res
    })
    
    
    # . react: export selected data ----
    edata_default_sel <- reactive({
        rv$clicks
        raw %>% 
            filter(!is.na(selected) )%>% 
            filter(selected == TRUE) %>% 
            select(author = AU, 
                   title = TI,
                   year = PY,
                   journal = SO,
                   citations = TC,
                   keywords = DE,
                   abstract = AB,
                   DOI = DI,
                   selected,
                   one_of(categories)
                   ) %>% 
            mutate(title = str_to_title(title),
                   journal = str_to_title(journal),
                   keywords = str_to_lower(keywords),
                   abstract = str_to_sentence(abstract)) %>% 
            rowwise() %>% 
            mutate(author = str_format_authors(author))
            
    })
    
    # . react: export non selected -----
    edata_default_notsel <- reactive({
        rv$clicks
        raw %>% 
            filter(!is.na(selected) )%>% 
            filter(selected == FALSE) %>% 
            select(author = AU, 
                   title = TI,
                   year = PY,
                   journal = SO,
                   citations = TC,
                   keywords = DE,
                   abstract = AB,
                   DOI = DI,
                   selected,
                   one_of(categories)
            ) %>% 
            mutate(title = str_to_title(title),
                   journal = str_to_title(journal),
                   keywords = str_to_lower(keywords),
                   abstract = str_to_sentence(abstract)) %>% 
            rowwise() %>% 
            mutate(author = str_format_authors(author))
        
    })
    
    
    # . download: xls default ----
    output$download_xls_default <- downloadHandler(
        filename = function() {
            paste("ScopusBrowser-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write_xlsx(list(selected = edata_default_sel(),
                            rejected = edata_default_notsel()), file)
        }
    )
    
    
    # . download: xls manual ----
    output$download_xls <- downloadHandler(
        filename = function() {
            paste("ScopusBrowser-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write_xlsx(edata(), file)
        }
    )
    
    # . download: rds ----
    output$downloadrds <- downloadHandler(
        filename = function() {
            paste("ScopusBrowser-", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
            write_rds(raw, file)
        }
    )
    
    # . download: rds ----
    output$downloadrds2 <- downloadHandler(
        filename = function() {
            paste("ScopusBrowser-", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
            write_rds(raw, file)
        }
    )
    
}

# Run the application ----
log_info("- - - - - - - - - - - - - - -")
log_info("- - - Application start - - -")
log_info("- - - - - - - - - - - - - - -")
shinyApp(ui = ui, server = server)
