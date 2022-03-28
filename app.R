library(dplyr)
library(ggplot2)
library(waffle)
library(shiny)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(shinybusy)

# Wordclouds
# SEE: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
library(wordcloud)
library(RColorBrewer)
library(tm)

# PDF
library(pdftools)
library(pdfsearch)

# Text summarisation
library(lexRankr)


#Import Data

# CSS for the UI
button_color_css <- "
#SearchBtnBlue {
  background: DodgerBlue;
  font-size: 15px;
}
.rightAlign{float:right;}

.centreDiv {
  text-align: center;
  vertical-align: middle;
}
.centreBox {
  margin: 10px 0px 10px 0px;
  height: 130px;
  text-align: center;
  vertical-align: middle;
  padding-top: 15px;
  color: antiquewhite;
}
.centreBox a {
  color: #8df1ec;
}
.ageBackground { background: #f3f3f3; 
                 color: #ffffff; 
                 border: 2px solid #b3adad;
}
.plumBackground { background: #d75e85; }
.plumBackground h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
.plumBackground2 { background: #bf3b65; }
.plumBackground2 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
.plumBackground3 { background: #95284a; }
.plumBackground3 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }

"

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Abstractor", theme = shinytheme("flatly"),
             tabPanel("Summarizer", fluid = TRUE, icon = icon("vote-yea"),
                      tags$style(button_color_css),
                      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                      tags$script(type="text/javascript", src="//s7.addthis.com/js/300/addthis_widget.js#pubid=ra-55a2c0ea679e0df1"),
  
                      # Google Analytics
                      tags$head(includeHTML(("google-analytics.html"))),

                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="16x16")),
                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="32x32")),
                      tags$head(tags$link(rel="canonical", href="https://vaccines.shinyapps.io/Abstractor/", sizes="32x32")),

                      # Open Graph
                      tags$head(tags$meta(property="og:title", content="Abstractor - Open Source Text Summarizer/ML project for Pfizer Document Analysis")),
                      tags$head(tags$meta(property="og:site_name", content="Abstractor")),
                      tags$head(tags$meta(property="og:url", content="https://vaccines.shinyapps.io/Abstractor/")),
                      tags$head(tags$meta(property="og:description", content="Search, summarise, analyse and discover information from the Pfizer documents released under FOIA")),
                      tags$head(tags$meta(property="og:type", content="website")),
                      tags$head(tags$meta(property="og:image", content="https://images.unsplash.com/photo-1618961734760-466979ce35b0?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1032&q=80")),

                      # Other pre-load setup                      
                      #shinythemes::themeSelector(),
                      add_busy_bar(color = "DodgerBlue", height = "5px"),

                      sidebarLayout(
                        sidebarPanel(
                          width=3,
                          helpText("This tool summarizes a document. It takes an incoming PDF and chooses what it considers to be the most important sentences and presents them in order."),
                          titlePanel("Summarizer"),
                          textInput(inputId = "pdfURL", 
                                    label = 'Location of PDF',
                                    value = 'https://phmpt.org/wp-content/uploads/2021/11/5.3.6-postmarketing-experience.pdf'
                          ),
                          br(),
                          actionButton(inputId = "SummarizeBtn", label = " Search", icon = icon("search"), style = "width: 100%"),
                          br(),
                          br(),
                          HTML("Author(s): dagger59"),
                          br(),
                          HTML("Contact: <a href='mailto:pfizerdox@protonmail.com'>pfizerdox@protonmail.com</a>"),
                          p(),
                          HTML("<iframe id='kofiframe' src='https://ko-fi.com/dagger59/?hidefeed=true&widget=true&embed=true&preview=true' style='border:none;width:100%;padding:10px;background:#ffffff;' height='712' title='jasonmorphett'></iframe>"),
                        ),
                        mainPanel(
                          width=9,
                          htmlOutput("summarizerOutput"),
                          hr(),
                          fluidRow(
                            HTML('<div class="addthis_inline_share_toolbox"></div>')
                          ),
                          br()
                        )
                      )
             ),

             tabPanel("Keywords", fluid = TRUE, icon = icon("file-alt"),
                      sidebarLayout(
                        sidebarPanel(
                          width=3,
                          helpText("This tool looks in a directory for all pdfs and searches them for keywords.  The results are then displayed by document and page number in a table with links to the documents matching the keyword(s)"),
                          titlePanel("Keywords finder"),
                          textInput(inputId = "pdfURL2", 
                                    label = 'Location of PDFs',
                                    value = 'C:\\Enigma\\Pfizer'
                          ),
                          textInput(inputId = "keywords", 
                                    label = 'Keywords',
                                    value = '',
                                    placeholder = 'e.g. antibody,HIV'
                          ),
                          helpText("Tip: Seperate words/phrases with commas"),
                          br(),
                          actionButton(inputId = "KeywordBtn", label = " Search", icon = icon("search"), style = "width: 100%"),
                          br(),
                          br(),
                          HTML("Author(s): mk_hostile | dagger59"),
                          br(),
                          HTML("Contact: <a href='mailto:pfizerdox@protonmail.com'>pfizerdox@protonmail.com</a>"),
                          p(),
                          HTML("<iframe id='kofiframe' src='https://ko-fi.com/dagger59/?hidefeed=true&widget=true&embed=true&preview=true' style='border:none;width:100%;padding:10px;background:#ffffff;' height='712' title='jasonmorphett'></iframe>"),
                        ),
                        mainPanel(
                          width=9,
                          fluidRow(
                            column(
                              htmlOutput("keywordsOutput"), width = 10)
                          ),
                          hr(),
                          fluidRow(
                            HTML('<div class="addthis_inline_share_toolbox"></div>')
                          ),
                          br()
                        )
                      )
             ),
             
             tabPanel("Next Tool", fluid = TRUE, icon = icon("glasses"),
                      fluidRow(
                        div(class="centreDiv", 
                            p("This is a blank panel where additional tools can be added"),
                            p("People/Teams can contribute to a tool"),
                            h2("Project Enigma")
                        )
                      )             
             ),

             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("Get Involved", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          h3(p("Calling all R/Shiny developers!")),
                                          p("We are an independent team of analysts, computer scientists and clinicians. The team have analysed, published and made videos on the dangers of SARS-CoV-2 'vaccines' and supported legal efforts against the vaccine regulators."),
                                          p("Abstractor is an Open Source project looking to assist anyone processing the ", a("55,000 pages", href = "https://aaronsiri.substack.com/p/instead-of-fdas-requested-500-pages"), " of monthly FOIA data from Pfizer being released by the FDA via the Public Health and Medical Professionals for Transparency (", a("PHMPT", href = "https://phmpt.org/"), ") organization.  The first tranche of ", a("documents", href = "https://phmpt.org/pfizers-documents/"), " was released on March 1st 2022 and subsequent to that, there will be a further release of 10s of thousands of pages of documents over the coming months."),
                                          p("We would like to reach out to interested developers with skills in R/Shiny and in particular text mining, text summarization, machine learning, AI, text analysis, document processing etc. to join us in extending Abstractor."),
                                          p("Abstractor will become the 'goto' resource for lawyers, the press, independent researchers and anyone interested in finding out what Pfizer submitted in support of it obtaining a EUA in record time."),
                                          p("If you would like to get involved, please contact us ", a("here", href = "mailto:abstractor@protonmail.com?subject=Abstractor: GETTING INVOLVED")),
                                          br(),
                                          h3(p("Getting started")),
                                          br(),
                                          HTML("<ol>"),
                                          HTML("<li><b>Get in contact</b>: Send us an <a ahref='mailto:abstractor@protonmail.com?subject=Abstractor: GETTING INVOLVED'>email</a> with a brief description about yourself and your idea for either a tool"),
                                          HTML("<li><b>We'll get back to you</b>: We'll take a look at your email and invite you to a Zoom call"),
                                          HTML("<li><b>Let's have a chat</b>: Nothing formal, just a 'getting to know each other' type call"),
                                          HTML("<li><b>Download Abstractor</b>: Great! you're on board. Download <a ahref='https://github.com/onmas59/Abstractor'>Abstractor</a> from GitHub"),
                                          HTML("<li><b>Start coding!</b>: Follow our online tutorial on how to get started in under 5 minutes"),
                                          HTML("</ol>"),
                                          br(),
                                          p("Depending on the complexity of your tool, you could be published and live on Abstractor in a matter of a few days!"),
                                          br(),
                                          hr(),
                                          h3(p("Contact us")),
                                          p("Get in touch: abstractor@protonmail.com"),
                                          br()
                                   ))
                        ),
                        tabPanel("Sources and Data", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h3(p("VAERS")),
                                          h5(p("CAVE uses data from the Vaccine Adverse Events Reporting System (", a("VAERS", href = "https://vaers.hhs.gov/"), "). VAERS is a database used to record and detect possible safety issues with vaccines. Data for adverse events from vaccines date back to 1990 and data is made publicly available to medical practitioners and the general public alike."),
                                             p("VAERS data can be entered by medical personnel and individuals and contains reports of their experiences."),
                                             p("The Center for Disease Control (", a("CDC", href = "https://www.cdc.gov/"), ") and Food and Drug Administration (", a("FDA", href = "https://www.fda.gov/"), ") jointly administer VAERS and respond to reports."),
                                             p("The data is comprehensive and provided in a way that can be analysed by the public, though skills in data handling are required to process it.  However, the CDC does provide a GUI 'like' tool called ", a("CDC WONDER", href = "https://wonder.cdc.gov/vaers.html"), " which is available to the public."),
                                             p("CAVE is kept up to date by periodically collecting the latest 500,000 adverse event records from VAERS and removes some fields to fit within the limits imposed by the website hosting company. A description of the fields can be made available by contacting me at uea15577@gmail.com."),
                                             br(),
                                             img(src = "vaers-logo.png", height = "40px"),
                                             br(),
                                             br()
                                          )
                                   ),
                                   column(6,
                                          h3(p("Other Sources")),
                                          h5(p("The following sources are used for additional data"),
                                             HTML("<ul><li>Population statistics - <a href='https://www.census.gov/topics/population.html'>US Census Department</a></li><li>COVID cases (US) - <a href='https://covid.cdc.gov/covid-data-tracker/#datatracker-home'>CDC Covid Data Tracker</a></li><li>Medical definitions - <a href='https://encyclopedia.thefreedictionary.com/'>The Free Dictionary</a></li></ul>")
                                          ),
                                          br(),
                                          h3(p("Adverse Event Reporting")),
                                          h5(p("Use following links to report any adverse events"),
                                             HTML("<ul><li>In the US - <a href='https://vaers.hhs.gov/reportevent.html'>VAERS</a></li><li>In the UK - <a href='https://coronavirus-yellowcard.mhra.gov.uk/'>Yellow Card</a></li></ul>")
                                          ),
                                          br()
                                   ))
                                 
                        ),
                        tabPanel("About Us", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h3(p("About the Project")),
                                          h5(p("How to join the project")),
                                          h5(p("How to contact us")),
                                          br(),
                                          h4(p("Spread The Word")),
                                          h5(p("Please help spread the word by sharing it on your social networks and/or sending it to a friend"),
                                             p("Follow me on ", a("substack", href = "https://jasonmorphett.substack.com/"), " where I write articles on VAERS insight"),                                             br(),
                                             HTML('<div class="addthis_inline_share_toolbox"></div>')
                                          ),
                                   ),
                                   column(6,
                                          h3(p("About the Team")),
                                          h5(p("Some words"),
                                             p("About us as a team"),
                                             p("And a suitable image"),
                                             
                                          ),
                                          hr(),
                                          HTML('<img src="pen-y-fan.jpg", height="300px"'),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Credits:"),
                                 h6(
                                   p("Photo by Brano on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/QSuou3VAtf4"))),
                                 h6(
                                   p("Photo by Nathan Dumlao on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/Y3AqmbmtLQI"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    ".")
                        )
             )
  )
)

##########################################################
# Define server
server <- function(input, output, session) {

  # Keyword finder tool
  # SEE: https://cran.r-project.org/web/packages/pdfsearch/vignettes/intro_to_pdfsearch.html
  output$keywordsOutput <- renderUI({
      ####################################################      
      # Your code goes here
      keywords <- rVals$keywords_keywords
      pdfURLs <- rVals$keywords_pdf

      # Do we have keywords?
      if(nchar(keywords) > 1) {
        showNotification("Working", closeButton=FALSE, duration = 5)
        
        # Remove leading and trailing whitespace and split into vector
        keywords <- unlist(strsplit(keywords, ","))
        keywords <- trimws(keywords)
        
        # Keyword finder
        # SEE: https://cran.r-project.org/web/packages/pdfsearch/vignettes/intro_to_pdfsearch.html
        directory <- pdfURLs
        results <- keyword_directory(directory
                            ,keyword = keywords
                            ,surround_lines = FALSE
                            ,ignore_case = TRUE
                            ,full_names = TRUE)

        # Shorten the sentences  
        results$line_text_short <- paste0(substr(results$line_text, 1, 100), "...")
        
        # Create hyperlink to files
        print(directory)
        directory <- gsub("\\\\", "/", directory)
        print(directory)
        #results$pdf_name_link <- paste0("<a href=", directory, "/", results$pdf_name, "#page=", results$page_num, " target='blank'>", results$pdf_name, "</a>")
        results$pdf_name_link <- paste0("<a href='#page=4 target='blank'>", results$pdf_name, "</a>")
        print(results$pdf_name_link)
          
        # Subset the results
        df <- results %>%
          select(
            "pdf_name_link",
            "keyword",
            "page_num",
            "line_num",
            "line_text_short"
          )
  
        # Show the datatable
        # See: https://datatables.net/reference/option/
        # And: https://rstudio.github.io/DT/
        DT::datatable(df,
                      class='table compact', 
                      #style="jqueryui", 
                      rownames = FALSE, 
                      escape = FALSE, 
                      width="120%",
                      height=400,
                      #filter = 'top', #Position of column filters
                      extensions = 'Buttons', 
                      options = list(ordering=F, 
                                     language = list(search = 'Filter:'),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#2C3E50', 'color': '#fff'});",
                                       "}"
                                     ),
                                     displayLength=10,  #Records to show/page
                                     lengthChange = 1,  #Show/hide records/page
                                     dom = 'Blfrtip',   #Reqiured for buttons
                                     buttons = c('copy', 'csv', 'excel', 'print'),
                                     searchHighlight = TRUE
                      ),
                      colnames = c("Filename" = "pdf_name_link", "Keyword" = "keyword", "Page" = "page_num", "Line" = "line_num", "Text" = "line_text_short")
        )
      }
      else {
        HTML("Results will appear here...")
      }
  })
  
  
  # Keyword finder tool
  # SEE: https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/
  output$summarizerOutput <- renderUI({
    ####################################################      
    # Your code goes here
    pdfURL <- rVals$summarizer_pdf
    str1 <- "<h4>Full Text (page 1)</h4>"
    str2 <- "--------------------<br>"
    str3 <- "<h4>Summary Text (whole document)</h4>"

    # Do we have a URL?
    if(nchar(pdfURL) > 1) {
      showNotification("Working", closeButton=FALSE, duration = 5)
      
      # Get PDF text
      pdf <- pdf_text(pdfURL)    

      # Summarise PDF
      showNotification("Summarizing PDF", closeButton=FALSE, duration = 5)
      top_3 = lexRankr::lexRank(pdf,
                                #only 1 article; repeat same docid for all of input vector
                                docId = rep(1, length(pdf)),
                                #return 3 sentences to mimick /u/autotldr's output
                                n = 3,
                                continuous = TRUE)
      
      # Reorder the top 3 sentences to be in order of appearance in article
      order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
      
      # Extract sentences in order of appearance
      ordered_top_3 = top_3[order_of_appearance, "sentence"]
      
      HTML(paste(str1, pdf[1], str2, str3, "<h4>#1</h4>", ordered_top_3[1], "<br><h4>#2</h4>", ordered_top_3[2], "<br><h4>#3</h4>", ordered_top_3[3], sep = '<br/>'))
    }
    else {
      HTML("Results will appear here...")
    }
  })
  
  # -------------------------
  # observe event for updating reactiveValues
  observeEvent(input$SummarizeBtn,
  {
    # Set inputs from UI 
    rVals$summarizer_pdf <- input$pdfURL
  })
  
  # -------------------------
  # observe event for updating reactiveValues
  observeEvent(input$KeywordBtn,
  {
    # Set inputs from UI 
    rVals$keywords_pdf <- input$pdfURL2
    rVals$keywords_keywords <- input$keywords
    
  })
  
  # -------------------------
  # See: https://riptutorial.com/shiny/example/32342/reactivevalues
  rVals <- reactiveValues(
    summarizer_pdf="",        # Summarizer tool
    keywords_pdf="",          # Keywords tool
    keywords_keywords="",     # Keywords tool
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
