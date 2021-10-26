# double check means
# graphing options - stacked, sd bar
# add filter for groups

# Libraries ####
require(shiny); require(shinyjs); require(shinythemes); require(shinyBS); require(shinyWidgets); require(knitr); require(kableExtra); require(DBI); require(odbc); require(tidyverse); library(DT); require(shinycssloaders)

# Load Data ####

# pronghorn opinion survey 2021
prong21 <- new.env(parent = .BaseNamespaceEnv)
source("code/prong21/source_prong21.R", prong21)


# Random Code ####


# _Code to timeout inactive browser ####

timeoutSeconds <- 3600

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


# _show app loading ####
load_data <- function() {
  Sys.sleep(1)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


# _modal popup for downloading excel/csv ####
myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download_csv","Csv"),
                  br(),
                  br(),
                  downloadButton("download_excel","Excel"),
                  easyClose = TRUE, title = "Download Table")
  )
}


# _javascript code ####
jscode <- '
shinyjs.init = function() {
  $(".nav").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
}
'

# css code ####

css <- '
.disabled {
  cursor: default !important;
  color: white !important;
}

div.dt-button-collection {
    width: 300px;
}
'

css1 <- HTML(
  ".dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
  }
  "
)


# _functions to fingerprint with javascript ####
# fingerprint_code
inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}
# fingerprint_code
inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

# _making some dataframes ####


# _color and bargraph settings ####

bar_border_color <- "Black"
# bar_fill_color <- "#375A7F"
flt_palette <- "Blues"
axis_text_size <- 12
axis_title_size <- 14
legend_text_size <- 10
legend_title_size <- 12
label_text_size <- 4
# bar_fill_color <- RColorBrewer::brewer.pal(3, flt_palette)[2]
bar_fill_color <- ggthemes::economist_pal()(9)[1]
error_bar <- TRUE
spinner_color <- "black"
all_x <- TRUE
spinner_color <- "#76C0C1"  #"#2C3E50"
facet_title_size <- 14
label_size <- 5

# UI CODE ####
ui <- 
  fluidPage(
    singleton(tags$head(tags$script(src = "pop_patch.js"))), # patch to make popups update
    # tags$head(tags$style("div.dataTables_scrollHead span {color: black;}")),
    tags$script(inactivity),
    tags$head(HTML("<title>Opinion Survey Data</title>")),
    tags$head(tags$style(css1)),
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "init"),
    tags$style(css),
    # div(
    #   align = "center",
    #   id = "loading_page",
    #   br(),
    #   br(),
    #   h1("Website loading..."),
    #   br(),
    #   img(src="giphy.webp", height = "600px")
    #   # icon("spinner", class = "fa-spin fa-3x"),
    # ),
    # shinyjs::hidden(
    #   div(
    #     id = "main_content",
    fluidRow(
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    sidebarLayout(
      fluid = TRUE,
      # sidebarPanel ####
      sidebarPanel(
        fluid = TRUE,
        width = 5,
        style = "height: 90vh; overflow-y: auto;",
        fluidRow(
          wellPanel(
            inputIp("ipid"),# fingerprint_code
            inputUserid("fingerprint"),# fingerprint_code
            fluidRow(
              column(6,
                     style = "padding-left:30px",
                     # _survey selection ####
                     uiOutput("sel_survey")
              ),
              wellPanel(
                column(3, align = "right",
                       # _download_survey ####
                       br(),
                       uiOutput("btn_download_survey"),
                ),
                column(3,
                       br(),
                       # _download_report ####
                       uiOutput("btn_download_report"),
                )
              )
            ),
            br(),
            # wellPanel(
            #   style = "background-color:#375A7F;",
            # _type of summary ####
            wellPanel(
              style = "background-color:#222222",
              uiOutput("sel_response_prong21"),
              tabsetPanel(
                tabPanel(
                  id = "group1",
                  title = uiOutput("sel_group1_prong21_title"),
                  br(),
                  fluidRow(
                    column(2,
                           h5("Question",style="padding-top:1px; padding-left:20px;"),
                           h5("Answer",style="padding-top:24px; padding-left:20px;")
                    ),
                    column(10,
                           uiOutput("sel_group1_prong21"),
                           uiOutput("sel_group1_prong21_flt")
                    )
                  )
                ),
                tabPanel(
                  title = uiOutput("sel_group2_prong21_title"),
                  br(),
                  fluidRow(
                    column(2,
                           h5("Question",style="padding-top:1px; padding-left:20px;"),
                           h5("Answer",style="padding-top:24px; padding-left:20px;")
                    ),
                    column(10,
                           uiOutput("sel_group2_prong21"),
                           uiOutput("sel_group2_prong21_flt")
                    )
                  )
                ),
                tabPanel(
                  title = uiOutput("sel_group3_prong21_title"),
                  br(),
                  fluidRow(
                    column(2,
                           h5("Question",style="padding-top:1px; padding-left:20px;"),
                           h5("Answer",style="padding-top:24px; padding-left:20px;")
                    ),
                    column(10,
                           uiOutput("sel_group3_prong21"),
                           uiOutput("sel_group3_prong21_flt")
                    )
                  )
                )
              ),
              wellPanel(
                align = "center",
                style = "background-color:#222222; padding-top:5px;",
                hr(),
                h5("Add or remove groups from your data summary by clicking on the above tabs (e.g. ", span("X-axis, Color Bars, Facet, Y-axis",style = "color:#00BC71"),"), selecting a question you are interested in and filtering to specific answer for that question.")
              )
            ),
            fluidRow(
              column(8,
                     wellPanel(
                       style = "background-color:#222222;",
                       fluidRow(
                         column(4,
                                uiOutput("sel_addlabel"),
                                uiOutput("sel_labelsize")
                         ),
                         column(4,
                                uiOutput("sel_stacked"),
                                uiOutput("sel_dropna")
                         ),
                         column(4, align = "center",
                                uiOutput("sel_enable_help_text")
                         )
                       )
                     )
              ),
              column(4, align = "center",
                     br(),
                     br(),
                     uiOutput("btn_reset_input")
              )
            )
          )
        )
      ),
      # mainPanel ####
      mainPanel(
        width = 7,
        navbarPage(
          id = "navbar",
          fluid = TRUE,
          position = "fixed-top",
          theme = shinythemes::shinytheme(theme = "darkly"),
          # shinythemes::themeSelector(),
          selected = 2,
          title = div(
            div(
              a(href = 'http://ifwisshiny.idfg.state.id.us:3838/MC/WildlifeResearchWebsite/',
                img(src = 'WildlifeResearchPage_logo_small.png',
                    title = "View the adventures of the Wildlife Research Team", height = "70px"),
                style = "padding-top:10px; padding-bottom:10px; padding-right:30px")
            )
          ),
          tabPanel(
            value = "tab2",
            title = h1("Opinion Survey Data",style=c("padding-right:20px;")),
            shinyBS::popify(
              h3("text to get popify to work"), title = "", content = ""
            )
          ),
          # _summary ####
          tabPanel(
            title = h3("Summary",style="padding-top:18px"),
            value = 2,
            tabsetPanel(
              # __plot ####
              tabPanel(
                title = h5("Graph"),
                conditionalPanel(
                  condition = "input.flt_group1_prong21 != ''",
                  wellPanel(
                    style = "padding-left: 40px; padding-right: 40px;",
                    fluidRow(
                      plotOutput("plot_prong21", height = 600) %>% withSpinner(color=spinner_color)
                    )
                  )
                )
              ),
              # __summarized data ####
              tabPanel( 
                title = h5("Table"),
                conditionalPanel(
                  condition = "input.flt_group1_prong21 != ''",
                  wellPanel(
                    style = "background: #303030",
                    wellPanel(
                      style = "background: #F9F9F9",
                      fluidRow(
                        DT::DTOutput("summary_prong21") %>% withSpinner(color=spinner_color)
                      )
                    )
                  )
                )
              )
            )
          ),
          # _data ####
          tabPanel(
            title = h3("Data",style="padding-top:18px"),
            value = 2,
            wellPanel(
              style = "background: #303030",
              wellPanel(
                style = "background: #F9F9F9",
                DT::DTOutput("survey_data_prong21")
              )
            )
          ),
          # _codebook ####
          tabPanel(
            title = h3("Codebook",style="padding-top:20px"),
            value = 3,
            wellPanel(
              style = "background: #303030",
              wellPanel(
                style = "background: #F9F9F9",
                DT::DTOutput("survey_codebook_prong21")
              )
            )
          ),
          h5(textOutput("counter"),style="color:#222222;"),# fingerprint_code
          h5(textOutput("testtext"),style="color:#222222;")# fingerprint_code
        )
      )
    )
    #   )
    # )
  )


# SERVER CODE ####
server <- function(input, output, session) {
  
  # timeout message ####
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  load_data()
  
  # shinycode to disable the first tab of the navpanel - to use as title ####
  toggleClass(condition = TRUE,
              # condition = input$foo,
              class = "disabled",
              selector = "#navbar li a[data-value=tab2]")
  
  # help labels ####
  output$sel_enable_help_text <- renderUI({
    checkboxInput(
      "enable_help_txt",
      label = "Help Text",
      value = TRUE
    )
  })
  
  observe({
    req(input$enable_help_txt)
    source("code/popup_label_descriptions.R", local=TRUE)
  })
  
  # reset button ####
  output$btn_reset_input <- renderUI({
    actionButton("reset_input","Reset Form",class="btn-danger")
  })
  
  observeEvent(input$reset_input, {
    
    updateSelectInput(session, "flt_response_prong21", selected = "Number of respondents")
    updateSelectInput(session, "flt_group1_prong21", selected = "Q1A_Years_Applied")
    updateSelectInput(session, "flt_group1_prong21_flt", selected = "")
    updateSelectInput(session, "flt_group2_prong21", selected = "")
    updateSelectInput(session, "flt_group2_prong21_flt", selected = "")
    updateSelectInput(session, "flt_group3_prong21", selected = "")
    updateSelectInput(session, "flt_group3_prong21_flt", selected = "")
    updateCheckboxInput(session, "flt_stacked", value = FALSE)
    updateCheckboxInput(session, "flt_dropna", value = TRUE)
    updateCheckboxInput(session, "flt_addlabel", value = TRUE)
    updateSelectInput(session, "flt_labelsize", selected = "5")
    
  })
  
  
  # sidebarPanel ####
  
  # __sel_survey ####
  output$sel_survey <- renderUI({
    selectInput("flt_survey",
                h4("Survey Name"),
                choices = c("Pronghorn Opinion Survey")
    )
  })
  
  # __btn_download_survey ####
  output$btn_download_survey <- renderUI({
    downloadButton("download_survey",label="Survey",style="background-color:#375A7F; border-color:#375A7F;")
  })
  
  
  # __btn_download_report ####
  output$btn_download_report <- renderUI({
    downloadButton("download_report",label="Report",style="background-color:#375A7F; border-color:#375A7F;")
  })
  
  # __sel_response_prong21 ####
  output$sel_response_prong21 <- renderUI({
    selectInput("flt_response_prong21",
                "What would you like to calculate?",
                choices = c("Number of respondents","Mean response value"))
  })
  calc_mean <- reactive(ifelse(input$flt_response_prong21 == "Mean response value", TRUE, FALSE))
  
  # __sel_group1_prong21_title ####
  output$sel_group1_prong21_title <- renderUI({
    req(input$flt_response_prong21)
    if(input$flt_response_prong21 == "Number of respondents"){
      h5("X-axis")
    } else {
      h5("Response Variable (Y-axis)")
    }
  })
  
  # __sel_group1_prong21 ####
  output$sel_group1_prong21 <- renderUI({
    req(input$flt_response_prong21)
    tbl <- prong21$dt_summary_group_choices
    if(input$flt_response_prong21 == "Number of respondents"){
      choices <- setNames(as.list(tbl$Name),as.list(tbl$NewLabel)) 
    } else {
      Q_means_names <- tbl$Name[grepl(paste0(prong21$Q_means,collapse="|"),tbl$Name)]
      Q_means_wt_names <- tbl$Name[grepl(paste0(prong21$Q_means_wt,collapse="|"),tbl$Name)]
      tbl_flt <- tbl %>%
        dplyr::filter(Name %in% c(Q_means_names,Q_means_wt_names))
      choices <- setNames(as.list(tbl_flt$Name),as.list(tbl_flt$NewLabel))
    }
    return(
      selectInput("flt_group1_prong21",
                  NULL,
                  choices = c("",choices),
                  selected = choices[1])
    )
  })
  group1_prong21 <- reactive({
    # print(input$flt_group1_prong21)
    return(prong21$dt_summary_group_choices$NewLabel[which(prong21$dt_summary_group_choices$Name %in% input$flt_group1_prong21)])
  })
  
  # __sel_group1_prong21_flt ####
  output$sel_group1_prong21_flt <- renderUI({
    
    req(input$flt_response_prong21 == "Number of respondents")
    
    trans <- prong21$dt_translation %>%
      dplyr::filter(Name %in% input$flt_group1_prong21) %>%
      dplyr::select(Code, ValuePlot)
    
    choices <- setNames(as.list(trans$Code),as.list(trans$ValuePlot))
    
    selectInput("flt_group1_prong21_flt",
                # h5("filter group 1...", style="font-weight: 400;"),
                NULL,
                choices = c("",choices),
                selected = "",
                multiple = TRUE
    )
    
  })
  
  # __sel_group2_prong21_title ####
  output$sel_group2_prong21_title <- renderUI({
    req(input$flt_response_prong21)
    req(input$flt_group1_prong21)
    if(input$flt_response_prong21 == "Number of respondents"){
      h5("Color Bars")
    } else {
      h5("X-axis")
    }
  })
  
  # __sel_group2_prong21 ####
  output$sel_group2_prong21 <- renderUI({
    req(input$flt_group1_prong21)
    tbl <- prong21$dt_summary_group_choices
    tbl_flt <- tbl %>%
      dplyr::filter(!Name %in% input$flt_group1_prong21)
    choices <- setNames(as.list(tbl_flt$Name),as.list(tbl_flt$NewLabel))
    selectInput("flt_group2_prong21",
                NULL,
                choices = c("",choices),
                selected = "")
  })
  group2_prong21 <- reactive({
    return(prong21$dt_summary_group_choices$NewLabel[prong21$dt_summary_group_choices$Name %in% input$flt_group2_prong21])
  })
  
  # __sel_group2_prong21_flt ####
  output$sel_group2_prong21_flt <- renderUI({
    
    req(input$flt_group1_prong21)
    
    trans <- prong21$dt_translation %>%
      dplyr::filter(Name %in% input$flt_group2_prong21) %>%
      dplyr::select(Code, ValuePlot)
    
    choices <- setNames(as.list(trans$Code),as.list(trans$ValuePlot))
    
    selectInput("flt_group2_prong21_flt",
                # h5("filter group 2...", style="font-weight: 400;"),
                NULL,
                choices = c("",choices),
                selected = "",
                multiple = TRUE
    )
    
  })
  
  # __sel_group3_prong21_title ####
  output$sel_group3_prong21_title <- renderUI({
    req(input$flt_response_prong21)
    req(input$flt_group2_prong21)
    if(input$flt_response_prong21 == "Number of respondents"){
      h5("Facet")
    } else {
      h5("Color Bars")
    }
  })
  
  # __sel_group3_prong21 ####
  output$sel_group3_prong21 <- renderUI({
    req(input$flt_group1_prong21, input$flt_group2_prong21)
    tbl <- prong21$dt_summary_group_choices
    tbl_flt <- tbl %>%
      dplyr::filter(!Name %in% c(input$flt_group1_prong21,input$flt_group2_prong21))
    choices <- setNames(as.list(tbl_flt$Name),as.list(tbl_flt$NewLabel))
    selectInput("flt_group3_prong21",
                NULL,
                choices = c("",choices),
                selected = "")
  })
  group3_prong21 <- reactive({
    return(prong21$dt_summary_group_choices$NewLabel[prong21$dt_summary_group_choices$Name %in% input$flt_group3_prong21])
  })
  
  # __sel_group3_prong21_flt ####
  output$sel_group3_prong21_flt <- renderUI({
    
    req(input$flt_group2_prong21)
    
    trans <- prong21$dt_translation %>%
      dplyr::filter(Name %in% input$flt_group3_prong21) %>%
      dplyr::select(Code, ValuePlot)
    
    choices <- setNames(as.list(trans$Code),as.list(trans$ValuePlot))
    
    selectInput("flt_group3_prong21_flt",
                # h5("filter group 3...", style="font-weight: 400;"),
                NULL,
                choices = c("",choices),
                selected = "",
                multiple = TRUE
    )
    
  })
  
  # __sel_stacked ####
  output$sel_stacked <- renderUI({
    checkboxInput(
      "flt_stacked",
      "Stack Bars",
      value = FALSE
    )
  })
  
  # __sel_dropna ####
  output$sel_dropna <- renderUI({
    checkboxInput(
      "flt_dropna",
      "Drop NAs",
      value = TRUE
    )
  })
  
  # __sel_addlabel ####
  output$sel_addlabel <- renderUI({
    checkboxInput(
      "flt_addlabel",
      "Add Labels",
      value = FALSE
    )
  })
  
  # __sel_labelsize ####
  output$sel_labelsize <- renderUI({
    req(input$flt_addlabel)
    numericInput(
      "flt_labelsize",
      h5("Label Size", style="font-weight: 400;"),
      value = label_size,
      min = 1,
      max = 10,
      step =0.5
    )
  })
  
  # __download_survey ####
  
  output$download_survey <- downloadHandler(
    filename = function(){
      paste(input$flt_survey,".pdf", sep="")
    },
    content = function(file) {
      file_path <- switch(input$flt_survey,
                          "Pronghorn Opinion Survey" = "www/survey/qnr_idfg_pronghorn_2021_FINAL.pdf"
      )
      file.copy(file_path,file)
    },
    contentType = "application/pdf"
  )
  
  # __download_report ####
  
  output$download_report <- downloadHandler(
    filename = function(){
      paste(input$flt_survey," Report.pdf", sep="")
    },
    content = function(file) {
      file_path <- switch(input$flt_survey,
                          "Pronghorn Opinion Survey" = "www/report/rprt_idfg_pronghorn_2021_TOPLINE-SUMMARY.pdf"
      )
      file.copy(file_path,file)
    },
    contentType = "application/pdf"
  )
  
  # mainPanel ####
  
  # _Summary ####
  
  # __dat_trans_prong21 ####
  dat_trans_prong21 <- reactive({
    req(input$flt_group1_prong21)
    
    group1_filter <- if(isTruthy(input$flt_group1_prong21_flt)) {input$flt_group1_prong21_flt} else {NA}
    group2_filter <- if(isTruthy(input$flt_group2_prong21_flt)) {input$flt_group2_prong21_flt} else {NA}
    group3_filter <- if(isTruthy(input$flt_group3_prong21_flt)) {input$flt_group3_prong21_flt} else {NA}
    
    xstar <- fnc_dat_trans_prong21(
      dt_translation = prong21$dt_translation,
      dat = prong21$DAT,
      group1 = input$flt_group1_prong21,
      group1_filter = group1_filter,
      group2 = ifelse(!isTruthy(input$flt_group2_prong21), NA, input$flt_group2_prong21),
      group2_filter = group2_filter,
      group3 = ifelse(!isTruthy(input$flt_group3_prong21), NA, input$flt_group3_prong21),
      group3_filter = group3_filter,
      DAT_Unit = prong21$DAT_Unit,
      DAT_MHRUnit = prong21$DAT_MHRUnit,
      Q_means = prong21$Q_means,
      Q_means_wt = prong21$Q_means_wt,
      calc_mean = calc_mean()
    )
    print(xstar)
  })
  
  
  # __summary_prong21 ####
  output$summary_prong21 <- DT::renderDataTable({
    
    req(dat_trans_prong21())
    req(input$flt_group1_prong21)
    
    tbl_trans <- tidyr::tribble(~from, ~to,
                                "wt_avg", "Mean Response",
                                "wt_avg_sd", "SD",
                                "avg", "Mean Response",
                                "avg_sd", "SD",
                                "n", "Number of Respondents",
                                "percent","Percent (%)")
    
    tbl <- dat_trans_prong21()$dat_tbl_trans %>%
      dplyr::mutate_at(vars(-!!sym(dat_trans_prong21()$response),-dplyr::any_of("percent")),as.factor)
    
    for(i in 1:ncol(tbl)){
      names(tbl)[i] <- ifelse(names(tbl)[i] %in% tbl_trans$from, tbl_trans$to[match(names(tbl)[i],tbl_trans$from)], names(tbl)[i])
    }

    dt_tbl <- DT::datatable(tbl,
                            filter = "top",
                            selection = "single",
                            rownames=FALSE,
                            extensions = 'Buttons',
                            escape = FALSE,
                            # style = "bootstrap",
                            options = list(
                              stateSave = TRUE,
                              dom='Blfrtip',
                              buttons=list(
                                "copy",
                                list(extend = "csv",
                                     filename = paste0("summary_data_prong21-", Sys.Date()))),
                              pageLength = 50,
                              lengthMenu = list(c(5,10,50,100,-1),list("5","10","50","100","All")),
                              paging=T
                            ))
    
    return(dt_tbl)
    
  })
  
  # __plot_prong21 ####
  output$plot_prong21 <- renderPlot({
    req(input$flt_group1_prong21)
    req(dat_trans_prong21())
    
    drop_na <- ifelse(isTruthy(input$flt_dropna), input$flt_dropna, FALSE)
    stacked <- ifelse(isTruthy(input$flt_stacked), input$flt_stacked, FALSE)
    add_text <- ifelse(isTruthy(input$flt_addlabel), input$flt_addlabel, FALSE)
    label_size <- ifelse(isTruthy(input$flt_labelsize), input$flt_labelsize, label_size)
    
    fnc_plot_prong21(
      dat = dat_trans_prong21()$dat_plot_trans,
      response = dat_trans_prong21()$response,
      response_label = dat_trans_prong21()$response_label,
      stacked = stacked,
      all_x = all_x,
      drop_na = drop_na,
      bar_border_color = bar_border_color,
      flt_palette = flt_palette,
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      label_text_size = label_text_size,
      bar_fill_color = bar_fill_color,
      facet_title_size = facet_title_size,
      error_bar = ifelse(is.na(dat_trans_prong21()$response_sd),FALSE,TRUE),
      response_sd = dat_trans_prong21()$response_sd,
      facet_wrap = TRUE,
      add_text = add_text,
      label_size = label_size
    )
  })
  
  # _Data ####
  
  # __survey_data_prong21 ####
  output$survey_data_prong21 <- DT::renderDataTable({
    
    req(input$flt_survey == "Pronghorn Opinion Survey")
    
    tbl <- prong21$DAT %>%
      dplyr::select(-ID_IDFG) %>%
      dplyr::mutate(dplyr::across(everything(),as.factor))
    
    
    dt_tbl <- DT::datatable(tbl,
                            filter = "top",
                            selection = "single",
                            rownames=FALSE,
                            extensions = 'Buttons',
                            # style = "bootstrap",
                            options = list(
                              stateSave = TRUE,
                              dom='Blfrtip',
                              buttons=list(
                                "copy",
                                list(extend = "csv",
                                     filename = paste0("survey_data_prong21-", Sys.Date()))),
                              scrollX = TRUE,
                              pageLength = 50,
                              lengthMenu = list(c(5,10,50,100,-1),list("5","10","50","100","All")),
                              paging=T
                            ))
    
    return(dt_tbl)
    
  })
  
  # _Codebook ####
  
  # __survey_codebook_prong21 ####
  output$survey_codebook_prong21 <- DT::renderDataTable({
    
    req(input$flt_survey == "Pronghorn Opinion Survey")
    
    tbl <- prong21$dt_translation %>%
      dplyr::mutate(dplyr::across(everything(),as.factor))
    
    dt_tbl <- DT::datatable(tbl,
                            filter = "top",
                            selection = "single",
                            rownames=FALSE,
                            extensions = 'Buttons',
                            # style = "bootstrap",
                            options = list(
                              stateSave = TRUE,
                              dom='Blfrtip',
                              buttons=list(
                                "copy",
                                list(extend = "csv",
                                     filename = paste0("survey_data_prong21-", Sys.Date()))),
                              scrollX = TRUE,
                              pageLength = -1,
                              lengthMenu = list(c(5,10,50,100,-1),list("5","10","50","100","All")),
                              paging=T
                            ))
    
    return(dt_tbl)
    
  })
  
  # VISIT COUNTER ####
  
  # fingerprint_code
  output$testtext <- renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))
  
  # fingerprint_code
  output$counter <- renderText({
    appname <- "OpinionSurveyApp"
    df <- session$clientData
    if(!file.exists(paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))){
      # if(!file.exists(paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))){
      df <- session$clientData
      counter <- 0
      datetime <- Sys.time()
      fingerprint <- input$fingerprint
      ipid <- input$ipid
      df_counter <- data.frame(datetime = datetime,
                               appname = appname,
                               fingerprint = fingerprint,
                               ipid = ipid)
      df_counter <- df_counter %>%
        dplyr::filter(fingerprint != "" & ipid != "")
      if(nrow(df_counter)>=1){
        df_counter$counter <- 1:nrow(df_counter)
      }
    } else {
      load(file=paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))
      # load(file=paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))
      df <- session$clientData
      counter <- df_counter$counter + 1
      datetime <- Sys.time()
      fingerprint <- input$fingerprint
      ipid <- input$ipid
      df_counter_new <- data.frame(counter = min(counter),
                                   datetime = datetime,
                                   appname = appname,
                                   fingerprint = fingerprint,
                                   ipid = ipid)
      df_counter_new <- df_counter_new %>%
        dplyr::filter(fingerprint != "" & ipid != "")
      df_counter <- dplyr::bind_rows(df_counter,df_counter_new)
      if(nrow(df_counter)>=1){
        df_counter$counter <- 1:nrow(df_counter)
      }
    }
    save(list=c("df_counter","counter"), file=paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))
    return(paste("Hits: ",!file.exists(paste("/srv/shiny-server/EAR/userdata/",appname,"_counter.RData",sep=""))))
    # save(list=c("df_counter","counter"), file=paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))
    # return(paste("Hits: ",!file.exists(paste("K:/Wildlife/Shiny_Apps/EAR/userdata/",appname,"_counter.RData",sep=""))))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
