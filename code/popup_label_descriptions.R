if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_survey", title = "", content = "Select the survey data you would like to summarize.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_survey")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "btn_download_survey", title = "", content = "Click here to download a pdf of the final version of the survey you have selected.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "btn_download_survey")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "btn_download_report", title = "", content = "Click here to download a pdf of the completed report for the survey you have selected.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "btn_download_report")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "btn_reset_input", title = "", content = "Click here to reset the data summary and formatting selections you have made.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "btn_reset_input")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_stacked", title = "", content = "Check this box to stack colored bar graph groups on top of each other.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_stacked")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_dropna", title = "", content = "Check this box to drop responses of NAs (essentially people who didn't answer a question) from the data summaries.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_dropna")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_addlabel", title = "", content = "Check this box to add data labels to the graph.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_addlabel")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_labelsize", title = "", content = "Increase or decrease the size of data labels on the graph.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_labelsize")
}

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_enable_help_text", title = "", content = "Check this box to see popup help text.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_enable_help_text")
}

# Pronghorn Survey ####

if(input$enable_help_txt){
  shinyBS::addPopover(session, "sel_response_prong21", title = "", content = "Select whether you would like to summarize survey data by calculating the number of respondents by a group or calculating a mean response value.", trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_response_prong21")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    content <- "Select the first survey category or question you would like to summarize survey data based on."
    title <- "X-axis"
  } else {
    content <- "Select the survey question/category you would like to calculate the mean values for."
    title <- "Y-axis"
  }
  shinyBS::addPopover(session, "sel_group1_prong21", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group1_prong21")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    content <- "Filter the first survey category or question to the levels you are interested in."
    title <- "X-axis"
  } else {
    content <- ""
    title <- ""
  }
  shinyBS::addPopover(session, "sel_group1_prong21_flt", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group1_prong21_flt")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    title <- "Color Bars"
    content <- "Select the second survey category or question you would like to summarize survey data based on."
  } else {
    title <- "X-axis"
    content <- "Select the first survey category or question you would like to summarize survey data based on."
  }
  shinyBS::addPopover(session, "sel_group2_prong21", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group2_prong21")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    content <- "Filter the second survey category or question to the levels you are interested in."
    title <- "Color Bars"
  } else {
    content <- "Filter the second survey category or question to the levels you are interested in."
    title <- "X-axis"
  }
  shinyBS::addPopover(session, "sel_group2_prong21_flt", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group2_prong21_flt")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    title <- "Facet"
    content <- "Select the third survey category or question you would like to summarize survey data based on."
  } else {
    title <- "Color Bars"
    content <- "Select the first survey category or question you would like to summarize survey data based on."
  }
  shinyBS::addPopover(session, "sel_group3_prong21", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group3_prong21")
}

if(input$enable_help_txt){
  req(input$flt_response_prong21)
  if(input$flt_response_prong21 == "Number of respondents"){
    content <- "Filter the third survey category or question to the levels you are interested in."
    title <- "Facet"
  } else {
    content <- "Filter the second survey category or question to the levels you are interested in."
    title <- "Color Bars"
  }
  shinyBS::addPopover(session, "sel_group3_prong21_flt", title = title, content = content, trigger = 'hover', placement = "right", options = list(container = "body"))
} else {
  removePopover(session, "sel_group3_prong21_flt")
}
