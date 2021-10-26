fnc_dat_trans_prong21 <- function(
  dt_translation = dt_translation,
  dat = dat,
  group1 = group1,
  group1_filter = group1_filter,
  group2 = group2,
  group2_filter = group2_filter,
  group3 = group3,
  group3_filter = group3_filter,
  DAT_Unit = DAT_Unit,
  DAT_MHRUnit = DAT_MHRUnit,
  Q_means = Q_means,
  Q_means_wt = Q_means_wt,
  calc_mean = FALSE
)
{
  
  Q_means_names <- names(dat)[grepl(paste0(Q_means,collapse="|"),names(dat))]
  Q_means_wt_names <- names(dat)[grepl(paste0(Q_means_wt,collapse="|"),names(dat))]
  
  group1_entry <- all(!is.na(group1))
  group1_filter_entry <- all(!is.na(group1_filter))
  group2_entry <- all(!is.na(group2))
  group2_filter_entry <- all(!is.na(group2_filter))
  group3_entry <- all(!is.na(group3))
  group3_filter_entry <- all(!is.na(group3_filter))
  
  if(group1_entry & group2_entry & group3_entry){
    group <- c(group1, group2, group3)
  } else {
    if(group1_entry & group2_entry){
      group <- c(group1, group2)
    } else {
      group <- group1
    }
  }
  
  if("Q9_Unit" %in% group){
    dat <- dat %>%
      dplyr::left_join(DAT_Unit)
  }
  
  if("MHR_UNIT" %in% group){
    dat <- dat %>%
      dplyr::left_join(DAT_MHRUnit)
  }
  
  if(calc_mean){
    if(group1 %in% Q_means_names){
      group_adj <- group[which(!group %in% group1)]
      dat_plot <- dat %>%
        { if(length(group_adj)>=1) dplyr::group_by_at(., group_adj) else . } %>%
        dplyr::summarise(avg = round(mean(as.numeric(!!rlang::sym(group1)), na.rm=T), digits=1),
                         avg_sd = round(sd(as.numeric(!!rlang::sym(group1)), na.rm=T), digits=1)) %>%
        { if(length(group_adj)>=1) . else dplyr::mutate(., !!group1 := "Overall") } %>%
        { if(group1_filter_entry) dplyr::filter(., !!rlang::sym(group1) %in% group1_filter) else .} %>%
        { if(group2_filter_entry) dplyr::filter(., !!rlang::sym(group2) %in% group2_filter) else .} %>%
        { if(group3_filter_entry) dplyr::filter(., !!rlang::sym(group3) %in% group3_filter) else .} %>%
        { if(group2_entry) dplyr::mutate(., !!rlang::sym(group2) := as.factor(!!rlang::sym(group2))) else .} %>%
        { if(group3_entry) dplyr::mutate(., !!rlang::sym(group3) := as.factor(!!rlang::sym(group3))) else .} %>%
        na.omit() %>%
        dplyr::select(-avg,-avg_sd,avg,avg_sd)
    } else {
      group_adj <- group[which(!group %in% group1)]
      dat_plot <- dat %>%
        { if(group1_filter_entry) dplyr::filter(., !!rlang::sym(group1) %in% group1_filter) else .} %>%
        { if(group2_filter_entry) dplyr::filter(., !!rlang::sym(group2) %in% group2_filter) else .} %>%
        { if(group3_filter_entry) dplyr::filter(., !!rlang::sym(group3) %in% group3_filter) else .} %>%
        { if(group2_entry) dplyr::mutate(., !!rlang::sym(group2) := as.factor(!!rlang::sym(group2))) else .} %>%
        { if(group3_entry) dplyr::mutate(., !!rlang::sym(group3) := as.factor(!!rlang::sym(group3))) else .} %>%
        { if(length(group_adj)>=1) dplyr::group_by_at(., group_adj) else .} %>%
        dplyr::mutate(wt_avg_sd = sd(as.numeric(!!rlang::sym(group1)),na.rm=T)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by_at(c(group, "wt_avg_sd"))  %>%
        dplyr::summarise(n = dplyr::n_distinct(ID_Qualtrics)) %>%
        na.omit() %>%
        dplyr::mutate(wt_sum = as.numeric(!!rlang::sym(group1))*n) %>%
        dplyr::ungroup() %>%
        { if(length(group_adj)>=1) dplyr::select(., -(!!rlang::sym(group1))) else dplyr::mutate(., !!group1 := "Overall") } %>%
        { if(length(group_adj)>=1) dplyr::group_by_at(., group_adj) else dplyr::group_by_at(., group) } %>%
        dplyr::summarise(n = sum(n),
                         wt_sum = sum(wt_sum),
                         wt_avg = round(wt_sum/n, digits=1),
                         wt_avg_sd = round(mean(wt_avg_sd), digits=1)) %>%
        dplyr::select(-n,-wt_sum)
    } 
  } else {
    dat_plot <- dat %>%
      dplyr::group_by_at(group)  %>%
      dplyr::summarise(n = dplyr::n_distinct(ID_Qualtrics)) %>%
      { if(group1_filter_entry) dplyr::filter(., !!rlang::sym(group1) %in% group1_filter) else .} %>%
      { if(group2_filter_entry) dplyr::filter(., !!rlang::sym(group2) %in% group2_filter) else .} %>%
      { if(group3_filter_entry) dplyr::filter(., !!rlang::sym(group3) %in% group3_filter) else .} %>%
      { if(group2_entry) dplyr::mutate(., !!rlang::sym(group2) := as.factor(!!rlang::sym(group2))) else .} %>%
      { if(group3_entry) dplyr::mutate(., !!rlang::sym(group3) := as.factor(!!rlang::sym(group3))) else .} %>%
      na.omit()
  }
  
  if(calc_mean){
    if(group1 %in% Q_means_names){
      response <- "avg"
      response_sd <- "avg_sd"
      response_label <- dt_translation[which(dt_translation$Name == group1)]
    } else {
      response <- "wt_avg"
      response_sd <- "wt_avg_sd"
      response_label <- unique(dt_translation$Label[which(dt_translation$Name == group1)])
    }
  } else {
    response <- "n"
    response_sd <- NA
    response_label <- NA
  }
  
  dat_plot_trans <- dat_tbl_trans <- dat_plot
  if(group1 %in% Q_means_names){
    focus_col <- names(dat_plot)[which(!names(dat_plot) %in% c("Q9_Unit","MHR_UNIT",response,response_sd,group1))]
  } else {
    focus_col <- names(dat_plot)[which(!names(dat_plot) %in% c("Q9_Unit","MHR_UNIT",response,response_sd))]
  }
  if(length(focus_col) >= 1){
    if(nrow(dat_plot_trans)>1){
      for(k in 1:length(focus_col)){
        col_sel <- focus_col[k]
        tbl_trans <- dt_translation[which(dt_translation$Name == col_sel),c("Code","Value")] %>%
          dplyr::arrange(Code)
        tbl_trans_plot <- dt_translation[which(dt_translation$Name == col_sel),c("Code","ValuePlot")] %>%
          dplyr::arrange(Code)
        if(all(is.na(tbl_trans$Value))){
          if(all(dat_tbl_trans[,col_sel] == "Overall")){
            dat_tbl_trans[,col_sel] <- dat_tbl_trans[,col_sel]
            dat_plot_trans[,col_sel] <- dat_plot_trans[,col_sel]
          } else {
            dat_tbl_trans[,col_sel] <- as.factor(as.numeric(as.character(unlist(dat_tbl_trans[,col_sel]))))
            dat_plot_trans[,col_sel] <- as.factor(as.numeric(as.character(unlist(dat_plot_trans[,col_sel]))))
          } 
        } else {
          new_col_trans <- tbl_trans$Value[match(unlist(dat_plot[,col_sel]),tbl_trans$Code)]
          new_col_trans_plot <- tbl_trans_plot$ValuePlot[match(unlist(dat_plot[,col_sel]),tbl_trans_plot$Code)]
          dat_tbl_trans[,col_sel] <- factor(new_col_trans,levels=tbl_trans$Value,labels = tbl_trans$Value)
          dat_plot_trans[,col_sel] <- factor(new_col_trans_plot,levels=tbl_trans_plot$ValuePlot,labels = tbl_trans_plot$ValuePlot)
        }
      }
    }
  }
  tbl_name_trans <- dplyr::distinct(dt_translation[which(dt_translation$Name %in% group),c("Name","Label")])
  
  for(i in 1:ncol(dat_plot_trans)){
    col_sel <- names(dat_plot_trans)[i]
    if(col_sel %in% Q_means_names & col_sel %in% group1){
      if(all(dat_tbl_trans[,col_sel] == "Overall")){
        dat_tbl_trans[,col_sel] <- dat_tbl_trans[,col_sel]
        dat_plot_trans[,col_sel] <- dat_plot_trans[,col_sel]
      } else {
        dat_tbl_trans[,col_sel] <- as.factor(as.numeric(as.character(unlist(dat_tbl_trans[,col_sel]))))
        dat_plot_trans[,col_sel] <- as.factor(as.numeric(as.character(unlist(dat_plot_trans[,col_sel]))))
      }
    }
  }
  
  for(i in 1:ncol(dat_plot_trans)){
    names(dat_tbl_trans)[i] <- ifelse(is.na(match(names(dat_tbl_trans)[i],tbl_name_trans$Name)), names(dat_tbl_trans)[i], tbl_name_trans$Label[match(names(dat_tbl_trans)[i],tbl_name_trans$Name)])
    names(dat_plot_trans)[i] <- ifelse(is.na(match(names(dat_plot_trans)[i],tbl_name_trans$Name)), names(dat_plot_trans)[i], tbl_name_trans$Label[match(names(dat_plot_trans)[i],tbl_name_trans$Name)])
  }
  
  #add percent
  if("n" %in% names(dat_tbl_trans)){
    if(!is.na(group1) & is.na(group2) & is.na(group3) & !group1_filter_entry){
      dat_tbl_trans$percent <- round(dat_tbl_trans$n/sum(dat_tbl_trans$n)*100, digits = 1)
      dat_plot_trans$percent <- round(dat_plot_trans$n/sum(dat_plot_trans$n)*100, digits = 1)
    }
  }
  
  return(list(dat_plot=dat_plot,
              dat_tbl_trans = dat_tbl_trans,
              dat_plot_trans = dat_plot_trans,
              response = response,
              response_sd = response_sd,
              response_label = response_label))
  
}

fnc_plot_prong21 <- function(
  dat = dat,
  response = n,
  response_label = response_label,
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
  error_bar = error_bar,
  response_sd = response_sd,
  facet_wrap = facet_wrap,
  add_text = add_text,
  label_size = label_size
)
{
  
  if(drop_na){
    dat <- na.omit(dat)
  } else {
    dat <- dat
  }
  groups <- names(dat)[which(!names(dat) %in% c(response,response_sd,"percent"))]
  ngroups <- length(groups)
  
  if(ngroups == 3){
    
    if(facet_wrap){
      
      new_col <- groups[2]
      dat_plot_new <- dat
      dat_plot_new <- dat_plot_new %>%
        dplyr::mutate(label = !!rlang::sym(response))
      
      plt1 <- ggplot2::ggplot(data = dat_plot_new, ggplot2::aes(x = !!rlang::sym(groups[1]), y = !!rlang::sym(response), group = !!rlang::sym(new_col), fill = !!rlang::sym(new_col))) +
        ggplot2::facet_wrap(vars(!!rlang::sym(groups[3])))
      
      numlevels <- dat_plot_new %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!rlang::syms(new_col)) %>%
        dplyr::distinct() %>%
        nrow()
      
      numlevels_x <- dat_plot_new %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!rlang::syms(groups[1])) %>%
        dplyr::distinct() %>%
        nrow()
      
      # mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, flt_palette))(numlevels)
      mycolors <- grDevices::colorRampPalette(ggthemes::economist_pal()(9))(numlevels)  
      
    } else {
      
      new_col <- paste0(groups[2]," - ",groups[3])
      
      
      dat_plot_new <- dat %>%
        tidyr::unite(col="group2plot",!!rlang::sym(groups[2]), !!rlang::sym(groups[3]), sep = " - ", remove=FALSE) %>%
        dplyr::rename(!!new_col := group2plot)
      
      plt1 <- ggplot2::ggplot(data = dat_plot_new, ggplot2::aes(x = !!rlang::sym(groups[1]), y = !!rlang::sym(response), group = !!rlang::sym(new_col), fill = !!rlang::sym(new_col)))
      
      numlevels <- dat_plot_new %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!rlang::syms(new_col)) %>%
        dplyr::distinct() %>%
        nrow()
      
      numlevels_x <- dat_plot_new %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!rlang::syms(groups[1])) %>%
        dplyr::distinct() %>%
        nrow()
      
      # mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, flt_palette))(numlevels)  
      mycolors <- grDevices::colorRampPalette(ggthemes::economist_pal()(9))(numlevels)  
      
    }
    
  }
  
  if(ngroups == 2){
    
    new_col <- groups[2]
    dat_plot_new <- dat
    dat_plot_new <- dat_plot_new %>%
      dplyr::mutate(label = !!rlang::sym(response))
    
    plt1 <- ggplot2::ggplot(data = dat_plot_new, ggplot2::aes(x = !!rlang::sym(groups[1]), y = !!rlang::sym(response), group = !!rlang::sym(new_col), fill = !!rlang::sym(new_col)))
    
    numlevels <- dat_plot_new %>%
      dplyr::ungroup() %>%
      dplyr::select(!!!rlang::syms(new_col)) %>%
      dplyr::distinct() %>%
      nrow()
    
    numlevels_x <- dat_plot_new %>%
      dplyr::ungroup() %>%
      dplyr::select(!!!rlang::syms(groups[1])) %>%
      dplyr::distinct() %>%
      nrow()
    
    # mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, flt_palette))(numlevels)
    mycolors <- grDevices::colorRampPalette(ggthemes::economist_pal()(9))(numlevels)  
    
  }
  
  if(ngroups == 1){
    
    new_col <- groups[1]
    dat_plot_new <- dat
    
      if("percent" %in% names(dat_plot_new)){
        dat_plot_new$label <- paste0(dat_plot_new$n, " (",dat_plot_new$percent,"%)")
      } else {
        dat_plot_new <- dat_plot_new %>%
          dplyr::mutate(label = !!rlang::sym(response))
      }
    
      plt1 <- ggplot2::ggplot(data = dat_plot_new, ggplot2::aes(x = !!rlang::sym(groups[1]), y = !!rlang::sym(response)))
      
      numlevels <- 1
      
      numlevels_x <- dat_plot_new %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!rlang::syms(groups[1])) %>%
        dplyr::distinct() %>%
        nrow()
      
    }
    
  print(dat_plot_new)
    y_lab <- if(response == "n") {"Number of Respondents"} else {paste0(response_label,"\n (Mean Response)")}
    
    plt <- plt1 +
      {if(stacked) {if(numlevels < 2) ggplot2::geom_bar(stat="identity", position = "stack", color = bar_border_color, fill = bar_fill_color, lwd = 1) else ggplot2::geom_bar(stat="identity", position = "stack", color = bar_border_color, lwd = 1)}} +
      {if(stacked == FALSE) {if(numlevels < 2) ggplot2::geom_bar(stat="identity", position = ggplot2::position_dodge(), fill = bar_fill_color, color = bar_border_color, lwd = 1) else ggplot2::geom_bar(stat="identity", position = ggplot2::position_dodge(), color = bar_border_color, lwd = 1)}} +
      {if(stacked == FALSE) {if(error_bar) ggplot2::geom_errorbar(ggplot2::aes(ymin=!!rlang::sym(response), ymax=!!rlang::sym(response)+(!!rlang::sym(response_sd))), width=.2, position=ggplot2::position_dodge(.9))}} +
      # {if(numlevels >= 2) {if(numlevels <= 9) ggplot2::scale_fill_brewer(palette=flt_palette) else ggplot2::scale_fill_manual(values = mycolors)}} +
      {if(numlevels >= 2) {if(numlevels <= 9) ggthemes::scale_fill_economist() else ggplot2::scale_fill_manual(values = mycolors)}} +
      ggplot2::theme_minimal() +
      ggplot2::xlab(groups[1]) +
      ggplot2::ylab(y_lab) +
      {if(numlevels_x > 5) ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) } +
      ggplot2::theme(legend.position = "bottom",
                     legend.box = "horizontal",
                     legend.background = ggplot2::element_rect(fill="#ECECEC", color="#ECECEC", size=0.5, linetype="solid"),
                     axis.text=ggplot2::element_text(size=axis_text_size),
                     axis.text.x=ggplot2::element_text(margin=margin(t=0,r=0,b=20,l=0)),
                     axis.text.y=ggplot2::element_text(margin=margin(t=0,r=0,b=0,l=20)),
                     axis.title=ggplot2::element_text(size=axis_title_size,face="bold"),
                     legend.title=ggplot2::element_text(size=legend_title_size,face="bold"),
                     legend.text=ggplot2::element_text(size=legend_text_size),
                     strip.text=ggplot2::element_text(size=facet_title_size)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title.position="top", title.hjust = 0.5)) +
      {if(all_x == FALSE) ggplot2::scale_x_discrete(drop=FALSE)}
    
    if(add_text){
      if(stacked == FALSE) {
        plt <- plt +
          geom_text(aes(label=label), position=ggplot2::position_dodge2(width=0.9), vjust = -1, color = "black", size=label_size, show.legend = FALSE)
      } else {
        plt <- plt +
          geom_text(aes(label=label),
                    position=ggplot2::position_stack(vjust=0.5),
                    size=label_size,
                    colour = map_chr(
                      ggplot_build(plt)[[1]][[1]]$fill,
                      coloratio::cr_choose_bw
                    ), show.legend = FALSE) +
          stat_summary(fun.y = sum, aes(label = ..y.., group = !!rlang::sym(groups[1])), geom = "text", vjust = -1, size = label_size, color = "black",fontface = 2, show.legend = FALSE)
      }
    }
  
  return(plt)
  
}


fnc_relevel_factor_prong21 <- function(
  dat = dat,
  variable = variable,
  new_levels = new_levels
){
  
  newdat <- dat %>%
    dplyr::mutate(!!sym(variable) := as.factor(!!sym(variable))) %>%
    as.data.frame()
  
  levels(newdat[,variable]) <- setNames(new_levels,nm=names(new_levels))
  
  newdat <- dplyr::as_tibble(newdat)
  
  return(newdat)
  
}