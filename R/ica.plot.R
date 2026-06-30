ica.plot <- function(models.list, 
	x.varname, 
	y.varnames = NULL, 
	x.level.names = NULL, 
	exp = FALSE,
	plot.pdf = FALSE, 
	no.screen = FALSE, 
	plot.name = "ica_plot", 
	plot.height = 9, 
	plot.width = 8,
	xlim.coord = NULL, 
	x.ticks = NULL, 
	xaxis.title = NULL, 
	h.coeff.align = NULL, 
	v.coeff.align = -0.3,
	coeff.labels = c("name.p","name.coeff.p","name","full.1.line","full.2.line"),
	signif.col = "red", 
	non.signif.col = "navyblue", 
	right.censor = NULL, 
	left.censor = NULL, 
	main.text = "", 
	axis.title.mag = 1.5, 
	axis.label.mag = 1.5, 
	axis.line.width = 1.5, 
	point.mag = 2, 
	point.type = 18, 
	ci.line.width = 1.5, 
	coeff.labels.mag = 1.25, 
	vertical.line.width = 2,
	plot.margins = c(8.1, 2.1, 4.1, 2.1),
	...){

  coeff.labels = match.arg(coeff.labels)
  
  if(!is.list(models.list) || length(models.list) < 1){
    stop("models.list must be a list containing at least one t23 object.")
  }

  # 1. Validate consistency
  mod.func <- models.list[[1]]$mod.func
  model.family <- models.list[[1]]$model.family
  
  for(i in 1:length(models.list)){
    if(models.list[[i]]$mod.func != mod.func) stop("All models must have the same mod.func")
    if(models.list[[i]]$model.family != model.family) stop("All models must have the same model.family")
  }
  
  # 2. Extract Common Levels
  common_levels <- NULL
  for(i in 1:length(models.list)){
    m <- models.list[[i]]
    coeff_mat <- if(m$mod.type == "univar") m$uni.coeff else m$multi.coeff
    match_idx <- grepl(paste0("^", x.varname), rownames(coeff_mat))
    if(sum(match_idx) == 0) stop(paste("x.varname '", x.varname, "' not found in model", i))
    matched_names <- rownames(coeff_mat)[match_idx]
    if(is.null(common_levels)) common_levels <- matched_names else common_levels <- intersect(common_levels, matched_names)
  }
  
  if(length(common_levels) == 0) stop("No common factor levels found across all models.")
  
  # 3. Handle custom level names
  if(!is.null(x.level.names)){
    if(length(x.level.names) != length(common_levels)) stop("x.level.names must match the number of extracted levels.")
    names(common_levels) <- x.level.names
  } else {
    names(common_levels) <- gsub(paste0("^", x.varname), "", common_levels)
  }
  
  # 4. Build Plot Dataframe (Keeping spatial coordinates in LINEAR log-scale)
  plot_df <- data.frame()
  for(i in 1:length(models.list)){
    m <- models.list[[i]]
    coeff_mat <- if(m$mod.type == "univar") m$uni.coeff else m$multi.coeff
    table_mat <- if(m$mod.type == "univar") m$uni.table else m$multi.table
    
    # Force all plotting coordinates to linear (log) scale for perfect visual symmetry
    if(mod.func == "cox") coeff_mat[,1:3] <- log(coeff_mat[,1:3]) 
    
    y_name <- if(!is.null(y.varnames) && length(y.varnames) >= i) y.varnames[i] else m$y.varname
    
    for(lvl_idx in seq_along(common_levels)){
      lvl <- common_levels[lvl_idx]
      display_name <- names(common_levels)[lvl_idx]
      
      est_plot <- as.numeric(coeff_mat[lvl, 1])
      ci_l_plot <- as.numeric(coeff_mat[lvl, 2])
      ci_u_plot <- as.numeric(coeff_mat[lvl, 3])
      pval <- as.numeric(coeff_mat[lvl, 4])
      
      # Determine what value prints in the label if the user chooses name.coeff.p
      est_disp <- if(exp && mod.func %in% c("logistic", "gee", "cox")) exp(est_plot) else est_plot
      
      plot_df <- rbind(plot_df, data.frame(
        model_idx = i, y_name = y_name, mod_type = m$mod.type, level = display_name,
        est_plot = est_plot, ci_l = ci_l_plot, ci_u = ci_u_plot, est_disp = est_disp, pval = pval,
        table_str = table_mat[lvl, 1], table_pval = table_mat[lvl, 2],
        coeff_digits = m$coeff.digits, trim_digits = m$trim.digits, stringsAsFactors = FALSE
      ))
    }
  }
  
  # 5. Y Positioning
  num_models <- length(models.list)
  y_positions <- numeric(nrow(plot_df))
  current_y <- 1
  gap <- 1.5
  for(i in num_models:1){
    model_rows <- which(plot_df$model_idx == i)
    for(r in rev(model_rows)){
      y_positions[r] <- current_y
      current_y <- current_y + 1
    }
    current_y <- current_y + gap
  }
  plot_df$y_pos <- y_positions
  
  # 6. Transform User Constraints to Log-Scale (if exp=TRUE)
  if(exp == TRUE){
     if(!is.null(xlim.coord)) xlim.coord <- log(xlim.coord)
     if(!is.null(x.ticks)) at.x.ticks <- log(x.ticks)
     if(!is.null(right.censor)) right.censor <- log(right.censor)
     if(!is.null(left.censor)) left.censor <- log(left.censor)
     if(is.numeric(h.coeff.align)) h.coeff.align <- log(h.coeff.align)
  } else {
     if(!is.null(x.ticks)) at.x.ticks <- x.ticks
  }

  # 7. Censoring Limits
  if(!is.null(right.censor)) plot_df$ci_u[plot_df$ci_u > right.censor] <- right.censor
  if(!is.null(left.censor)) plot_df$ci_l[plot_df$ci_l < left.censor] <- left.censor
  
  # 8. Coordinate Alignments
  if(is.null(h.coeff.align)) h.coeff.align <- max(plot_df$ci_u)
  
  if(is.null(xlim.coord) & h.coeff.align=="right.next"){
    x.start <- min(plot_df$ci_l)
    x.stop <- max(plot_df$ci_u)
    xlim.coord <- c(x.start, x.stop + (x.stop-x.start)*1)
  }else if(is.null(xlim.coord) & h.coeff.align=="left.next"){
    x.start <- min(plot_df$ci_l)
    x.stop <- max(plot_df$ci_u)
    xlim.coord <- c(x.start-(x.stop-x.start)*1, x.stop)	
  } else if(is.null(xlim.coord)) {
    xlim.coord <- c(min(plot_df$ci_l), max(plot_df$ci_u))
  }
  
  # 9. X-Axis Title
  if(is.null(xaxis.title)){
    if(exp == TRUE){
       if(mod.func=="logistic" | (mod.func=="gee" & model.family=="binomial")) xaxis.title <- paste("Odds Ratio (95% CI) for", x.varname)
       else if(mod.func=="cox") xaxis.title <- paste("Hazard Ratio (95% CI) for", x.varname)
       else xaxis.title <- paste("Exponentiated Estimate (95% CI) for", x.varname)
    } else {
       if(is.null(x.ticks) & (mod.func=="logistic" | (mod.func=="gee" & model.family=="binomial"))) xaxis.title <- bquote(paste(Delta," log odds (95% CI) for ", .(x.varname),sep="")) 
       else if(mod.func=="cox") xaxis.title <- "log Relative Hazard (95% CI) " 
       else xaxis.title <- bquote(paste(Delta, " ", .(x.varname), " (95% CI) ",sep="")) 
    }
  }
  
  # 10. Render Setup
  if(no.screen==TRUE) pdf(paste(plot.name,"_",format(Sys.time(),"%Y_%m_%d_%H%M"),".pdf",sep=""),height=plot.height,width=plot.width)
  
  par(mar=plot.margins)
  plot(plot_df$est_plot, plot_df$y_pos,
       main=main.text, xlab="", ylab="",
       xlim=xlim.coord, ylim=c(0.5, max(plot_df$y_pos) + gap),
       pch=NA, cex.lab=axis.title.mag, axes=F)
       
  # FIXED: Removed at=0 so the title centers perfectly along the X-axis natively
  mtext(xaxis.title, side=1, line=4.5, cex=axis.title.mag)
  
  if(!is.null(x.ticks)){
    axis(1, at=at.x.ticks, labels=x.ticks, cex.axis=axis.label.mag, lwd=axis.line.width)
  } else {
    axis(1, cex.axis=axis.label.mag, lwd=axis.line.width)
  }
  
  # Null vertical dashed line is at 0 (log(1) = 0)
  abline(v=0, lty="dashed", lwd=vertical.line.width)
  
  # 11. Label Alignments
  if(h.coeff.align=="right.next"){
    h.coeff.align2 <- plot_df$ci_u + 0.05*(max(plot_df$ci_u)-min(plot_df$ci_l))
    text.adj <- 0 
  }else if(h.coeff.align=="left.next"){
    h.coeff.align2 <- plot_df$ci_l - 0.05*(max(plot_df$ci_u)-min(plot_df$ci_l)) 
    text.adj <- 1 
  }else if(is.numeric(h.coeff.align) & length(h.coeff.align)==1){
    h.coeff.align2 <- rep(h.coeff.align, nrow(plot_df))
    text.adj <- ifelse(h.coeff.align<0, 1, 0)
  }else if(is.numeric(h.coeff.align) & length(h.coeff.align)>1){
    h.coeff.align2 <- rep(h.coeff.align, length.out=nrow(plot_df))
    text.adj <- ifelse(min(h.coeff.align)<0, 1, 0)
  }
  
  # 12. Text Strings
  line.names <- character(nrow(plot_df))
  for(i in 1:nrow(plot_df)){
    if(coeff.labels=="name") line.names[i] <- plot_df$level[i]
    else if(coeff.labels=="full.1.line") line.names[i] <- paste(plot_df$level[i], " ", plot_df$table_str[i], ", p=", plot_df$table_pval[i], sep="")
    else if(coeff.labels=="full.2.line") line.names[i] <- paste(plot_df$level[i], "\n", plot_df$table_str[i], ", p=", plot_df$table_pval[i], sep="")
    else if(coeff.labels=="name.p") line.names[i] <- paste(plot_df$level[i], " (p=", plot_df$table_pval[i], ")", sep="")
    else if(coeff.labels=="name.coeff.p") line.names[i] <- paste(plot_df$level[i], " (", format(round(plot_df$est_disp[i], plot_df$coeff_digits[i]), nsmall=plot_df$coeff_digits[i], trim=plot_df$trim_digits[i]), ", p=", plot_df$table_pval[i], ")", sep="")
  }
  
  line.col <- ifelse(plot_df$pval <= 0.05, signif.col, non.signif.col)
  
  # 13. Plotting Loop
  for(i in 1:nrow(plot_df)){
    y <- plot_df$y_pos[i]
    points(plot_df$est_plot[i], y, pch=point.type, cex=point.mag, col=line.col[i])
    lines(c(plot_df$ci_l[i], plot_df$ci_u[i]), c(y, y), lwd=ci.line.width, col=line.col[i]) 
    text(h.coeff.align2[i], (y + v.coeff.align), line.names[i], cex=coeff.labels.mag, adj=text.adj)
  }
  
  # 14. Model Headers
  center_x <- mean(xlim.coord)
  for(i in 1:num_models){
    m_rows <- plot_df[plot_df$model_idx == i, ]
    if(nrow(m_rows) > 0){
      top_row <- m_rows[which.max(m_rows$y_pos), ]
      label_y <- top_row$y_pos + 0.8
      model_label <- paste(top_row$y_name, "(", top_row$mod_type, ")")
      text(center_x, label_y, model_label, font=2, adj=0.5, cex=coeff.labels.mag)
    }
  }
  
  if(plot.pdf==TRUE & no.screen==FALSE){
    dev.copy(pdf, file=paste(plot.name,"_",format(Sys.time(),"%Y_%m_%d"),".pdf",sep=""), height=plot.height, width=plot.width)
    dev.off()
  }
  
  if(no.screen == TRUE) dev.off()
}
