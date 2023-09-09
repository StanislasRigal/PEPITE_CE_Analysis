###########################################################
# A series of functions (helpers) for LC-MNL using gmnl.
# By: Mauricio Sarrias
###########################################################

shares <- function(obj){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  Q <- length(cons_class)
  shares <- exp(cons_class) / sum(exp(cons_class))
  names(shares) <- paste("share q", 1:Q, sep = "=")  
  return(shares)
}


plot_ci_lc <- function(obj, var = NULL, mar = c(2, 5, 2, 2),
                       cex.pts = 0.9, cex.var = 0.8, 
                       var.las = 2, pch.pts = 20, col.pts = 1, ...){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  name.x <- if (is.null(var)) names(obj$mf)[-1] else var
  Q <- length(cons_class)
  lc.names <- c()
  for (i in 1:length(name.x)) {
    lc.names <- c(lc.names, paste("class", 1:Q, name.x[i], 
                                  sep = "."))
  }
  bhat <- bhat[lc.names]
  se   <- se[lc.names]
  
  u <-  bhat + 1.96 * se
  l <-  bhat - 1.96 * se
  n.c <- length(bhat)
  idx <- seq(1, n.c)
  k <- 1 / n.c
  
  par(mar = mar)
  plot(c(l, u), c(idx + k, idx - k), 
       type = "n", axes = F, main = "" , xlab = "", 
       ylab = "", ...)
  axis(3)
  axis(2, n.c:1, names(bhat)[n.c:1], las = var.las, 
       tck = FALSE, lty = 0, cex.axis = cex.var)
  abline(v = 0, lty = 2)
  points(bhat, idx, pch = pch.pts, cex = cex.pts, 
         col = col.pts)
  segments(l, idx, u, idx, lwd = 2, 
           col = "red")
}

plot_ci_lc_ggplot <- function(obj, var = NULL, mar = c(2, 5, 2, 2),
                       cex.pts = 0.9, cex.var = 0.8, 
                       var.las = 2, pch.pts = 20, col.pts = 1, ...){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  name.x <- if (is.null(var)) names(obj$mf)[-1] else var
  Q <- length(cons_class)
  lc.names <- c()
  for (i in 1:length(name.x)) {
    lc.names <- c(lc.names, paste("class", 1:Q, name.x[i], 
                                  sep = "."))
  }
  bhat <- bhat[lc.names]
  se   <- se[lc.names]
  
  u <-  bhat + 1.96 * se
  l <-  bhat - 1.96 * se
  n.c <- length(bhat)
  idx <- seq(1, n.c)
  k <- 1 / n.c
  
  data_to_plot <- data.frame(estimate=bhat,se=se,upper_ci=u,lower_ci=l)
  data_to_plot$class <- substr(row.names(data_to_plot),1,7)
  data_to_plot$class <- paste(toupper(substr(data_to_plot$class, 1, 1)), substr(data_to_plot$class, 2, nchar(data_to_plot$class)), sep="")
  data_to_plot$class2 <- data_to_plot$class 
  data_to_plot$class2[which(data_to_plot$class2=="Class.1")] <- "Saving time"
  data_to_plot$class2[which(data_to_plot$class2=="Class.2")] <- "Protecting biodiversity on its own"
  data_to_plot$class2[which(data_to_plot$class2=="Class.3")] <- "Landscape and nature use"
  data_to_plot$class2[which(data_to_plot$class2=="Class.4")] <- "Trade-off time biodiversity"
  data_to_plot$variable <- substr(row.names(data_to_plot),9,20)
  data_to_plot$variable[which(data_to_plot$variable=="Biome1")] <- "Biome periurban"
  data_to_plot$variable[which(data_to_plot$variable=="Biome2")] <- "Biome rural"
  data_to_plot$variable[which(data_to_plot$variable=="Temps")] <- "Travel time increase"
  data_to_plot$variable[which(data_to_plot$variable=="Acces")] <- "Nature use"
  data_to_plot$variable[which(data_to_plot$variable=="Paysage")] <- "Landscape"
  data_to_plot$variable[which(data_to_plot$variable=="Biodiversite")] <- "Biodiversity"
  data_to_plot$variable_class <- paste0(data_to_plot$class, sep=" ", data_to_plot$variable)
  data_to_plot$variable_class <- factor(data_to_plot$variable_class, levels=c(paste0("Class.4",sep=" ",c("Biome rural", "Biome periurban","Biodiversity","Nature use","Landscape","Travel time increase")),
                                                                              paste0("Class.3",sep=" ",c("Biome rural", "Biome periurban","Biodiversity","Nature use","Landscape","Travel time increase")),
                                                                              paste0("Class.2",sep=" ",c("Biome rural", "Biome periurban","Biodiversity","Nature use","Landscape","Travel time increase")),
                                                                              paste0("Class.1",sep=" ",c("Biome rural", "Biome periurban","Biodiversity","Nature use","Landscape","Travel time increase"))))
  data_to_plot$signif <- ifelse(sign(data_to_plot$upper_ci)==sign(data_to_plot$lower_ci),0.6,0.2)
  
  plot_res <- ggplot(data_to_plot, aes(x=estimate, y=variable_class)) +
    geom_segment(aes(x=lower_ci, xend=upper_ci, y=variable_class, yend=variable_class), color="skyblue") +
    geom_point(size=4, aes(color=class2,alpha=signif)) +
    guides(alpha="none") +
    geom_vline(xintercept = 0) +
    theme_light() +
    geom_hline(yintercept = 6.5, linetype="dashed") +
    geom_hline(yintercept = 12.5, linetype="dashed") +
    geom_hline(yintercept = 18.5, linetype="dashed") +
    xlab("Estimates") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  return(plot_res)
}
