# modify plotmag function to remove axes
plotmag.tts <- function(object, zlims = NULL, neat = TRUE,
                        colorfill = NULL, colorbar = TRUE,
                        title = NULL, filename = NA,
                        axes = FALSE, ...) {
  
  wav <- Mod(get_values(object))
  times <- get_times(object)
  timescales <- get_timescales(object)
  
  if (is.null(zlims)) {
    zlims <- range(wav, na.rm = TRUE)
  } else {
    rg <- range(wav, na.rm = TRUE)
    if (rg[1] < zlims[1] || rg[2] > zlims[2]) {
      stop("Error in plotmag.tts: zlims must encompass the z axis range of what is being plotted")
    }
  }
  
  if (neat) {
    inds <- which(!is.na(colMeans(wav, na.rm = TRUE)))
    wav <- wav[, inds]
    timescales <- timescales[inds]
  }
  
  if (is.null(colorfill)) {
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill <- grDevices::colorRampPalette(jetcolors)
  }
  
  ylocs <- pretty(timescales, n = 8)
  xlocs <- pretty(times, n = 8)
  
  if (!is.na(filename)) {
    grDevices::pdf(paste0(filename, ".pdf"))
  }
  
  # plot image without axes
  if (!colorbar) {
    graphics::image(x = times, y = log2(timescales), z = wav, xlab = "Time",
                    zlim = zlims, ylab = "Timescale", axes = FALSE,
                    col = colorfill(100), main = title, ...)
  } else {
    fields::image.plot(x = times, y = log2(timescales), z = wav, xlab = "Time",
                       zlim = zlims, ylab = "Timescale", axes = FALSE,
                       col = colorfill(100), main = title, ...)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
}