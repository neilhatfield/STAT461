shadowgram <- function(dataVec, label = NULL, layers = 50){
  distMat <- as.matrix(dist(dataVec))
  distMat[which(distMat == 0)] <- NA

  range <- max(dataVec, na.rm = TRUE) - min(dataVec, na.rm = TRUE)
  minDist <- min(distMat, na.rm = TRUE)
  q1Dist <- quantile(distMat, na.rm = TRUE, probs = c(0.25))
  q3Dist <- quantile(distMat, na.rm = TRUE, probs = c(0.75))

  adjustments <- seq(
    from = q1Dist,
    to = q3Dist,
    length.out = layers
  )

  basePlot <- ggplot(
    data = data.frame(
      x = dataVec
    ),
    mapping = aes(x = x)
  ) +
    theme_bw() +
    scale_x_continuous(
      limits = c(
        min(dataVec, na.rm = TRUE) - 5,
        max(dataVec, na.rm = TRUE) + 5)
    )

  for (i in 1:layers) {
    basePlot <- basePlot +
      geom_density(
        size = 0,
        adjust = adjustments[i],
        fill = "black",
        alpha = 1/layers,
        na.rm = TRUE
      )
  }

  basePlot <- basePlot + xlab(label = label)

  return(basePlot)
}
