shadowgram <- function(dataVec, label = NULL, layers = 50,
                       color = "black", aStep = 5){
  require(tidyverse)
  distMat <- as.matrix(dist(dataVec))
  distMat[which(distMat == 0)] <- NA

  range <- max(dataVec, na.rm = TRUE) - min(dataVec, na.rm = TRUE)
  minDist <- min(distMat, na.rm = TRUE)
  q1Dist <- quantile(distMat, na.rm = TRUE, probs = c(0.25))
  q3Dist <- quantile(distMat, na.rm = TRUE, probs = c(0.75))

  adj <- data.frame(
    adjustments = seq(
      from = min(minDist, 0.25, na.rm = TRUE),
      to = max(range, 10, na.rm = TRUE),
      length.out = layers
    )
  )

  adj$alpha <- NA

  for (i in 1:aStep) {
    a <- (i - 1) * floor(layers / aStep)
    b <- i * floor(layers / aStep)
    for (j in a:min(b, nrow(adj))) {
      adj$alpha[j] <- (aStep - i + 1) / layers
    }
  }

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
        max(dataVec, na.rm = TRUE) + 5),
      expand = expansion(mult = 0, add = 0)
    )

  for (i in 1:layers) {
    basePlot <- basePlot +
      geom_density(
        linewidth = 0,
        adjust = adj$adjustments[i],
        fill = color,
        alpha = adj$alpha[i],
        na.rm = TRUE,
        color = NA
      )
  }

  basePlot <- basePlot +
    xlab(label = label) +
    ylab("Density") +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.01), add = 0)
    )

  return(basePlot)
}
