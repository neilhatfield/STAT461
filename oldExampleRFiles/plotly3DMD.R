packages <- c("tidyr","plotly","rstatix")
lapply(packages, library, character.only=TRUE)


set.seed(7)
cv1 <- rnorm(100, mean = 3, sd = 15)
set.seed(14)
cv2 <- rnorm(100, mean = 2, sd = 20)

temp1 <- data.frame(
  covariate1 = cv1,
  covariate2 = cv2,
  factor = sort(rep(LETTERS[1:5], 20)),
  response = NA
)

ttt <- function(x,a,b,c) {
  if(a == "A") {
    t1 = 9
  } else if (a == "B") {
      t1 = -10
  } else if (a == "C"){
      t1 = 7
  } else if (a == "D"){
      t1 = 0
  } else {
      t1 = -6
  }
  return( pi*(b/5)^2 + -7*c + t1 + rnorm(1,0,4))
}

set.seed(28)
temp1$response <- mapply(ttt, a = temp1$factor, b=temp1$covariate1, c=temp1$covariate2)

boxplot(temp1$covariate1 ~ temp1$factor)
boxplot(temp1$covariate2 ~ temp1$factor)
boxplot(temp1$response ~ temp1$factor)

plotly::plot_ly(data = temp1,
                x = ~covariate1,
                y = ~covariate2,
                z = ~response,
                color = ~factor) %>%
  plotly::add_markers() %>%
  plotly::layout(scene = list(xaxis = list(title = "Covariate 1"),
                              yaxis = list(title = "Covariate 2"),
                              zaxis = list(title = "Response")))


temp2 <- rstatix::mahalanobis_distance(temp1)
temp2 <- cbind(temp2, factor = temp1$factor)
plotly::plot_ly(data = temp2,
                x = ~covariate1,
                y = ~covariate2,
                z = ~response,
                color = ~factor,
                symbol = ~is.outlier,
                symbols = c("circle", "cross"))%>%
  plotly::add_markers() %>%
  plotly::layout(scene = list(xaxis = list(title = "Covariate 1"),
                              yaxis = list(title = "Covariate 2"),
                              zaxis = list(title = "Response")))

