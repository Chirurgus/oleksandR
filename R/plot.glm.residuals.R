# Created by Oleksandr Sorochynskyi
# On 01/06/19

#' Plot glm residuals
#'
#' Plot glm residuals together with a kernel smoothing.
#' 
#' 
#' 
#' @param model Model object of which to find the objects
#' @param model.name The name to use in the plot
#' @param residual.type The type of residuals to extract, and plot
#' @param response.type The scale at which to show the predictions
#' @param reest Weather or not should be 'predict_reest' be used
#' @keywords ecdf, cdf
#' @export
#' @references
plot.residuals <- function(model, model.name, residual.type = "pearson", response.type="link", reest=FALSE) {
  # Extraire des residus de notre modèle
  residus <- residuals(model, type=residual.type)
  if (reest) {
    prediction <- predict_reest(model, type=response.type)
  } else {
    prediction <- predict(model, type=response.type)
  }

  # Lissage locale des residus
  plot(prediction, residus,
       main= paste("Modèle", model.name),
       ylab= "Residus de Pearson",
       xlab= "Prediction (linéaire)")
  r.smooth <- ksmooth(prediction, residus)
  abline(h=0, col="blue")
  lines(r.smooth, col="red")
  legend("topright",
         legend= c("y = 0", "Lissage local des residus"),
         lty= 1,
         col= c("blue", "red")
  );
}
