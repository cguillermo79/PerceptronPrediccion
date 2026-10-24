#' Predecir con un perceptron
#' @param modelo Modelo entrenado.
#' @param X Matriz o data.frame de predictores.
#' @return Vector de predicciones binarias.
#' @export
predecir_perceptron <- function(modelo, X) {
  campos <- c('pesos', 'sesgo', 'medias', 'desviaciones')
  if (!is.list(modelo) || !all(campos %in% names(modelo)))
    stop('Modelo invalido.')
  X <- as.matrix(X)
  if (!is.numeric(X) || any(!is.finite(X)))
    stop('X debe ser numerica y finita.')
  if (ncol(X) != length(modelo$pesos))
    stop('Dimensiones incompatibles.')
  Xs <- sweep(X, 2, modelo$medias, '-')
  Xs <- sweep(Xs, 2, modelo$desviaciones, '/')
  as.integer(drop(Xs %*% modelo$pesos + modelo$sesgo) >= 0)
}
