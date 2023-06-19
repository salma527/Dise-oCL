#' Diseño Cuadrado Latino
#'
#' Obtiene la Tabla de Analisis de Varianza (ANOVA) para un Diseño en Cuadrado Latino (DCL).
#'
#' @param respuesta (string) nombre de la variable respuesta.
#' @param tratamiento (string) nombre de la variable que representan los tratamientos.
#' @param fila (string) nombre de la variable que representan las filas.
#' @param columna (string) nombre de la variable que representan las columnas.
#' @param data {(\code{data.frame})Tabla de datos en formato largo con los datos de los tratamientos, filas, columnas y de la variable respuesta.
#' @return Devuelve una tabla en formato \code{data.frame} con los cálculos correspondientes al análisis de varianza.
#' @export
#'
#' @examples
#' \dontrun{
#' # Ejemplo 1
#' # 1 Limpiar la memoria de R
#' rm(list = ls ())
#' # 2 Cargamos el paquete DCL
#' library(DCL)
#' # 3 Ruta del archivo y nombre del mismo
#' # ruta <- ''...C:/ruta/data.csv''
#' # 4 Lectura de archivo de datos
#' MisDatos <- read.csv("data.csv")
#'
#' # 5 Ejecutamos la funcion TablaAnova
#' #TablaAnova(respuesta = "respuesta", tratamiento = "tratamiento", fila = "fila", columna = "columna", data = data)
#' }
#' 
#' # Ejemplo 2
#' # 1 Con datos acomodados en un Dataframe
#' MisDatos <- data.frame(
#' rendimiento = c(13.7, 13.2, 12.3, 12, 12, 11.6, 11.2, 11.1, 10.5, 10.3, 10.2, 7.7, 7.5, 5.9, 5.8,12.2),
#' edades = c(4, 4, 2, 2, 3, 1, 3, 1, 1, 3, 4, 2, 4, 3, 1, 2),
#' condicion = c(3, 1, 4, 2, 1, 4, 3, 2, 1, 2, 4, 1, 2, 4, 3, 3), 
#' tratamiento = c("b", "a" ,"b" ,"a" ,"b" ,"a" ,"a" ,"b" ,"c" ,"c" ,"c" ,"d" ,"d" ,"d" , "d", "c"))
#' # 2 Cargamos el paquete DCL
#' library(DCL)
#' # 3 Ejecutamos la funcion
#' TablaAnova(respuesta = "rendimiento", tratamiento = "tratamiento", fila = "edades", columna = "condicion", data = MisDatos)
#'tabla anova
TablaAnova <- function(respuesta, tratamiento, fila, columna, data){
  # Defino la variable respuesta y los tratamientos y bloques como factores
  y <- MisDatos[,respuesta]
  trat <- factor(MisDatos[, tratamiento])
  fila <- factor(MisDatos[, fila])
  columna <- factor(MisDatos[,columna])
  a <- nlevels(trat)
  b <- nlevels(fila)
  c <- nlevels(columna)
  # Correccion para la media
  suma_total <- sum(y)
  C <- suma_total^2 /(a*b)
  # SC Total
  sc_total <- sum(y^2) - C
  gl_total <- a^2-1
  # SC tratamientos
  sumasxtrat <- tapply(y, INDEX = trat, FUN = sum)
  n_trat <- tapply(y, INDEX = trat, FUN = length)
  sc_trat <- sum(sumasxtrat^2 / n_trat) - C
  gl_trat <- a-1
  cm_trat <- sc_trat / gl_trat
  # SC columna
  sumasxfila <- tapply(y, INDEX = fila, FUN = sum)
  n_fila <- tapply(y, INDEX = fila, FUN = length)
  sc_fila <- sum(sumasxfila^2 / n_fila) - C
  gl_fila <- a-1
  cm_fila <- sc_fila / gl_fila
  # SC fila
  sumasxcolumna <- tapply(y, INDEX = columna, FUN = sum)
  n_columna <- tapply(y, INDEX = columna, FUN = length)
  sc_columna <- sum(sumasxcolumna^2 / n_columna) - C
  gl_columna<- a-1
  cm_columna <- sc_columna / gl_columna
  # error experimental
  sc_error <- sc_total - (sc_columna + sc_fila + sc_trat)
  gl_error <- (a-1)*(b-2)
  cm_error <- sc_error / gl_error
  # Valores F
  F_trat <- cm_trat / cm_error
  F_fila <- cm_columna / cm_error
  F_columna <- cm_fila / cm_error
  # P-values
  p_value_trat <- pf(F_trat, gl_trat, gl_error, lower.tail = FALSE)
  p_value_fila <- pf(F_fila, gl_fila, gl_error, lower.tail = FALSE)
  p_value_columna <- pf(F_columna, gl_columna, gl_error, lower.tail = FALSE)
  # Creamos el dataframe
  df <- data.frame(FV = c("Tratamientos", "Columnas", "Filas", "Error experimental", "Total"),
                   GL = c(gl_trat, gl_columna, gl_fila, gl_error, gl_total),
                   SC = c(sc_trat, sc_columna, sc_fila, sc_error, sc_total),
                   CM = c(cm_trat, cm_columna, cm_fila, cm_error, NA),
                   vF = c(F_trat, F_fila, F_columna, NA, NA),
                   `Pr(>vF)` = c(p_value_trat, p_value_fila, p_value_columna, NA, NA),
                   check.names = FALSE)


  rownames(df) <- NULL
  anava <- format(df)
  anava[is.na(df)] <- ""

  return(anava)
}

