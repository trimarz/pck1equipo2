#' Esta función descarga desde una url datos .zip o .gz, los descomprime y finalmente crea un data frame que devuelve
#'
#' @param input.url   Es la URL desde donde se obtendrán los datos
#' @param input.file  Es el nombre del fichero comprimido (.gz o .zip)
#' @param dest.file   Es el nombre del fichero en el que quedarán los datos descomprimidos
#' @param dir.data    Es el nombre de la carpeta donde se guardarán los datos descomprimidos
#' @param trace       Es el indicador de mostrar o no la traza
#' @return            Los ficheros quedan descomprimidos y en la carpeta indicada
#' @examples
#' ReadRawData("mydatas/resultados.zip", "resultados.zip", "DataSource.csv", TRUE)
#' @export
DownloadData <- function(input.url, input.file, trace=TRUE) {
  
  if (trace) print(paste("[*] Descarga desde ", input.url, ", descomprime el fichero y lo guarda como ", input.file))
  
  #if(!file.exists(input.file))
  download.file(url = input.url, destfile = input.file)
}

UnzipFile <- function(zip.file, file.in.zip, trace=TRUE) {
  if (trace)
    print(paste("[*] Descomprime el fichero ", zip.file))
  print(zip.file)
  print(file.in.zip)
  unzip(zipfile = zip.file, exdir = file.in.zip)
  rm(zipfiles)
  rm(maxmind.file, zipfiles)
}


