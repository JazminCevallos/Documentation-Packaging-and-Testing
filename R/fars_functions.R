# Cargar un archivo CSV
# 
# '@descripcion
# 'Esta funcion es necesaria para subir un archivo CSV definido por el argumento \ code {filename} y regresa
# 'a tibble, se debe cuidar la ruta para que no recaiga en un error.
# '@param filename
# 'Ruta al archivo CSV (Es un caracter)
# '@return
# ' Obtenemos un marco de lelctura (data.frame) respecto al csv.
# '@examples
# '\ dontrun {accident<- fars_read ("./ data / accident_2015.csv.bz2")}
# '@importFrom readr read_csv
# '@importFrom dplyr tbl_df

fars_read  <-  function ( filename ) {
  if ( ! archivo.existe ( nombre de archivo ))
    stop ( " archivo ' " , nombre de archivo , " ' no existe " )
  data  <- suppressMessages ({
    readr :: read_csv ( filename , progress  =  FALSE )
  })
  dplyr :: tbl_df ( datos )
}

# Crea un nombre de archivo
# '
# '@descripcion
# 'crea un nombre de archivo .csv.bz2 basado en el \ code {year}
# 'argumento en una forma "accident_ <year> .csv.bz2". Debe ser un numero entero.
# '@ param year Numerical of integer input muestra el ano.
# '@return 
# 'Da el nombre del archivo con carateres "accident_ <year> .csv.bz2" 
# '@examples
# '\ dontrun {
# 'makefilename (2015)}

make_filename  <-  function ( year ) {
  ano  <- as.integer ( ano )
  sprintf ( " accident_% d.csv.bz2 " , aÃ±o )
}

# La funcion lee el mes y el ano.
# '
# '@description
# 'La funcion toma la informacion de los anos y devuelve una lista de datos
# 'fotogramas con columnas MES y ano basadas en datos del csv.
# '@param years Un vector o lista de ans en formato numerico o entero.
# '@return 
# 'Da a manera de data.frme una lista con el mismo nÃºmero de filas, donde tenemos
# 'como los datos en los archivos "accident_ <year> .csv.bz2" y dos columnas - MONTH y
# 'ano. Si recibe un NULL el archivo no existe
# '@examples
# '\ dontrun {
# 'fars_read_years (2014: 2016)
# 'fars_read_years (list (2014, 2017,2015))}
# '@importFrom dplyr%>% mutate select
# '
fars_read_years  <-  function ( years ) {
  lapply ( aÃ±os , funciÃ³n ( aÃ±o ) {
    archivo  <- make_filename ( aÃ±o )
    trata de atraparlo({
      dat  <- fars_read ( archivo )
      dplyr :: mutate_ ( dat , aÃ±o  =  ~ aÃ±o )% > %
        dplyr :: select_ ( " MES " , " aÃ±o " )
    }, error  =  function ( e ) {
      advertencia ( " aÃ±o invÃ¡lido: " , aÃ±o )
      return ( NULL )
    })
  })
}

# Conteo del numero de accidentes por mes y ano
# 'La funcion calcula el nÃºmero de accidentes mensuales en Estados Unidos,
# 'los anos se pueden pasar como una lista o un vector.
# '@param years  Un vector o lista de aÃ±os que se lo busca de entre los dato
# '@return 
# 'Da un data.frame con dos entradas meses en filas vrs anos en columnas, esta
# 'muestra el numero de accidentes 
# '@examples
# '\ dontrun {
# 'fars_summarize_years (c(2010, 2013, 2015, 2017))}
# '@importFrom dplyr%>% bind_rows group_by summarize
# '@importFrom tidyr spread
# '@export

fars_summarize_years  <-  function ( aÃ±os ) {
  dat_list  <- fars_read_years ( aÃ±os )
  dplyr :: bind_rows ( dat_list )% > %
    dplyr :: group_by_ ( " year " , " MONTH " )% > %
    dplyr :: summarize_ ( n  =  ~ n ())% > %
    tidyr :: spread_ ( " aÃ±o " , " n " )
}

# Grafica los accidentes en un mapa segun sea el estado
# '
# 'Este codigo nos permite  observar los accidentes a traves de un mapa.
# '@param state.num 
# 'El numero de un estado debe ser numerico o entero y debe estar tal cual como
# 'se indica en FARS.
# '@ param year  ano de analisis 
# '@return Regresa un diagrama de los accidentes segun en el \ code {state.num} y
# '\ code {year}, cuidar el aoo debe estar en el conjunto de datos para evitar errores
# '@examples
# '\ dontrun {
# 'fars_map_state (45, 2015)}
# '@importFrom dplyr filter
# '@importFrom map map
# '@importFrom graphics points
# '@export

fars_map_state  <-  function ( state.num , year ) {
  nombre de archivo  <- make_filename ( aÃ±o )
  data  <- fars_read ( nombre del archivo )
  state.num  <- as.integer ( state.num )
  
  if ( ! ( state.num  % en% unique ( data $ STATE )))
    stop ( " nÃºmero de estado invÃ¡lido: " , estado.num )
  data.sub  <-  dplyr :: filter_ ( data , ~ STATE  ==  state.num )
  if (nrow ( data.sub ) ==  0L ) {
    mensaje ( " no hay accidentes para trazar " )
    return ( invisible ( NULL ))
  }
  is.na ( data.sub $ LONGITUD ) <-  data.sub $ LONGITUD  >  900
  is.na ( data.sub $ LATITUDE ) <-  data.sub $ LATITUDE  >  90
  con ( data.sub , {
    maps :: map ( " estado " , ylim  = rango ( LATITUDE , na.rm  =  TRUE ),
                  xlim  = rango ( LONGITUD , na.rm  =  VERDADERO ))
    graphics :: points ( LONGITUD , LATITUDE , pch  =  46 )
  })
}