#' Function to retrieve the answers of the data transformation
#'
#' This function is used to indicate a desired option and retrieve the answer
#' @param opcion Number (Integer, between 1 and 6)
#' @return Displays the selected answer on the console
#' @examples
#' retrieve_answer (2)
#' retrieve_answer (5)
#'

retrieve_answer <- function (opcion){
  opcion <- dlgInput( message = " Ingrese un número entre 1 y 6: ")$res
  opcion <- as.integer(opcion)

  if (opcion >= 1L && opcion <= 6L){
    cat("A continuación se mostrará el resultado de su selección:.\n\n")
  }
  else {
    cat("Error: el número seleccionado no aplica, por favor intentelo de nuevo.\n\n")
    retrieve_answer()
  }

  if (opcion == 1L){
    cat("Usted seleccionó: 5.2.4 Exercises, items 1, and 2, a continuación podrá observar los primero 15 datos.\n\n ")
    Ex_1 <- filter(flights_o, arr_delay >= 120)
    Ex_2 <- filter(flights_o, dest  == "IAH" | dest == "HOU" )

    cat("ITEM 1")
    print(Ex_1 [1:15,])
    cat("ITEM 2")
    print(Ex_2 [1:15,])

  }


  if (opcion == 2L){
    cat("Usted seleccionó: 5.3.1 Exercises all items, a continuación podrá observar los primero 15 datos.\n\n ")
    Ex_3 <- arrange(flights_o, desc(is.na(flights_o)))
    Ex_4 <- arrange(flights_o, desc(dep_delay))
    Ex_5 <- select(flights_o,distance, air_time )%>%
      mutate(speed= distance/air_time)%>%
      select(speed)%>%
      arrange(desc(speed))
    Ex_6 <- arrange(flights_o, desc(distance),desc(air_time))

    cat("ITEM 1")
    print(Ex_3 [1:15,])
    cat("ITEM 2")
    print(Ex_4 [1:15,])
    cat("ITEM 3")
    print(Ex_5 [1:15,])
    cat("ITEM 4")
    print(Ex_6 [1:15,])

  }

  if (opcion == 3L){
    cat("Usted seleccionó: 5.4.1 Exercises: items 2,3 and 4, a continuación podrá observar los primero 15 datos.\n\n ")
    Ex_7 <- select(flights_o, air_time, distance, hour, minute, air_time)
    vars <-  c("year", "month", "day", "dep_delay", "arr_delay")
    Ex_8 <- select(flights_o, any_of(vars))
    Ex_9 <- select(flights_o, contains("TIME"))

    cat("ITEM 2:","En este caso al incluir el nombre de una variable varias
    veces el programa selecciona dicha columna una sola vez")
    print(Ex_7 [1:15,])
    cat("ITEM 3","La función any_of() selecciona variables haciendo coincidir
    patrones en sus nombres y es util ya que los nombres de las variables coinciden
    con en el vector de caracteres, así selecciona estas columnas especificas.")
    print(Ex_8 [1:15,])
    cat("ITEM 4","Para este caso, el ayudante contains ayuda a seleccionar todas
    las columnas cuyas variables tienen en su nombre la palabra time, el valor
    predeterminado se cambia en donde va TIME ejemplo se puede usar la palabra delay.")
    print(Ex_8 [1:15,])
  }


  if (opcion == 4L){
    cat("Usted seleccionó: 5.5.2 Exercises: items 1 and 2, a continuación podrá observar los primero 15 datos.\n\n ")
    Ex_10 <- select(flights_o,dep_time, sched_dep_time)%>%
      mutate (flights_dep_time= dep_time %/% 100 * 60 + dep_time %% 100) %% 1440 %>%
      mutate (fligths_s_t= sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440
    Ex_11 <- select(flights_o, air_time, arr_time, dep_time)%>%
      mutate(arr_1= arr_time %/% 100 * 60 + arr_time %% 100) %% 1440%>%
      mutate(dep_1= dep_time %/% 100 * 60 + dep_time %% 100) %% 1440%>%
      mutate(air_time_diff = air_time - arr_1 + dep_1)

    cat("ITEM 1")
    print(Ex_10 [1:15,])
    cat("ITEM 2")
    print(Ex_11 [1:15,])
  }


  if (opcion == 5L){
    cat("Usted seleccionó: 5.6.7 Exercises: item 1, a continuación podrá observar la respuesta.\n\n ")
    cat("El restraso en la llegada es más importante que el retraso de salida ya
       que el primero puede traer consecuencias para el pasajero mientras que el
       retraso en la salida si bien se puede presentar, en algun caso no necesariamente
      implica consecuencias")
  }


  if(opcion == 6L){
    cat("Usted seleccionó: 5.7.1 Exercises: item 2, a continuación podrá observar la respuesta.\n\n ")

    Ex_12 <- filter(flights_o,!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay) )%>%
      mutate(on_time= !is.na(arr_time) & (arr_delay <= 0))%>%
      group_by(tailnum) %>%
      summarise(on_time = mean(on_time), n = n()) %>%
      filter(n >= 20) %>%
      filter(min_rank(on_time) == 1)

    cat("ITEM 2")
    print(Ex_12)
  }

  else{
    cat("seleccione el número deseado.\n\n")
    retrieve_answer()
  }

}
