(* informeActividadesSospechosas.sml *)

fun informeActividadesSospechosas (archivoRuta: string) =
  let
    (* Intentar abrir el archivo y manejar el error si no se puede abrir *)
    val inputStream = 
      (TextIO.openIn archivoRuta) 
      handle Io => (print ("No se puede abrir el archivo: " ^ archivoRuta ^ "\n"); raise Fail "Error al abrir archivo")

    (* Función recursiva para leer todas las líneas del archivo *)
    fun leerLineasSospechosas stream =
      case TextIO.inputLine stream of
          SOME linea => linea :: leerLineasSospechosas stream
        | NONE => []

    (* Obtiene todas las líneas del archivo *)
    val lineasSospechosas = leerLineasSospechosas inputStream

    (* Cerrar el archivo después de leer todas las líneas *)
    val _ = TextIO.closeIn inputStream

    (* Función para limpiar la entrada de espacios en blanco y saltos de línea *)
    fun limpiarEntradaSospechosas entrada =
      String.implode (List.filter (fn c => c <> #" " andalso c <> #"\n" andalso c <> #"\r") (String.explode entrada))

    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLineaSospechosa linea =
      let
        val campos = String.fields (fn c => c = #",") linea
      in
        case campos of
          [cuentaOrigen, fechaHora, tipoTransaccion, montoStr, _] =>
            let
              val fecha = String.extract (fechaHora, 0, SOME 10)  (* Extraer solo la fecha de fechaHora *)
              val monto = Real.fromString (limpiarEntradaSospechosas montoStr)
            in
              (cuentaOrigen, fecha, tipoTransaccion, monto)
            end
        | _ => ("", "", "", NONE)  (* Manejar líneas con campos faltantes o incorrectos *)
      end

    (* Contar retiros y transferencias por cuenta y fecha *)
    fun contarTransaccionesPorCuentaFecha transacciones =
      let
        (* Función para actualizar el contador de transacciones por cuenta y fecha *)
        fun actualizarContador (cuenta, fecha, tipo, contador) =
          if tipo = "retiro" orelse tipo = "trasferencia" then
            let
              (* Actualizar la lista con la cuenta y fecha si ya existen, o añadir un nuevo registro *)
              fun actualizar [] = [(cuenta, fecha, 1)]
                | actualizar ((cu, fe, count) :: rest) =
                    if cu = cuenta andalso fe = fecha then (cu, fe, count + 1) :: rest
                    else (cu, fe, count) :: actualizar rest
            in
              actualizar contador
            end
          else contador

        (* Procesa cada transacción y actualiza el contador *)
        fun procesarTransacciones [] contador = contador
          | procesarTransacciones ((cuenta, fecha, tipo, _) :: rest) contador =
              let
                val nuevoContador = actualizarContador (cuenta, fecha, tipo, contador)
              in
                procesarTransacciones rest nuevoContador
              end
      in
        procesarTransacciones transacciones []
      end

    (* Obtiene las transacciones sospechosas *)
    fun obtenerSospechosas contador =
      List.filter (fn (_, _, count) => count >= 5) contador

    (* Parsear todas las líneas y contar las transacciones *)
    val transacciones = List.map parsearLineaSospechosa lineasSospechosas
    val contador = contarTransaccionesPorCuentaFecha transacciones
    val sospechosas = obtenerSospechosas contador

    (* Función para imprimir las transacciones sospechosas con formato *)
    fun imprimirSospechosas (cuenta, fecha, count) =
      print ("Cuenta Origen: " ^ cuenta ^ "\tFecha: " ^ fecha ^ "\tNúmero de transacciones sospechosas: " ^ Int.toString count ^ "\n")

  in
    print "Informe de actividades sospechosas:\n";
    List.app imprimirSospechosas sospechosas
  end;
