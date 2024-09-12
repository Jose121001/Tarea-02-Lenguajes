fun transaccionesPorCuenta (archivoRuta: string) =
  let
    (* Intentar abrir el archivo y manejar el error si no se puede abrir *)
    val inputStream = 
      (TextIO.openIn archivoRuta) 
      handle Io => (print ("No se puede abrir el archivo: " ^ archivoRuta ^ "\n"); raise Fail "Error al abrir archivo")

    (* Función recursiva para leer todas las líneas del archivo *)
    fun leerLineasTransacciones stream =
      case TextIO.inputLine stream of
          SOME linea => linea :: leerLineasTransacciones stream
        | NONE => []

    (* Obtener todas las líneas del archivo *)
    val lineas = leerLineasTransacciones inputStream

    (* Cerrar el archivo después de leer todas las líneas *)
    val _ = TextIO.closeIn inputStream

    (* Función para limpiar la entrada de espacios en blanco y saltos de línea *)
    fun limpiarEntradaTransacciones entrada =
      String.implode (List.filter (fn c => c <> #" " andalso c <> #"\n" andalso c <> #"\r") (String.explode entrada))

    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLineaTransacciones linea =
      let
        val campos = String.fields (fn c => c = #",") linea
      in
        case campos of
          [cuentaOrigen, fechaHora, tipoTransaccion, montoStr, cuentaDestino] =>
            let
              val cuentaOrigenLimpia = limpiarEntradaTransacciones cuentaOrigen
              val cuentaDestinoLimpia = limpiarEntradaTransacciones cuentaDestino
              val fecha = limpiarEntradaTransacciones (String.extract (fechaHora, 0, SOME 10))  (* Extraer solo la fecha *)
              val tipo = limpiarEntradaTransacciones tipoTransaccion
              val monto = case Real.fromString (limpiarEntradaTransacciones montoStr) of
                            SOME m => SOME m
                          | NONE => NONE
            in
              (cuentaOrigenLimpia, fecha, tipo, monto, cuentaDestinoLimpia)
            end
        | _ => ("", "", "", NONE, "")  (* Manejar líneas con campos faltantes o incorrectos *)
      end

    (* Solicita al usuario el número de cuenta *)
    fun solicitarCuentaTransacciones () =
      let
        val _ = print "Ingrese el número de cuenta a buscar:\n"
        val cuenta = TextIO.inputLine TextIO.stdIn
      in
        case cuenta of
            SOME c => limpiarEntradaTransacciones c
          | NONE => (print "Error al leer la cuenta, por favor intente de nuevo.\n"; solicitarCuentaTransacciones ())
      end

    (* Filtra las transacciones por la cuenta proporcionada *)
    fun filtrarTransaccionesPorCuenta cuenta transacciones =
      List.filter (fn (cuentaOrigen, _, _, _, cuentaDestino) =>
                      cuentaOrigen = cuenta orelse cuentaDestino = cuenta)
                  transacciones

    (* Función para imprimir las transacciones de la cuenta con formato *)
    fun imprimirTransaccionPorCuenta (cuentaOrigen, fecha, tipo, SOME monto, cuentaDestino) =
      print ("Cuenta Origen: " ^ cuentaOrigen ^ "\tFecha: " ^ fecha ^ "\tTipo: " ^ tipo ^ "\tMonto: " ^ Real.toString monto ^ "\tCuenta Destino: " ^ cuentaDestino ^ "\n")
    | imprimirTransaccionPorCuenta _ = print "Transacción inválida\n"  (* Manejar casos de transacciones inválidas *)

    (* Parsear todas las líneas y filtrar las transacciones de la cuenta proporcionada *)
    val transacciones = List.map parsearLineaTransacciones lineas
    val cuentaBuscada = solicitarCuentaTransacciones ()
    val transaccionesFiltradas = filtrarTransaccionesPorCuenta cuentaBuscada transacciones

  in
    if null transaccionesFiltradas then
      print ("No se encontraron transacciones para la cuenta: " ^ cuentaBuscada ^ "\n")
    else (
      print ("Transacciones para la cuenta: " ^ cuentaBuscada ^ "\n");
      List.app imprimirTransaccionPorCuenta transaccionesFiltradas
    )
  end;
