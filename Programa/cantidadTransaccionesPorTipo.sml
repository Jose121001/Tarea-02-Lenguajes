(* cantidadTransaccionesPorTipo.sml *)

fun contarTransaccionesPorTipo (archivoRuta: string) =
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

    (* Obtiene todas las líneas del archivo *)
    val lineasTransacciones = leerLineasTransacciones inputStream

    (* Cerrar el archivo después de leer todas las líneas *)
    val _ = TextIO.closeIn inputStream

    (* Función para limpiar la entrada de espacios en blanco y saltos de línea *)
    fun limpiarEntradaTransaccion entrada =
      String.implode (List.filter (fn c => c <> #" " andalso c <> #"\n" andalso c <> #"\r") (String.explode entrada))

    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLineaTransaccion linea =
      let
        val campos = String.fields (fn c => c = #",") linea
      in
        case campos of
          [_, _, tipoTransaccion, _, _] => limpiarEntradaTransaccion tipoTransaccion
        | _ => ""  (* Manejar líneas con campos faltantes o incorrectos *)
      end

    (* Solicita al usuario el tipo de transacción *)
    fun solicitarTipoTransaccion () =
      let
        val _ = print "Ingrese el tipo de transacción a contar (deposito, retiro, transferencia):\n"
        val tipo = TextIO.inputLine TextIO.stdIn
      in
        case tipo of
            SOME t => limpiarEntradaTransaccion t
          | NONE => (print "Error al leer el tipo de transaccion, por favor intente de nuevo.\n"; solicitarTipoTransaccion ())
      end

    (* Cuenta las transacciones por el tipo proporcionado *)
    fun contarTransacciones tipo transacciones =
      List.foldl (fn (transaccion, contador) => if transaccion = tipo then contador + 1 else contador) 0 transacciones

    (* Parsear todas las líneas y contar las transacciones del tipo proporcionado *)
    val transaccionesLimpias = List.map parsearLineaTransaccion lineasTransacciones
    val tipoBuscado = solicitarTipoTransaccion ()
    val cantidad = contarTransacciones tipoBuscado transaccionesLimpias

  in
    print ("Cantidad de transacciones del tipo '" ^ tipoBuscado ^ "': " ^ Int.toString cantidad ^ "\n")
  end;
