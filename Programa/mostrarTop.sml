fun mostrarTop (archivoRuta: string) =
  let
    (* Intentar abrir el archivo y manejar el error si no se puede abrir *)
    val inputStream = 
      (TextIO.openIn archivoRuta) 
      handle Io => (print ("No se puede abrir el archivo: " ^ archivoRuta ^ "\n"); raise Fail "Error al abrir archivo")

    (* Función recursiva para leer todas las líneas del archivo *)
    fun leerLineas stream =
      case TextIO.inputLine stream of
          SOME linea => 
            (print ("Linea leidada: " ^ linea ^ "\n"); linea :: leerLineas stream)  (* Mensaje de depuración para cada línea leída *)
        | NONE => []

    (* Obtener todas las líneas del archivo *)
    val lineas = leerLineas inputStream

    (* Cerrar el archivo después de leer todas las líneas *)
    val _ = TextIO.closeIn inputStream

    (* Función para limpiar la entrada de espacios en blanco y saltos de línea *)
    fun limpiarEntrada entrada =
      String.implode (List.filter (fn c => c <> #" " andalso c <> #"\n" andalso c <> #"\r") (String.explode entrada))

    (* Función para solicitar un número real al usuario y manejar posibles errores *)
    fun solicitarReal mensaje =
      let
        val _ = print mensaje
        val input = TextIO.inputLine TextIO.stdIn
      in
        case input of
            SOME linea => 
              let
                val lineaLimpia = limpiarEntrada linea
              in
                (case Real.fromString lineaLimpia of
                     SOME r => r
                   | NONE => (print "Entrada no valida. Por favor, ingrese un numero real en formato valido (por ejemplo, 100.50). No use comas ni puntos para separar miles.\n"; solicitarReal mensaje))
              end
          | NONE => (print "Error al leer la entrada, por favor intente de nuevo.\n"; solicitarReal mensaje)
      end

    (* Solicitar el monto mínimo y máximo *)
    val minMonto = solicitarReal "Ingrese el monto minimo (numero real, por ejemplo, 20.0):\n"
    val maxMonto = solicitarReal "Ingrese el monto maximo (numero real, por ejemplo, 200000.0):\n"

    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLinea linea =
      let
        val campos = String.fields (fn c => c = #",") linea
      in
        case campos of
          [cuentaOrigen, fechaHora, tipoTransaccion, montoStr, _] =>
            let
              val monto = Real.fromString (limpiarEntrada montoStr)
              val _ = print ("Linea parseada: " ^ cuentaOrigen ^ ", " ^ fechaHora ^ ", " ^ tipoTransaccion ^ ", " ^ (case monto of SOME m => Real.toString m | NONE => "NONE") ^ "\n")  (* Mensaje de depuración para cada línea parseada *)
            in
              (cuentaOrigen, fechaHora, tipoTransaccion, monto)
            end
        | _ => ("", "", "", NONE)  (* Manejar líneas con campos faltantes o incorrectos *)
      end

    (* Filtrar las transacciones dentro del rango de montos *)
    val transaccionesFiltradas = List.filter
      (fn (_, _, _, optMonto) =>
        case optMonto of
            SOME monto => monto >= minMonto andalso monto <= maxMonto
          | NONE => false)  (* Manejar casos en los que el monto es NONE *)
      (List.map parsearLinea lineas)

    (* Mostrar mensaje de depuración con el número de transacciones filtradas *)
    val _ = print ("Numero de transacciones filtradas: " ^ Int.toString (List.length transaccionesFiltradas) ^ "\n")

    (* Implementación de inserción para ordenar de forma descendente por monto *)
    fun insertar (transaccion, []) = [transaccion]
      | insertar (transaccion, head :: tail) =
        let
          val (_, _, _, optMonto1) = transaccion
          val (_, _, _, optMonto2) = head
        in
          case (optMonto1, optMonto2) of
              (SOME monto1, SOME monto2) =>
                  if monto1 >= monto2 then transaccion :: head :: tail
                  else head :: insertar (transaccion, tail)
            | _ => transaccion :: head :: tail
        end

    fun ordenar [] = []
      | ordenar (transaccion :: rest) = insertar (transaccion, ordenar rest)

    (* Ordenar las transacciones utilizando el algoritmo de inserción *)
    val transaccionesOrdenadas = ordenar transaccionesFiltradas

    (* Función para imprimir las transacciones con formato *)
    fun imprimirTransaccion (cuentaOrigen, fechaHora, tipoTransaccion, SOME monto) =
      print ("Cuenta Origen: " ^ cuentaOrigen ^ "\tFecha y Hora: " ^ fechaHora ^ "\tTipo: " ^ tipoTransaccion ^ "\tMonto: " ^ Real.toString monto ^ "\n")
    | imprimirTransaccion _ = print "Transacción inválida\n"  (* Manejar casos de transacciones inválidas *)
  in
    print "Top de transacciones dentro del rango especificado:\n";
    List.app imprimirTransaccion transaccionesOrdenadas
  end;
