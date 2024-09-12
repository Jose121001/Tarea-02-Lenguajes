(* resumen.sml *)

fun generarResumen (archivoRuta: string) =
  let
    (* Intentar abrir el archivo y manejar el error si no se puede abrir *)
    val inputStream = 
      (TextIO.openIn archivoRuta) 
      handle Io => (print ("No se puede abrir el archivo: " ^ archivoRuta ^ "\n"); raise Fail "Error al abrir archivo")

    (* Función recursiva para leer todas las líneas del archivo *)
    fun leerLineasResumen stream =
      case TextIO.inputLine stream of
          SOME linea => linea :: leerLineasResumen stream
        | NONE => []

    (* Obtiene todas las líneas del archivo *)
    val lineasResumen = leerLineasResumen inputStream

    (* Cerrar el archivo después de leer todas las líneas *)
    val _ = TextIO.closeIn inputStream

    (* Función para limpiar la entrada de espacios en blanco y saltos de línea *)
    fun limpiarEntradaResumen entrada =
      String.implode (List.filter (fn c => c <> #" " andalso c <> #"\n" andalso c <> #"\r") (String.explode entrada))

    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLineaResumen linea =
      let
        val campos = String.fields (fn c => c = #",") linea
      in
        case campos of
          [cuentaOrigen, fechaHora, tipoTransaccion, montoStr, cuentaDestino] =>
            let
              val cuentaOrigenLimpia = limpiarEntradaResumen cuentaOrigen
              val cuentaDestinoLimpia = limpiarEntradaResumen cuentaDestino
              val tipo = limpiarEntradaResumen tipoTransaccion
              val monto = case Real.fromString (limpiarEntradaResumen montoStr) of
                            SOME m => m
                          | NONE => 0.0
            in
              (cuentaOrigenLimpia, fechaHora, tipo, monto, cuentaDestinoLimpia)
            end
        | _ => ("", "", "", 0.0, "")
      end

    (* Parsear todas las líneas del archivo *)
    val transacciones = List.map parsearLineaResumen lineasResumen

    (* Función para contar las transacciones por tipo *)
    fun contarTransaccionesPorTipo transacciones =
      let
        fun contar (tipo, contador) [] = contador
          | contar (tipo, contador) ((_, _, t, _, _) :: rest) =
              if t = tipo then contar (tipo, contador + 1) rest
              else contar (tipo, contador) rest
      in
        [("deposito", contar ("deposito", 0) transacciones),
         ("retiro", contar ("retiro", 0) transacciones),
         ("trasferencia", contar ("trasferencia", 0) transacciones)]
      end

    (* Función para encontrar la transacción con el monto mayor y menor *)
    fun encontrarMontosExtremos transacciones =
      let
        (* Inicializar con valores extremos realistas *)
        val transaccionMayorInicial = ("", "", "", ~1.0, "")
        val transaccionMenorInicial = ("", "", "", Real.posInf, "")

        (* Función para encontrar el monto máximo *)
        fun maxMonto (t1 as (_, _, _, m1, _), t2 as (_, _, _, m2, _)) =
              if Real.compare (m1, m2) = GREATER then t1 else t2

        (* Función para encontrar el monto mínimo *)
        fun minMonto (t1 as (_, _, _, m1, _), t2 as (_, _, _, m2, _)) =
              if Real.compare (m1, m2) = LESS then t1 else t2

        (* Encuentra las transacciones con el monto máximo y mínimo *)
        val transaccionMayor = List.foldl maxMonto transaccionMayorInicial transacciones
        val transaccionMenor = List.foldl minMonto transaccionMenorInicial transacciones
      in
        (transaccionMayor, transaccionMenor)
      end

    (* Función para encontrar la cuenta con más transacciones origen *)
    fun cuentaConMasTransaccionesOrigen transacciones =
      let
        fun actualizarContadorOrigen cuenta [] = [(cuenta, 1)]
          | actualizarContadorOrigen cuenta ((cu, count) :: rest) =
              if cu = cuenta then (cu, count + 1) :: rest
              else (cu, count) :: actualizarContadorOrigen cuenta rest

        fun contarOrigen [] contador = contador
          | contarOrigen ((cuenta, _, _, _, _) :: rest) contador =
              contarOrigen rest (actualizarContadorOrigen cuenta contador)

        val contadorOrigen = contarOrigen transacciones []
        val maxOrigen = List.foldl (fn ((cu, count), (maxCu, maxCount)) => if count > maxCount then (cu, count) else (maxCu, maxCount)) ("", 0) contadorOrigen
      in
        maxOrigen
      end

    (* Función para encontrar la cuenta con el mayor monto recibido *)
    fun cuentaConMayorMontoRecibido transacciones =
      let
        fun actualizarMontoDestino cuenta monto [] = [(cuenta, monto)]
          | actualizarMontoDestino cuenta monto ((cu, total) :: rest) =
              if cu = cuenta then (cu, total + monto) :: rest
              else (cu, total) :: actualizarMontoDestino cuenta monto rest

        fun sumarMontosDestino [] contador = contador
          | sumarMontosDestino ((_, _, _, monto, destino) :: rest) contador =
              sumarMontosDestino rest (actualizarMontoDestino destino monto contador)

        val contadorDestino = sumarMontosDestino transacciones []
        val maxDestino = List.foldl (fn ((cu, total), (maxCu, maxTotal)) => if total > maxTotal then (cu, total) else (maxCu, maxTotal)) ("", 0.0) contadorDestino
      in
        maxDestino
      end

    (* Generar el resumen con todos los cálculos *)
    val transaccionesPorTipo = contarTransaccionesPorTipo transacciones
    val (transaccionMayor, transaccionMenor) = encontrarMontosExtremos transacciones
    val (cuentaMasTransacciones, _) = cuentaConMasTransaccionesOrigen transacciones
    val (cuentaMayorMonto, _) = cuentaConMayorMontoRecibido transacciones

  in
    (* Imprimir el resumen *)
    print "Resumen de Transacciones:\n";
    List.app (fn (tipo, cantidad) => print ("Tipo: " ^ tipo ^ " - Cantidad: " ^ Int.toString cantidad ^ "\n")) transaccionesPorTipo;
    print ("Transaccion con el monto mayor: " ^ Real.toString (#4 transaccionMayor) ^ "\n");
    print ("Transaccion con el monto menor: " ^ Real.toString (#4 transaccionMenor) ^ "\n");
    print ("Cuenta con mas transacciones origen: " ^ cuentaMasTransacciones ^ "\n");
    print ("Cuenta con mayor monto recibido: " ^ cuentaMayorMonto ^ "\n")
  end;
