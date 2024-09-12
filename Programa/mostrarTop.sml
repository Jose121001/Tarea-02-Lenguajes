(* Función para filtrar y mostrar las transacciones dentro de un rango de montos *)
(*Funcion obtenida de chat e implementada disatinta*)
fun mostrarTop (lineas: string list) (minMonto: real) (maxMonto: real) =
  let
    (* Función auxiliar para convertir una línea de texto a un registro de transacción *)
    fun parsearLinea linea =
      case String.fields (fn c => c = #",") linea of
          [cuentaOrigen, fechaHora, tipoTransaccion, montoStr] => 
              let
                val monto = Option.map valOf (Real.fromString montoStr)  (* Convertir a real usando Real.fromString sacado de chat *)
              in
                (cuentaOrigen, fechaHora, tipoTransaccion, monto)
              end
        | _ => ("", "", "", NONE)  (* En caso de error de formato, devolver una tupla vacía *)

    (* Filtra las transacciones dentro del rango de montos/Implementacion por chatGPT *)
    val transaccionesFiltradas = List.filter
      (fn (_, _, _, SOME monto) => monto >= minMonto andalso monto <= maxMonto)
      (List.map parsearLinea lineas)

    (* Ordena las transacciones de forma descendente por monto *)
    val transaccionesOrdenadas = ListMergeSort.sort (fn (_, _, _, SOME monto1) => fn (_, _, _, SOME monto2) => Real.compare (monto2, monto1)) transaccionesFiltradas

    (* Función para imprimir las transacciones con formato *)
    fun imprimirTransaccion (cuentaOrigen, fechaHora, tipoTransaccion, SOME monto) =
      print ("Cuenta Origen: " ^ cuentaOrigen ^ "\tFecha y Hora: " ^ fechaHora ^ "\tTipo: " ^ tipoTransaccion ^ "\tMonto: " ^ Real.toString monto ^ "\n")
  in
    print "Top de transacciones dentro del rango especificado:\n";
    List.app imprimirTransaccion transaccionesOrdenadas  (* Imprimir cada transacción ordenada *)
  end;
