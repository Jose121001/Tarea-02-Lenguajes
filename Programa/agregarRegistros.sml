(* Importar funciones de utilidad desde utils.sml *)
use "utils.sml";

(* Función para agregar un registro *)
fun agregarRegistro archivoRuta =
  let
    (* Solicitar datos al usuario *)
    val _ = TextIO.print "Ingrese el numero de cuenta origen: "
    val cuentaOrigen = valOf (TextIO.inputLine TextIO.stdIn)

    val _ = TextIO.print "Ingrese la fecha y hora (YYYY-MM-DD HH:MM:SS): "
    val fechaHora = valOf (TextIO.inputLine TextIO.stdIn)

    val _ = TextIO.print "Ingrese el tipo de transaccion (deposito, retiro, transferencia): "
    val tipoTransaccion = valOf (TextIO.inputLine TextIO.stdIn)

    val _ = TextIO.print "Ingrese el monto: "
    val monto = valOf (TextIO.inputLine TextIO.stdIn)

    (* Solicita número de cuenta destino solo si es una transferencia *)
    val cuentaDestino =
      if trim tipoTransaccion = "transferencia" then
        (TextIO.print "Ingrese el número de cuenta destino: "; valOf (TextIO.inputLine TextIO.stdIn))
      else
        ""

    (* Crea el registro como una línea de texto *)
    val registro = String.concat [trim cuentaOrigen, ",", trim fechaHora, ",", trim tipoTransaccion, ",", trim monto, if cuentaDestino = "" then "" else "," ^ trim cuentaDestino, "\n"]

    (* Abre el archivo en modo de escritura para agregar el registro *)
    val outputStream = TextIO.openAppend archivoRuta
  in
    (* Escribe el registro en el archivo *)
    TextIO.output (outputStream, registro);
    TextIO.closeOut outputStream;
    TextIO.print "Registro agregado exitosamente.\n"
  end;
