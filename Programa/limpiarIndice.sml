
(* Función para limpiar el índice (vaciar el archivo) *)
fun limpiarIndice (archivoRuta: string) =
  let
    (* Abrir el archivo en modo de escritura, lo que borra su contenido *)
    val outputStream = TextIO.openOut archivoRuta
  in
    (* Cerrar el archivo inmediatamente después de abrirlo para que quede vacío *)
    TextIO.closeOut outputStream;
    (* Imprimir un mensaje de confirmación *)
    TextIO.print "El índice ha sido limpiado exitosamente.\n"
  end;
