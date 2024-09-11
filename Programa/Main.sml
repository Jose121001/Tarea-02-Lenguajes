fun main () =
  let
    (* Solicitar la ruta del archivo al iniciar *)
    fun solicitarRutaArchivo () =
      (
        print "Ingrese la ruta del archivo de transacciones (por ejemplo, /tmp/transacciones.csv):\n";
        case TextIO.inputLine TextIO.stdIn of
            SOME ruta => ruta
          | NONE => (print "Error al leer la ruta, por favor intente de nuevo.\n"; solicitarRutaArchivo ())
      )

    (* Función para imprimir el menú principal *)
    fun printMainMenu () =
      (
        print ("Bienvenido al sistema bancario, seleccione la opción a trabajar:\n");
        print ("1- Agregar Registros\n");
        print ("2- Limpiar índice\n");
        print ("3- Menu de Analizador\n");
        print ("4- Salir\n");
        print ("Opción: ")
      )

    (* Función para imprimir el menú del Analizador *)
    fun printAnalyzerMenu () =
      (
        print ("Menu de Analizador:\n");
        print ("a. Mostrar top por monto\n");
        print ("b. Informe de actividades sospechosas\n");
        print ("c. Transacciones por cuenta\n");
        print ("d. Cantidad de transacciones por tipo\n");
        print ("e. Resumen\n");
        print ("f. Volver al menu principal\n");
        print ("Opción: ")
      )

    (* Declarar funciones mutuamente recursivas con "and" *)
    fun handleMainMenu archivoRuta =
      let
        val option = TextIO.inputLine TextIO.stdIn
      in
        case option of
            SOME "1\n" => (print "Agregar Registros\n"; handleMainMenu archivoRuta)
          | SOME "2\n" => (print "Limpiar indice\n"; handleMainMenu archivoRuta)
          | SOME "3\n" => (print "Ir al menu de Analizador\n"; handleAnalyzerMenu archivoRuta)
          | SOME "4\n" => print "Saliendo...\n"
          | _ => (print "Opción invqlida, intente de nuevo.\n"; handleMainMenu archivoRuta)
      end

    and handleAnalyzerMenu archivoRuta =
      let
        val option = TextIO.inputLine TextIO.stdIn
      in
        case option of
            SOME "a\n" => (print "Mostrar top por monto\n"; handleAnalyzerMenu archivoRuta)
          | SOME "b\n" => (print "Informe de actividades sospechosas\n"; handleAnalyzerMenu archivoRuta)
          | SOME "c\n" => (print "Transacciones por cuenta\n"; handleAnalyzerMenu archivoRuta)
          | SOME "d\n" => (print "Cantidad de transacciones por tipo\n"; handleAnalyzerMenu archivoRuta)
          | SOME "e\n" => (print "Resumen\n"; handleAnalyzerMenu archivoRuta)
          | SOME "f\n" => handleMainMenu archivoRuta
          | _ => (print "Opción inválida, intente de nuevo.\n"; handleAnalyzerMenu archivoRuta)
      end
  in
    (* Iniciar el programa solicitando la ruta del archivo *)
    let
      val archivoRuta = solicitarRutaArchivo ()
    in
      (* Mostrar el menú principal una vez obtenida la ruta del archivo *)
      printMainMenu ();
      handleMainMenu archivoRuta
    end
  end;

(* Ejecutar la función main al iniciar el programa *)
main ();
