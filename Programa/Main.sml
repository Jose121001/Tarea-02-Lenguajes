(*Importacion de archivos*)
use "agregarRegistros.sml";
use "limpiarIndice.sml";
use "utils.sml";


fun main () =
  let
    (* Función para limpiar caracteres no deseados como espacios en blanco y saltos de línea *)
    fun limpiarRuta ruta =
      let
        (* Eliminar los espacios en blanco al principio y al final *)
        val rutaLimpia = trim ruta
        (* Convertir la cadena a una lista de caracteres *)
        val listaCaracteres = String.explode rutaLimpia
        (* Filtrar solo los caracteres que no sean saltos de línea o retorno de carro *)
        val listaFiltrada = List.filter (fn c => c <> #"\n" andalso c <> #"\r") listaCaracteres
        (* Reconstruir la cadena limpia *)
        val rutaFinal = String.implode listaFiltrada
      in
        rutaFinal
      end

    (* Solicitar la ruta del archivo al iniciar y limpiar la entrada *)
    fun solicitarRutaArchivo () =
      (
        print "Ingrese la ruta del archivo de transacciones (por ejemplo, C:\\Users\\joses\\Documents\\datatransacciones.csv):\n";
        case TextIO.inputLine TextIO.stdIn of
            SOME ruta => limpiarRuta ruta  (* Limpiar la entrada de caracteres no deseados *)
          | NONE => (print "Error al leer la ruta, por favor intente de nuevo.\n"; solicitarRutaArchivo ())
      )

    (* Función para imprimir el menú principal *)
    fun printMainMenu () =
      (
        print ("Bienvenido al sistema bancario, seleccione la opción a trabajar:\n");
        print ("1- Agregar Registros\n");
        print ("2- Limpiar indice\n");
        print ("3- Menu de Analizador\n");
        print ("4- Salir\n");
        print ("Opcion: ")
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
    (*Se llaman las funciones segun su opc*)
    fun handleMainMenu archivoRuta =
      let
        val option = TextIO.inputLine TextIO.stdIn
      in
        case option of
            SOME "1\n" => (agregarRegistro archivoRuta; handleMainMenu archivoRuta)
          | SOME "2\n" => (limpiarIndice archivoRuta; handleMainMenu archivoRuta)
          | SOME "3\n" => (print "Ir al menu de Analizador\n"; handleAnalyzerMenu archivoRuta)
          | SOME "4\n" => print "Saliendo...\n"
          | _ => (print "Opción invqlida, intente de nuevo.\n"; handleMainMenu archivoRuta)
      end
    (*Si no se utiliza el and daria error de llamamiento ya que se estan llamado al mismo tiempo*)
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
