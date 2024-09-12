(* utils.sml *)


(* Implementación personalizada de dropWhile para listas *)
fun dropWhile _ [] = []
  | dropWhile f (x::xs) =
      if f x then dropWhile f xs else (x::xs);

(* Implementación del operador de tubería (|>) *)
infix 1 |>
fun x |> f = f x;



(* Implementación personalizada de dropWhile para listas *)
fun dropWhile _ [] = []
  | dropWhile f (x::xs) =
      if f x then dropWhile f xs else (x::xs);

(* Implementación del operador de tubería (|>) *)
infix 1 |>
fun x |> f = f x;

(* Función para eliminar espacios en blanco de una cadena *)
fun trim s =
  let
    (* Definir caracteres de espacio que queremos eliminar *)
    fun isSpace c = c = #" " orelse c = #"\t" orelse c = #"\n"
    (* Convertir la cadena a una lista de caracteres *)
    val chars = String.explode s
    (* Eliminar los espacios al principio y al final *)
    val trimmedChars = dropWhile isSpace chars |> List.rev |> dropWhile isSpace |> List.rev
  in
    (* Reconstruir la cadena desde la lista de caracteres *)
    String.implode trimmedChars
  end;
