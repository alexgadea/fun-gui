module GeneralizationExample

import SumVerificated

-- Queremos encontrar la implementación de la siguiente función, 
-- la cual devuelve True si todo segmento inicial de una lista de naturales tiene suma positiva.
-- (Con nuestros tipos de datos no tiene mucho sentido ya que siempre será True (no tenemos negativos)

let spec
    segPos xs = 〈∀ i : 0 ≤ i ∧ i < #xs : ¬ (sum@(xs↑i) < 0)〉
end

-- Al querer derivar esto nos encontraríamos con un problema, ya que
-- no podemos aplicar la especificación tal como está, necesitamos generalizarla:

let spec
    genSegPos n xs =  〈∀ i : 0 ≤ i ∧ i < #xs : ¬ (n + sum@(xs↑i) < 0)〉
 end
 
 
let derivation genSegPos by recursion on xs

case [] ->
                    genSegPos@n@[]
            ≡  {spec genSegPos}
                    〈∀ i : 0 ≤ i ∧ i < #[] : ¬ (n + sum@([]↑i) < 0)〉
            ≡ { Definición de Longitud }
                    〈∀ i : 0 ≤ i ∧ i < 0 : ¬ (n + sum@([]↑i) < 0)〉
            ≡ { Intervalo Vacío }
                    〈∀ i : False : ¬ (n + sum@([]↑i) < 0)〉
            ≡ { Rango Vacío Para Todo }
                    True
                    
case (y ▹ ys) ->
                     genSegPos@n@(y ▹ ys)
            ≡  {spec genSegPos}
                    〈∀ i : 0 ≤ i ∧ i < #(y ▹ ys) : ¬ (n + sum@((y ▹ ys)↑i) < 0)〉
            ≡ { Definición de Longitud }
                    〈∀ i : 0 ≤ i ∧ i < succ (#ys) : ¬ (n + sum@((y ▹ ys)↑i) < 0)〉
            ≡ { Separación del primer término Para Todo }
                    ¬ (n + sum@((y ▹ ys)↑0) < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + sum@((y ▹ ys)↑(succ i)) < 0)〉
            ≡ { Definición de Take }
                     ¬ (n + sum@[] < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + sum@((y ▹ ys)↑(succ i)) < 0)〉
            ≡ { Definición de Take }
                     ¬ (n + sum@[] < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + sum@(y ▹ (ys↑i)) < 0)〉
            ≡ { fun sum }
                    ¬ (n + sum@[] < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + (y + sum@(ys↑i)) < 0)〉
            ≡ { fun sum }
                    ¬ (n + 0 < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + (y + sum@(ys↑i)) < 0)〉
            ≡ { Neutro a derecha de la suma }
                    ¬ (n  < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ (n + (y + sum@(ys↑i)) < 0)〉
            ≡ { Asociatividad de la suma }
                    ¬ (n  < 0) ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ¬ ((n + y) + sum@(ys↑i) < 0)〉
            ≡  {spec genSegPos}
                    ¬ (n  < 0) ∧ genSegPos@(n+y)@ys
end

-- Esta es la definición de la función generalizada:
let fun
    genSegPos n xs =  
        case xs of
            [] -> True
            (y ▹ ys) -> ¬ (n  < 0) ∧ genSegPos@(n+y)@ys
          end
 end

-- Y entonces podemos definir la función original:
let fun 
    segPos xs = genSegPos@0@xs
end

