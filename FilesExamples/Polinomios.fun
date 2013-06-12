module Polinomios

-- En este módulo queremos obtener una implementación para la función de 
-- evaluar un polinomio. Un polinomio estará representado por una lista donde cada
-- elemento es un coeficiente.


-- Haremos uso de una función auxiliar que no está definida en el core del lenguaje: la función exponenciación:
let fun
    pow x y = case y of
                            0 -> 1
                            succ n -> x * (pow@x)@n
                         end
end

-- Y definimos una propiedad sobre la exponenciación que no la demostraremos:
let prop
    powSucc = (pow@x)@(succ i) = x * (pow@x)@i
end



-- Ahora especificamos la función evaluar:
let spec
    evaluar xs n = 〈∑i : 0≤i ∧ i<(#xs) : xs.i * (pow@n)@i〉
end


-- Y la derivamos por recursión en la lista:
let derivation evaluar by recursion on xs
case [] -> (evaluar@[])@n
                     = { spec evaluar }
                     〈∑i : 0≤i ∧ i<(#[]) : [].i * (pow@n)@i〉
                     = { Definición de Longitud }
                     〈∑i : 0≤i ∧ i<0 : [].i * (pow@n)@i〉
                     = { Intervalo Vacío }
                     〈∑i : False : [].i * (pow@n)@i〉
                     = { Rango Vacío Sumatoria }
                     0
                     
case (y|>ys) -> (evaluar@(y|>ys))@n
                                = { spec evaluar }
                                〈∑i : 0≤i ∧ i< #(y|>ys) : (y|>ys).i * (pow@n)@i〉
                                = { Definición de Longitud }
                                〈∑i : 0≤i ∧ i< succ (#ys) : (y|>ys).i * (pow@n)@i〉
                                = { Aritmética en Intervalo }
                                〈∑i : i=0 ∨(0 < i ∧ i< succ (#ys)) : (y|>ys).i * (pow@n)@i〉
                                = { Partición de Rango Sumatoria }
                                〈 ∑ i : i = 0 : (y|>ys).i * (pow@n)@i 〉 + 〈∑i : 0 < i ∧ i< succ (#ys) : (y|>ys).i * (pow@n)@i〉
                                = { Rango Unitario Sumatoria }
                                (y|>ys).0 * (pow@n)@0 + 〈∑i : 0 < i ∧ i< succ (#ys) : (y|>ys).i * (pow@n)@i〉
                                = { Definición de Indexar }
                                y * (pow@n)@0 + 〈∑i : 0 < i ∧ i< succ (#ys) : (y|>ys).i * (pow@n)@i〉
                                = { Relación entre < y ≤ }
                                 y * (pow@n)@0 +  〈 ∑ i : succ 0 ≤ i ∧ i < succ (#ys) : (y|>ys).i * (pow@n)@i〉
                                 = {  Reindizado Sumatoria }
                                 y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : (y|>ys).(succ i) * (pow@n)@(succ i)〉
                                = { Definición de Indexar }
                                 y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : (ys.i) * ((pow@n)@(succ i))〉
                                 = { fun pow }
                                 y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : (ys.i) * (n * (pow@n)@i)〉
                                 = { Asociatividad del producto }
                                  y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : ((ys.i) * n) * (pow@n)@i〉
                                  = { Simetría del producto }
                                  y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : (n * (ys.i)) * (pow@n)@i〉
                                  = { Asociatividad del producto }
                                  y * (pow@n)@0 +  〈 ∑ i : 0 ≤ i ∧ i < #ys : n * ((ys.i) * (pow@n)@i)〉
                                  = { Distributividad a izquierda del * y Sumatoria }
                                  y * (pow@n)@0 +  n * 〈 ∑ i : 0 ≤ i ∧ i < #ys : (ys.i) * (pow@n)@i〉
                                  = { spec evaluar }
                                  y * (pow@n)@0 +  n * (evaluar@ys)@n
end
                     

