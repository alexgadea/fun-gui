module SumVerificated

let spec 
    sum xs = 〈 ∑ i : 0 ≤ i ∧ i < #xs : xs.i 〉 
end

let fun sum xs = case xs of
                            [] -> 0
                            (y|>ys) -> y + sum@ys
                        end
verified from sumVerification

let prop truchada = sum@[] = 0 end

{- El siguiente teorema es la verificación de la función sum -}
let thm sumVerification = sum@xs = case xs of
                                                                                        [] -> 0
                                                                                        (y|>ys) -> y + sum@ys
                                                                                    end
begin proof  [~ especificacion : sum@xs = 〈 ∑ i : 0 ≤ i ∧ i < #xs : xs.i 〉  ~]
            induction in xs for sum@xs .=.  case xs of
                                                                                     [] -> 0
                                                                                     (y|>ys) -> y + sum@ys
                                                                                 end
            where
basic 
[] ->     sum@[]
              = { especificacion}
              〈 ∑ i : 0 ≤ i ∧ i < #[] : [].i 〉
              = { Definición de Longitud }
              〈 ∑ i : 0 ≤ i ∧ i < 0 : [].i 〉
              = { Intervalo Vacío }
              〈 ∑ i : False : [].i 〉
              = { Rango Vacío Sumatoria }
              0

induction y ▹ ys with hypind -> sum@(y ▹ ys)
                                                                    = { especificacion }
                                                                    〈 ∑ i : 0 ≤ i ∧ i < #(y ▹ ys) : (y ▹ ys).i〉
                                                                    = { Definición de Longitud }
                                                                    〈 ∑ i : 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys).i〉
                                                                    = { Aritmética en Intervalo }
                                                                    〈 ∑ i : i = 0 ∨ (0 < i ∧ i < succ (#ys)) : (y ▹ ys).i〉
                                                                    = { Partición de Rango Sumatoria }
                                                                    〈 ∑ i : i = 0 : (y ▹ ys).i 〉 + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i 〉
                                                                    = { Rango Unitario Sumatoria }
                                                                    ((y ▹ ys).0) + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i〉
                                                                    = { Definición de Indexar }
                                                                    y + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i〉
                                                                    = { Relación entre < y ≤ }
                                                                    y + 〈 ∑ i : succ 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys).i〉
                                                                    = { Reindizado Sumatoria }
                                                                    y + 〈 ∑ i : 0 ≤ i ∧ i < #ys : (y ▹ ys).(succ i)〉
                                                                    = { Definición de Indexar }
                                                                    y + 〈 ∑ i : 0 ≤ i ∧ i < #ys : ys.i〉
                                                                    = { especificacion }
                                                                    y + (sum@ys)
end proof
