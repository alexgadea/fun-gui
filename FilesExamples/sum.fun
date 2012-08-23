module Sum

let spec 
    sum xs = 〈 ∑ i : 0 ≤ i ∧ i < #xs : xs.i 〉 
end


let derivation sum by recursion on xs
    case [] -> sum@[]
              = { spec sum }
              〈 ∑ i : 0 ≤ i ∧ i < #[] : [].i 〉
              = { Longitud de la lista vacía }
              〈 ∑ i : 0 ≤ i ∧ i < 0 : [].i 〉
              = { Intervalo Vacío }
             〈 ∑ i : False : [].i 〉
              = { Rango Vacío Sumatoria }
              0
                          
    case (y|>ys) -> sum@(y ▹ ys)
                    = { spec sum }
                    〈 ∑ i : 0 ≤ i ∧ i < #(y ▹ ys) : (y ▹ ys).i〉
                    = { Longitud de lista no vacía }
                    〈 ∑ i : 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    = { Aritmética en Intervalo }
                    〈 ∑ i : i = 0 ∨ (0 < i ∧ i < succ (#ys)) : (y ▹ ys).i〉
                    = { Partición de Rango Sumatoria }
                    〈 ∑ i : i = 0 : (y ▹ ys).i 〉 + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i 〉
                    = { Rango Unitario Sumatoria }
                    ((y ▹ ys).0) + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    = { Proyectar el elemento inicial }
                    y + 〈 ∑ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    = { Relación entre < y ≤ }
                    y + 〈 ∑ i : succ 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    = { Reindizado Sumatoria }
                    y + 〈 ∑ i : 0 ≤ i ∧ i < #ys : (y ▹ ys).(succ i)〉
                    = { Proyectar el elemento (i+1) }
                    y + 〈 ∑ i : 0 ≤ i ∧ i < #ys : ys.i〉
                    = { spec sum }
                    y + (sum@ys)
end

