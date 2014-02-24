module And

let spec 
    and.xs = 〈∀ i : 0 ≤ i ∧ i < #xs : xs!i 〉
end

let derivation and by recursion on xs
    case [] -> and.[]
               ≡ { spec and }
                         〈∀ i : 0 ≤ i ∧ i < #[]: []!i 〉 
               ≡ { Definición de Longitud }
                         〈∀ i : 0 ≤ i ∧ i < 0: []!i 〉 
               ≡ { Intervalo Vacío }
                         〈∀ i : False : []!i〉 
               ≡ { Rango Vacío Para Todo }
                         True
                         
    case  y ▹ ys -> and.(y ▹ ys)
                    ≡ { spec and }
                                〈∀ i : 0 ≤ i ∧ i < #(y ▹ ys) : (y ▹ ys)!i〉
                   ≡ { Definición de Longitud }
                                〈∀ i : 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys)!i〉
                    ≡ { Aritmética en Intervalo }
                                〈∀ i : i = 0 ∨ (0 < i ∧ i < succ (#ys)) : (y ▹ ys)!i〉
                    ≡ { Partición de Rango Para Todo }
                                〈∀ i : i = 0 : (y ▹ ys)!i〉 ∧ 〈∀ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys)!i〉
                    ≡ { Rango Unitario Para Todo }
                                (y ▹ ys)!0 ∧ 〈∀ i : 0 < i ∧ i < succ(#ys) : (y ▹ ys)!i〉
                    ≡ { Definición de Indexar }
                                y ∧ 〈∀ i : 0 < i ∧ i < succ(#ys) : (y ▹ ys)!i〉
                    ≡ { Relación entre < y ≤ }
                                y ∧ 〈∀ i : succ 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys)!i〉
                    ≡ { Reindizado Para Todo }
                                y ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : (y ▹ ys)!(succ i)〉
                    ≡ { Definición de Indexar }
                                y ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ys!i〉
                    ≡ { spec and }
                                y ∧ and.ys
end
