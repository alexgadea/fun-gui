module And

let spec 
    and xs = 〈∀ i : 0 ≤ i ∧ i < #xs : xs.i 〉
end

let derivation and by recursion on xs
    case [] -> and@[]
               ≡ { spec and }
                         〈∀ i : 0 ≤ i ∧ i < #[]: [].i 〉 
               ≡ { Longitud de la lista vacía }
                         〈∀ i : 0 ≤ i ∧ i < 0: [].i 〉 
               ≡ { Harry Potter Equivalencia }
                         〈∀ i : False : [].i〉 
               ≡ { Rango Vacío Para Todo }
                         True
                         
    case  y ▹ ys -> and@(y ▹ ys)
                    ≡ { spec and }
                                〈∀ i : 0 ≤ i ∧ i < #(y ▹ ys) : (y ▹ ys).i〉
                    ≡ { Longitud de lista no vacía }
                                〈∀ i : 0 ≤ i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    ≡ { Harry Potter Equivalencia }
                                〈∀ i : i = 0 ∨ (0 < i ∧ i < succ (#ys)) : (y ▹ ys).i〉
                    ≡ { Partición de Rango Para Todo }
                                〈∀ i : i = 0 : (y ▹ ys).i〉 ∧ 〈∀ i : 0 < i ∧ i < succ (#ys) : (y ▹ ys).i〉
                    ≡ { Rango Unitario Para Todo }
                                (y ▹ ys).0 ∧ 〈∀ i : 0 < i ∧ i < succ(#ys) : (y ▹ ys).i〉
                    ≡ { Proyectar el elemento inicial }
                                y ∧ 〈∀ i : 0 < i ∧ i < succ(#ys) : (y ▹ ys).i〉
                    ≡ { Harry Potter Equivalencia }
                                y ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : (y ▹ ys).(succ i)〉
                    ≡ { Proyectar el elemento (i+1) }
                                y ∧ 〈∀ i : 0 ≤ i ∧ i < #ys : ys.i〉
                    ≡ { spec and }
                                y ∧ and@ys
end
