module P


let fun
    isZero x = case x of
                               0 -> True
                               succ n -> False
                          end
end

{- Ejemplo de uso de la definicion de una funcion con case en teoremas -}

let thm teo = 
   isZero.0 ≡ True
begin proof
    isZero.0
  ≡ { fun isZero }
   True
end proof
    
let thm teodos = isZero.(succ y) ≡ False
begin proof
    isZero.(succ y)
  ≡ { fun isZero }
   False
end proof

let prop falsa = True ≡ False
end

let prop otra = True ≡ y
end

let val x = False
end

let fun y = 0
end

let thm trucho = ¬ True ≡ True
begin proof
    ¬ True
    ≡ { Definición de False }
    False
    ≡ { val x }
    x
    ≡ { prop otra }
    True
end proof



