module InducExamples


-- Funcion potenciacion
let fun
    pow x y = case y of
                            0 -> 1
                            succ n -> x * (pow@x)@n
                         end
end

-- Funcion cuadrado
let fun
    cuad x  = case x of
                            0 -> 0
                            succ i -> cuad@i + 2 * i + 1
                         end
end

-- Una propiedad que deberia estar como axioma
let prop
    absorventeProd = 0 * n = 0
end

-- Una propiedad que podriamos demostrarla pero no lo haremos
let prop
    powProp = pow@(succ i)@2 = pow@i@2 + 2*i + 1
end

-- Un teorema probado por induccion:
let thm
    propCuad = cuad@n = pow@n@2

begin proof
    induction in n for cuad@n .=. pow@n@2

    where

basic
0 -> cuad@0
        = { fun cuad }
        0
        = { prop absorventeProd }
        0 * (pow@0@1)
        = { fun pow }
        pow@0@2
        
induction (succ m) with hypind ->
        cuad@(succ m)
        = { fun cuad }
        cuad@m + 2 * m + 1
        = { hypind }
        pow@m@2 + 2 * m + 1
        = { prop powProp }
        pow@(succ m)@2
end proof
        
