module MutualRecursion

let fun
    par x = 
            if x=0 then True
                          else impar@(pred x)
            fi
end

let fun
    impar x =
            if x = 0 then False
                            else par@(pred x)
            fi
end
