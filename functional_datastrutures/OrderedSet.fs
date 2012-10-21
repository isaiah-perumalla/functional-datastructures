// Learn more about F# at http://fsharp.net

namespace OrderedSet
    module TreeSet=

        type 'a rbTree = |E  
                         | R of 'a rbTree * 'a * 'a rbTree 
                         | B of 'a rbTree * 'a * 'a rbTree

        let empty = E

        let rec mem x = function
                | E -> false
                | R(l,v,r) | B(l,v,r) ->
                    match compare x v with
                    |c when c<0 -> mem x l
                    |c when c>0 -> mem x r
                    |_ -> true 

        
        let rec add x = function
                |E -> B(E,x,E)
                |R(l,v,r) | B(l,v,r) as n ->
                    match compare x v with
                    |c when c<0 -> balance B(add x l, v, r)
                    |c when c>0 -> balance B(l, v, add x r)
                    |_ -> n

        let balance = function
            |E -> 

        let assert_balanced r = 
            let rec check_black_height root h = 
                    match root with
                    |E -> h = 0 
                    |B(l, _, r) -> check_black_height l (h-1) && check_black_height r (h-1)
                    |R(l, _, r) -> (check_black_height l h) && (check_black_height r h) 
            let rec black_height root h =
                    match root with
                    | E -> h
                    | B(l, _, _) -> black_height l (h+1)
                    | R(l, _, _) -> black_height l h
            in
            check_black_height  r (black_height r 0)
            
                    


