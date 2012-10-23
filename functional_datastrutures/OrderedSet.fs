// R.Sedgewick's Left leaning red-black tree implemention

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

        
        let balance t = match t with
                           |B( R( R(lll, llv, llr) , lv, lr), v, r) -> R(B(lll, llv, llr) , lv, B(lr, v, r)) 
                           |B(R(ll, lv,lr), v, R(rl, rv, rr)) ->     R(B(ll, lv, lr), v, B(rl, rv, rr))
                           |B(R(ll,lv, R(llr, lrv, lrr) ), v, r) -> R(B(ll, lv, llr), lrv, B(lrr,v,r))
                           |B(l, v, R(rl, rv, rr)) -> B(R(l,v,rl), rv, rr)  
                           |_ -> t


        let inline node n (l,v,r) =
            match n with 
                |B(_,_,_) -> B(l,v,r)
                |_ -> R(l,v,r)

        let rec add x = function
                |E -> R(E,x,E)
                |R(l,v,r) 
                |B(l,v,r) as n ->
                    match compare x v with
                    |c when c<0 ->  balance (node n (add x l,v,r))
                    |c when c>0 ->  balance (node n (l, v, add x r))
                    |_ -> n

        
        let insert set x = 
            let s = add x set
            in match s with 
                |R(l, v, r) -> B(l, v, r)
                |_ -> s
        
        let rec assert_left_leaning_invariants  = function
                    |R(_,_,R(_,_,_)) -> false
                    |B(_,_,R(_,_,_)) -> false
                    |R(R(_,_,_), _, _) -> false
                    |R(l,v,r) | B(l,v,r) -> assert_left_leaning_invariants l && assert_left_leaning_invariants r
                    |_ -> true
            
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
            
                    


