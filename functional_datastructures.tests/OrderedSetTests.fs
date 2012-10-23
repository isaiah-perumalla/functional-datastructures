namespace OrderedSet.Tests

open NUnit.Framework
open FsUnit
open OrderedSet
open System

[<TestFixture>]
type ``Given a set should test for member ship`` ()=
       
       let randomSeq n = 
            let r = new Random()
            in seq{for i in 1 .. n -> r.Next()} 

       let assert_invariants t = (TreeSet.assert_balanced t && TreeSet.assert_left_leaning_invariants t) 
                                    |> should be True
      
       [<Test>] member test.
         ``simple test for equality.`` ()=
           TreeSet.mem 1 TreeSet.empty  |> should be False
           
       [<Test>] 
       member test.
        ``test membership on non empty set`` ()= 
           TreeSet.mem 2 (TreeSet.add 2 TreeSet.empty)  |> should be True

       [<Test>] 
       member test.
        ``test balance on increasing seq insertion `` ()= 
            let set_with_1000 = Seq.fold TreeSet.insert TreeSet.empty {1 .. 1000}
            TreeSet.mem 100 set_with_1000 |> should be True
            assert_invariants set_with_1000
            
       [<Test>] 
       member test.
        ``test balance on decreasing seq insertion `` ()= 
            let set = Seq.fold TreeSet.insert TreeSet.empty (seq{1000 .. -1 ..1})
            TreeSet.mem 100 set |> should be True
            assert_invariants set
                    
       [<Test>] 
       member test.
        ``test balance on random seq insertion `` ()= 
            let set_with_random = Seq.fold TreeSet.insert TreeSet.empty (randomSeq 10000)    
            assert_invariants set_with_random


       [<Test>] 
       member test.
        ``test performanance of inserts `` ()=
          // time (Seq.fold TreeSet.insert TreeSet.empty) (randomSeq 1000000) |> ignore
           let ins set y=  Set.add y set
           time (Seq.fold ins Set.empty) (randomSeq 10000000) |> ignore
      
       [<Test>] 
       member test.
        ``test performanance of lookups `` ()=
           let set = (Seq.fold TreeSet.insert TreeSet.empty) (randomSeq 1000000) 
           let ins set y=  Set.add y set
           time (Seq.fold ins Set.empty) (randomSeq 10000000) |> ignore

            

