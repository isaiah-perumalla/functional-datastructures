namespace OrderedSet.Tests

open NUnit.Framework
open FsUnit
open OrderedSet
open System

[<TestFixture>]
type ``Given a set should test for member ship`` ()=
       
       let randomSeq n = 
            let r = new Random()
            in seq{for i in 1 .. n -> r.Next(5000)} 
      
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
            TreeSet.assert_balanced set_with_1000 |> should be True
            TreeSet.assert_left_leaning_invariants set_with_1000 |> should be True
            
       [<Test>] 
       member test.
        ``test balance on decreasing seq insertion `` ()= 
            let set_with_random = Seq.fold TreeSet.insert TreeSet.empty (seq{1000 .. -1 ..1})
            TreeSet.mem 100 set_with_random |> should be True
            TreeSet.assert_balanced set_with_random |> should be True
            TreeSet.assert_left_leaning_invariants set_with_random |> should be True
                    
       [<Test>] 
       member test.
        ``test balance on random seq insertion `` ()= 
            let set_with_random = Seq.fold TreeSet.insert TreeSet.empty (randomSeq 1000)
           
            TreeSet.assert_balanced set_with_random |> should be True
            TreeSet.assert_left_leaning_invariants set_with_random |> should be True


            

