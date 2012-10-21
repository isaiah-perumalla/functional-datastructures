namespace OrderedSet.Tests

open NUnit.Framework
open FsUnit
open OrderedSet

[<TestFixture>]
type ``Given a set should test for member ship`` ()=
       
       let add_to_set set v = TreeSet.add v set
       [<Test>] member test.
         ``simple test for equality.`` ()=
           TreeSet.mem 1 TreeSet.empty  |> should be False
           
       [<Test>] 
       member test.
        ``test membership on non empty set`` ()= 
           TreeSet.mem 2 (TreeSet.add 2 TreeSet.empty)  |> should be True

       [<Test>] 
       member test.
        ``test balance `` ()= 
            let set_with_1000 = Seq.fold add_to_set TreeSet.empty {1 .. 1000}
            TreeSet.mem 100 set_with_1000 |> should be True
            TreeSet.assert_balanced set_with_1000 |> should be True
                


            

