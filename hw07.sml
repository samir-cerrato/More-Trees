use "hw07-lib.sml";
exception Unimplemented

(* ---------------------------------------------------------------------- *)
(* map *)

fun pluralize_rec (t : string tree) : string tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (x ^ "s")
      | Node(l,r) => Node(pluralize_rec l , pluralize_rec r)

fun mult_rec (c : int, t : int tree) : int tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (c * x)
      | Node(l,r) => Node(mult_rec (c,l) , mult_rec (c,r))

(* TASK *)
(*Purpose: computes a new tree by applyimng some function f  to each element
of the given tree.*)
fun map (f : 'a -> 'b, t : 'a tree) : 'b tree = 
    case t of  
        Empty => Empty
        | Leaf x => Leaf (f(x))
        | Node (l,r) => Node (map(f,l), map(f,r))

(* TASK *)
(*Purpose: same purpose as pluralize_rec, except it is defined using map rather
than being defined directly by recursion
Examples:
["a","b","c"] = ["as", "bs", "cs"]
["d","e","f"] = ["ds", "es", "fs"]
["g","h","i"] = ["gs", "hs", "is"]
["j","k","l"] = ["js", "ks", "ls"]*)
fun pluralize (t : string tree) : string tree =
    map(fn x => x ^ "s" , t)

(*Purpose: same purpose as mult_rec, except it is defined using map rather
than being defined directly by recursion
Examples:
(4, [1,2,3) = [4,8,12]
(2, [2,3,4]) = [4,6,8]
(3, [3,4,5]) = [9,12,15]
(5, [5,6,7]) = [25,30,35]*)
fun mult (c : int, t : int tree) : int tree = 
    map(fn x => x * c , t)

fun test_pluralize() =
    (testsl "p1" (tolist (pluralize (fromlist ["a","b","c"]))) ["as","bs","cs"];
    testsl "p2" (tolist (pluralize (fromlist ["d","e","f"]))) ["ds","es","fs"];
    testsl "p3" (tolist (pluralize (fromlist ["g","h","i"]))) ["gs","hs","is"];
    testsl "p4" (tolist (pluralize (fromlist ["j","k","l"]))) ["js","ks","ls"])

fun test_mult() =
    (testil "m1" (tolist (mult (4,fromlist [1,2,3]))) [4,8,12];
     testil "m2" (tolist (mult (2,fromlist [2,3,4]))) [4,6,8];
     testil "m3" (tolist (mult (3,fromlist [3,4,5]))) [9,12,15];
     testil "m4" (tolist (mult (5,fromlist [5,6,7]))) [25,30,35])


(* ---------------------------------------------------------------------- *)
(* reduce *)


fun sum_rec (t : int tree) : int =
    case t of
        Empty => 0
      | Leaf x => x
      | Node(t1,t2) => (sum_rec t1) + (sum_rec t2)

fun join_rec (t : string tree) : string =
    case t of
        Empty => ""
      | Leaf x => x
      | Node(t1,t2) => (join_rec t1) ^ (join_rec t2)

(* TASK *)
(*Purpose: implements the operation of reduction on trees *)
fun reduce (n : 'a * 'a -> 'a, b : 'a, t : 'a tree) : 'a = 
    case t of 
        Empty => b
        | Node (l,r) => n (reduce (n,b,l), reduce (n,b,r))
        | Leaf x => x

(* TASK *)
(*Purpose: equivalent to sum_rec but defined using reduce rather than being defined
directly by recursion.
Example:
[1,2,3,4] = 10
[2,3,4,5] = 14
[3,4,5,6] = 18
[4,5,6,7] = 22*)        
fun sum (t : int tree) : int = 
    reduce(fn (a,b) => a+b, 0, t)

(*Purpose: equivalent to join_rec but defined using reduce rather than being defined
directly by recursion.
Examples:
["a","b","c"] = "abc"
["d","e","f"] = "def"
["g,","h","i"] = "ghi"
["j","k","l"] = "jkl"*)
fun join (t : string tree) : string = 
    reduce(fn (a,b) => a ^ b , "", t)

fun test_sum() =
    (testi "s1" (sum (fromlist [1,2,3,4])) 10;
    testi "s2" (sum (fromlist [2,3,4,5])) 14;
    testi "s3" (sum (fromlist [3,4,5,6])) 18;
    testi "s4" (sum (fromlist [4,5,6,7])) 22)

fun test_join() =
    (tests "j1" (join (fromlist ["a","b","c"])) "abc";
     tests "j2" (join (fromlist ["d","e","f"])) "def";
     tests "j3" (join (fromlist ["g","h","i"])) "ghi";
     tests "j4" (join (fromlist ["j","k","l"])) "jkl")
    

(* ---------------------------------------------------------------------- *)
(* programming with map and reduce *)

(* TASK *)
(*Purpose: fuction such that flatten t contains all of the elements of all of the trees in t. The elements of
each tree t1 in t should occur in flatten t in the same order in which they occur in t1; if
a tree t1 is to the left of a tree t2 in t, the elements of t1 should occur to the left of the
elements of t2 in flatten t.
Examples:
([1,2,3], [4,5,6]) = [1,2,3,4,5,6]
([4,5,6], [7,8,9]) = [4,5,6,7,8,9]
([2,3,4], [5,6,7]) = [2,3,4,5,6,7]
([7,8,9], [10,11,12]) = [7,8,9,10,11,12]*)
fun flatten (t : ('a tree) tree) : 'a tree = 
    reduce(fn (a,b) => Node (a,b), Empty, t)

fun test_flatten() =
    (testil "f1" (tolist (flatten (fromlist [ fromlist [1,2,3], fromlist [4,5,6]]))) [1,2,3,4,5,6];
    testil "f2" (tolist (flatten (fromlist [ fromlist [4,5,6], fromlist [7,8,9]]))) [4,5,6,7,8,9];
    testil "f3" (tolist (flatten (fromlist [ fromlist [2,3,4], fromlist [5,6,7]]))) [2,3,4,5,6,7];
    testil "f4" (tolist (flatten (fromlist [ fromlist [7,8,9], fromlist [10,11,12]]))) [7,8,9,10,11,12])

(* TASK *)
(*Purpose: a function such that filter (p, t) contains all and only the elements x : ’a of t for which p x
returns true. The elements that are kept should be in the same order as in the original tree.
Example:
x mod 2 [1,2,3,4,5,6] = [2,4,6]
x = 5 [1,3,5,7,9] = [5]
x > 3 [2,3,4,5,6] = [4,5,6]
x < 7 [4,5,6,7,8,9] = [4,5,6]*)
fun filter (p : 'a -> bool, t : 'a tree) : 'a tree = 
    let val tt = map (fn x =>(case p(x) of
                                  true => Leaf x
                                    | false => Empty), t)
                                  in reduce (fn (x,y) => Node(x,y), Empty, tt) end
                                  

fun test_filter() =
    (testil "fi1" (tolist (filter (fn x => (x mod 2) = 0, fromlist [1,2,3,4,5,6]))) [2,4,6];
    testil "fi2" (tolist (filter (fn x => x = 5, fromlist [1,3,5,7,9]))) [5];
    testil "fi3" (tolist (filter (fn x => x > 3, fromlist [2,3,4,5,6]))) [4,5,6];
    testil "fi4" (tolist (filter (fn x => x < 7, fromlist [4,5,6,7,8,9]))) [4,5,6])

fun assist (x: 'a, tree2 : 'b tree) : ('a * 'b) tree =
    map (fn y => (x,y), tree2)    
(* TASK *)
(*Purpose: a function such that allpairs(t1, t2) contains the pair (x,y) for every element x of t1 and y of t2.
The order of the pairs is unspecified.
Example:
[1,2,3] ["a","b"] = [(1,"a"),(1,"b"),(2,"a"),(2,"b"),(3,"a"),(3,"b")]
[1,2,3] ["c","d"] = [(1,"c"),(1,"d"),(2,"c"),(2,"d"),(3,"c"),(3,"d")]
[4,5,6] ["a","b"] = [(4,"a"),(4,"b"),(5,"a"),(5,"b"),(6,"a"),(6,"b")]
[4,5,6] ["c","d"] = [(4,"c"),(4,"d"),(5,"c"),(5,"d"),(6,"c"),(6,"d")]*)
fun allpairs (tree1 : 'a tree, tree2 : 'b tree) : ('a * 'b) tree = 
      flatten (map (fn x => assist (x, tree2), tree1))

fun test_allpairs() =
    (* note: if your output comes out in a different order, that's OK; just change the expected result *)
    (testisl "ap1" (tolist (allpairs (fromlist [1,2,3], fromlist ["a","b"])))
                   [(1,"a"),(1,"b"),(2,"a"),(2,"b"),(3,"a"),(3,"b")];
    testisl "ap2" (tolist (allpairs (fromlist [1,2,3], fromlist ["c","d"])))
                   [(1,"c"),(1,"d"),(2,"c"),(2,"d"),(3,"c"),(3,"d")];
    testisl "ap3" (tolist (allpairs (fromlist [4,5,6], fromlist ["a","b"])))
                   [(4,"a"),(4,"b"),(5,"a"),(5,"b"),(6,"a"),(6,"b")];
    testisl "ap2" (tolist (allpairs (fromlist [4,5,6], fromlist ["c","d"])))
                   [(4,"c"),(4,"d"),(5,"c"),(5,"d"),(6,"c"),(6,"d")])

fun run() =
    (test_pluralize();
     test_mult();
     test_sum();
     test_join();
     test_flatten();
     test_filter();
     test_allpairs())
    
(* ---------------------------------------------------------------------- *)
(* partnr *)

type answers = int * int * int * int

fun same(x : int, y : int) : real = 
    case x = y of
        true => 1.0
      | false => 0.0

fun count_same ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
    same (a1,a1') + same (a2,a2') + same (a3,a3') + same (a4,a4')


(*Purpose: a helper function that scores a value for the first question
*)
fun value1 (x: int, y: int): real =
    case x = 5 of
         true => (case y = 5 of
                    true => 1.0
                      |false => 0.60)
       |false => case y = 5 of
                      true => 0.60
                     |false => same(x,y)

(*Purpose: a helper function that scores a value for the second question
*)
fun value2(x: int, y: int): real=
    case x = y of
         true => 1.5
        |false => ~0.5

(*Purpose: a helper function that scores a value for the third question
*)
fun value3(x: int, y: int): real =
    case y = 3 of
         true =>(case x = 3 of
                      true => 1.5
                     |false => ~0.5)
        |false => case x = 3 of
                       true => ~0.5
                      |false => same(x,y)
(* TASK *)
(*Purpose: a function , which implements some
alternative compatibility scoring of my choice. Other scoring functions might
give different weights to different questions, or allow “fuzzy matching” of answers, however my scoring
will account for the total value of the question being asked. *)
fun my_scoring ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
    value1(a1, a1') + value2(a2, a2') + value3(a3, a3') + same(a4, a4')

(* TASK *)

(*Purpose: elimnates duplicates that already occured and a user's score*)
fun duplicates(t : ((string * answers) * (string * answers)) tree) : ((string * answers) * (string * answers)) tree =
    filter(fn ((x,y), (g,f)) => x < g, t)

(*Purpose: a function where similarity is a scoring function, cutoff is a real number, people is the input data
for all of the users. matches (similarity, cutoff, people) should compute a tree of
pairs (person1,person2,score) where
• each score is the similarity score of person1 and person2
• the tree is sorted from highest scores to lowest scores
• only pairs of people whose score is bigger than cutoff are included
• the tree never contains a pair of people of the form (person1,person1, ) or both the
pair (person1,person2, ) and the pair (person2,person1, ). *)
fun matches (similarity : answers * answers -> real,
             cutoff : real,
             people : (string * answers) tree) : (string * string * real) tree = 

            let val Node(l, r) = filter(fn (x,y,z) => z > cutoff, map((fn((g,f), (x,d)) => (g, x, similarity(f,d))),
             duplicates(allpairs(people, people))))
             in
             sort((fn((m1, m2, n1), (m3, m4, n2)) =>(case Real.compare(n1, n2) of
                 LESS => GREATER
                 |EQUAL => EQUAL
                 |GREATER => LESS)), Node(l, r))
             end
(* code for testing *)

val test_data : (string * answers) tree = fromlist [ ("A",(1,1,1,1)), ("B",(2,2,2,2)), ("C",(1,2,2,2)) ]

fun show_matches (similarity : answers * answers -> real, cutoff : real, people : (string * answers) tree) : unit =
    List.app (fn (n1,n2,score) => print (n1 ^ " and " ^ n2 
                                         ^ " have compatibility " ^ (Real.toString score ^ "\n"))) 
             (tolist (matches (similarity, cutoff, people)))

val survey : (string * (int * int * int * int)) tree =
fromlist
[
("adj", (5, 3, 3, 1) ),
("UltrastableGlass", (3, 1, 2, 1 )),
("dontcha", (3,3,3,1)),
("clb", (4, 1, 3, 2)),
("dkw",(3,1,1,1)),
("ad", (5, 3, 3, 1)),
("letsgojets", (3, 1, 1, 2)),
("lalala", (4,3,1,1)),
("012", (5, 1, 3, 2)),
("swk", (5, 3, 1, 2)),
("cerulean33", (1, 3, 3, 2)),
("tlc",(3,1,3,2)),
("acdc",( 5, 1, 3, 2)),
("awg",(2,1,3,1)),
("unik",(4, 3, 3,1 )),
("abm",( 3,3,1,1)),
("pop",(1,1,3,2)),
("adp",(3,3,1,1)),
("jeanralphio",(3,3,1,1)),
("ericartman",(4,3,3,2)),
("xug",( 2, 1, 3, 4)),
("FoxShine",(4,1,1,2)),
("elz",( 2, 1, 1, 1)),
("kurt",(5,3,1,1)),
("computersciencestudent",(5,1,1,1)),
("tree",( 3,1,2,2)),
("jdl", (5, 1, 3, 1)),
("JAF",(2,1,3,1)),
("whosthatpokemon", (5, 3, 3, 2)),
("kots",( 2, 3, 3, 2)),
("rthornley",(3,3,3,2)),
("nkykia",(3,1,1,2)),
("poh",( 5, 3, 3, 2)),
("keyboardcat",( 5, 3, 3, 2)),
("sar",(1,1,1,1)),
("dt",( 1, 3, 3, 1)),
("chrisnolan78",( 4, 3, 3, 1)),
("huh",(2,3,3,1)),
("abc",( 5, 3, 3, 2)),
("bloop",(5,1,1,2))]

(* try running, for example, show_matches(count_same,3.0,survey); *)

