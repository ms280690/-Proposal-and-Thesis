

--Mehul Solanki.


-- Curry Example.
{--
The paper Embedding Prolog gives a small example of the append function. The following is a "Prologish" version of that. Paper is attached
--}


{--
:t (=:=)
(=:=) :: a -> a -> Success

The operator that does the magic.
--}

--append [] q r = (q=:=r)
--append (x:xs) q (x:ys) = append xs q ys  

{--
append [] [1] a where a free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [] [1] a where a free
{a = [1]} Success

append [1,2] [3,4] a where a free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [1,2] [3,4] a where a free
{a = [1,2,3,4]} Success

append [1] [] a where a free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [1] [] a where a free
{a = [1]} Success

append [] [1,2] [1,2]
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [] [1,2] [1,2]
Success

append [] [1,2] [3,4]
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [] [1,2] [3,4]
!

append [1,a,2,3] [3,c,4,5] d where a,c,d free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: append [1,a,2,3] [3,c,4,5] d where a,c,d free
{a = _x8, c = _xq, d = [1,_x8,2,3,3,_xq,4,5]} Success

--}






-- Something like Prolog

append1 :: [a] -> [a] -> [a]
append1 [] x = x
append1 (y:ys) x = [y] ++ (append1 ys x)

{--
(append1 a b) =:= [1,2] where a,b free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: (append1 a b) =:= [1,2] where a,b free
{a = [], b = [1,2]} Success
{a = [1], b = [2]} Success
{a = [1,2], b = []} Success


(append1 a b) =:= [1..] where a,b free    
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: (append1 a b) =:= [1..] where a,b free
Stack space overflow: current size 8388608 bytes.
Use `+RTS -Ksize -RTS' to increase it.
Evaluation terminated with non-zero status 2


(x:xs) =:= [1,2,3] where x,xs free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: (x:xs) =:= [1,2,3] where x,xs free
{x = 1, xs = [2,3]} Success
--}


data Stack e = Empty | Push e (Stack e)

top Empty = Empty
top (Push e _) = e

pop Empty = Empty
pop (Push _ s) = s


{--
Left Linear function as each variable 
--}
member _ [] = False
member x (y:ys) = x == y || member x ys


insert x y = x:y
insert x (y:ys) = y:insert x ys

{--
insert x [1..5] where x free
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: insert x [1..5] where x free
{x = _x5} [_x5,1,2,3,4,5]
{x = _x5} [1,_x5,2,3,4,5]
{x = _x5} [1,2,_x5,3,4,5]
{x = _x5} [1,2,3,_x5,4,5]
{x = _x5} [1,2,3,4,_x5,5]
{x = _x5} [1,2,3,4,5,_x5]


insert 1 [2..5]
[1 of 3] Skipping  Prelude          ( /home/mehul/kics2-0.3.1/lib/Prelude.curry, /home/mehul/kics2-0.3.1/lib/.curry/Prelude.fcy )
[2 of 3] Skipping  Program1         ( Program1.curry, .curry/Program1.fcy )
[3 of 3] Compiling Curry_Main_Goal  ( Curry_Main_Goal.curry, .curry/Curry_Main_Goal.fcy )
Evaluating expression: insert 1 [2..5]
[1,2,3,4,5]
[2,1,3,4,5]
[2,3,1,4,5]
[2,3,4,1,5]
[2,3,4,5,1]
--}


main = print "Hello"