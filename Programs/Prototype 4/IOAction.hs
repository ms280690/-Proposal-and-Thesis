-- http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/

data IOAction a = 
-- A container for a value of type a.
                  Return a 
-- A container holding a String to be printed to stdout, followed by another IOAction a.
                | Put String (IOAction a) 
-- A container holding a function from String -> IOAction a, which can be applied to whatever String is read from stdin.
                | Get (String -> IOAction a)
{--

Return 1

Put "hello" (Return ())
Put "hello" (
  Return ()
)

Put "hello" (Return 1)
Put "hello" (
  Return 1
)

Put "hello" (get)
Put "hello" (
  Get ($0 -> 
    Return "$0"
  )
)

Get put
Get ($0 -> 
  Put "$0" (
    Return ()
  )
)

--}

-- Read and return
get :: IOAction String
get   = Get Return
{--

Get ($0 -> 
  Return "$0"
)

--}

-- Print and return.
put :: String -> IOAction ()
put s = Put s (Return ())
{--

put "hello"
Put "hello" (
  Return ()
)

--}

-- (>>=) Action sequencer and combiner :- read -> write -> read -> write -> ........
seqio :: IOAction a -> (a -> IOAction b) -> IOAction b
--      (First action   (Take and perform                       
--      which generates  next action)
--      value a) 
seqio (Return a) f = f a
seqio (Put s io) f = Put s (seqio io f)
seqio (Get g)    f = Get (\s -> seqio (g s) f)

--Take input and print.
echo :: IOAction ()
echo = get `seqio` put
{--

Get ($0 -> 
  Put "$0" (
    Return ()
  )
)

--}

hello :: IOAction ()
hello = put "What is your name?"      `seqio` \_    ->
        get                           `seqio` \name ->
        put "What is your age?"       `seqio` \_    ->
        get                           `seqio` \age  ->
        put ("Hello " ++ name ++ "!") `seqio` \_    -> 
        put ("You are " ++ age ++ " years old")
{--

Put "What is your name?" (
  Get ($0 -> 
    Put "What is your age?" (
      Get ($1 -> 
        Put "Hello $0!" (
          Put "You are $1 years old" (
            Return ()
          )
        )
      )
    )
  )
)

run hello
What is your name?
Mehul
What is your age?
25
Hello Mehul!
You are 25 years old

--}

-- hello in "do" block since IOAction is a Monad
hello2 :: IOAction ()
hello2 = do put "What is your name?"
            name <- get
            put "What is your age?"
            age <- get
            put ("Hello, " ++ name ++ "!")
            put ("You are " ++ age ++ " years old!")
{--

Put "What is your name?" (
  Get ($0 -> 
    Put "What is your age?" (
      Get ($1 -> 
        Put "Hello, $0!" (
          Put "You are $1 years old!" (
            Return ()
          )
        )
      )
    )
  )
)

run hello2
What is your name?
Mehul
What is your age?
25
Hello, Mehul!
You are 25 years old!

--}

-- where the effects happen.
-- "Real" IO functions like return, putStrLn, getLine.
run :: IOAction a -> IO a
run (Return a) = return a
run (Put s io) = putStrLn s >> run io
run (Get f)    = getLine >>= run . f
{--

run (Return 1)
1

run (Put "hello" get)
hello
1
"1"

run (Get put)
1
1

--}


-- Glue code that makes everything play nice --

instance Monad IOAction where
    return = Return
    (>>=)  = seqio

instance Show a => Show (IOAction a) where
  show io = go 0 0 io
    where
      go m n (Return a) = ind m "Return " ++ show a
      go m n (Put s io) = ind m "Put " ++ show s ++ " (\n" ++ go (m+2) n io ++ "\n" ++ ind m ")"
      go m n (Get g)    = let i = "$" ++ show n
                          in ind m "Get (" ++ i ++ " -> \n" ++ go (m+2) (n+1) (g i) ++ "\n" ++ ind m ")"

      ind m s = replicate m ' ' ++ s

-- IOAction is also a Functor --

mapio :: (a -> b) -> IOAction a -> IOAction b
mapio f (Return a) = Return (f a)
mapio f (Put s io) = Put s (mapio f io)
mapio f (Get g)    = Get (\s -> mapio f (g s))
{--

mapio (+1) (Return 1)
Return 2

mapio (id) (Put "hello" get)
Put "hello" (
  Get ($0 -> 
    Return "$0"
  )
)

mapio (id) (Get put)
Get ($0 -> 
  Put "$0" (
    Return ()
  )
)

--}

instance Functor IOAction where
    fmap = mapio


