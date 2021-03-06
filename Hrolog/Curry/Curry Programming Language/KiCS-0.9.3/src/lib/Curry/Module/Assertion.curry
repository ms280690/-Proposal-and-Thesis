------------------------------------------------------------------------------
--- This module defines the datatype and operations for the
--- Curry module tester "currytest".
---
--- @author Michael Hanus, Bernd Brassel
--- @version July 2006
------------------------------------------------------------------------------

module Assertion(Assertion(..), -- for writing test cases
                 -- the remaining entities are only used by the test tool:
                 checkAssertion,
                 seqStrActions,writeAssertResult,
                 ProtocolMsg(..),
                 showTestMod,showTestCase,showTestEnd,showTestCompileError) where

import List((\\))
import IO

infixl 1 `seqStrActions`

--- Datatype for defining test cases.
--- @cons AssertTrue   s b     - assert (with name s) that b must be true
--- @cons AssertEqual  s e1 e2 - assert (with name s) that e1 and e2 must
---                              be equal (w.r.t. ==)
--- @cons AssertValues s e vs  - assert (with name s) that vs is the multiset
---                              of all values of e (i.e., all values of e are
---                              compared with the elements in vs w.r.t. ==)
--- @cons AssertSolutions s c vs - assert (with name s) that constraint
---   abstraction c has the multiset of solutions vs
---   (i.e., the solutions of c are compared with the elements in vs w.r.t. ==)
--- @cons AssertIO     s a r   - assert (with name s) that I/O action a
---                              yields the result value r
--- @cons AssertEqualIO s a1 a2 - assert (with name s) that I/O actions a1 and
---                               a2 yield equal (w.r.t. ==) results
data Assertion a = AssertTrue      String Bool
                 | AssertEqual     String a a
                 | AssertValues    String a [a]
                 | AssertSolutions String (a->Success) [a]
                 | AssertIO        String (IO a) a
                 | AssertEqualIO   String (IO a) (IO a)


--- Combines two actions and combines their results.
--- Used by the currytest tool.
seqStrActions :: IO (String,Bool) -> IO (String,Bool) -> IO (String,Bool)
seqStrActions a1 a2 =
  do (s1,b1) <- a1
     (s2,b2) <- a2
     return (s1++s2,b1&&b2)

--- Executes and checks an assertion, and process the result
--- by an I/O action.
--- Used by the currytest tool.
--- @param protocol - an action to be applied after test execution
--- @param assertion - an assertion to be tested
--- @return a protocol string and a flag whether the test was successful
checkAssertion :: ((String,Bool) -> IO (String,Bool)) -> Assertion _
                                                      -> IO (String,Bool)
checkAssertion prot (AssertTrue name cond) =
  catchFail (checkAssertTrue name cond)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertEqual name call result) =
  catchFail (checkAssertEqual name call result)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertValues name expr results) =
  catchFail (checkAssertValues name expr results)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertSolutions name constr results) =
  catchFail (checkAssertSolutions name constr results)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertIO name action result) =
  catchFail (checkAssertIO name action result)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertEqualIO name action1 action2) =
  catchFail (checkAssertEqualIO name action1 action2)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot

-- Checks Boolean assertion.
checkAssertTrue :: String -> Bool -> IO (String,Bool)
checkAssertTrue name cond =
  if cond
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": assertion not satisfied\n",False)

-- Checks equality assertion.
checkAssertEqual :: String -> a -> a -> IO (String,Bool)
checkAssertEqual name call result = do
  let r = call
  if r==result
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": equality assertion not satisfied:\n"++
                "Computed answer: "++show r++"\n"++
                "Expected answer: "++show result++"\n",False)

-- Checks all values assertion.
checkAssertValues :: String -> a -> [a] -> IO (String,Bool)
checkAssertValues name call results = do
  st <- getSearchTree call 
  let rs = allValuesB st
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": values assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks all solutions of a constraint abstraction.
checkAssertSolutions :: String -> (a->Success) -> [a] -> IO (String,Bool)
checkAssertSolutions name constr results = do
  st <- getSearchTree (let x free in (x,constr x)) 
  let rs = map fst (allValuesB st)
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": solutions assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks an IO assertion.
checkAssertIO :: String -> IO a -> a -> IO (String,Bool)
checkAssertIO name action result = do
  r <- action
  if r==result
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO assertion not satisfied:\n"++
                 "Computed answer: "++show r++"\n"++
                 "Expected answer: "++show result++"\n\n",False)

-- Checks equality of results of two IO assertions.
checkAssertEqualIO :: String -> IO a -> IO a -> IO (String,Bool)
checkAssertEqualIO name action1 action2 = do
  r1 <- action1
  r2 <- action2
  if r1==r2
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO equality assertion not satisfied:\n"++
                 "Computed answer 1: "++show r1++"\n"++
                 "Computed answer 2: "++show r2++"\n\n",False)

--- Writes the results of assertion checking into a file and stdout,
--- if the results are non-empty.
--- Used by the currytest tool.
writeAssertResult :: (String,Bool) -> IO ()
writeAssertResult (result,flag) =
  if flag
  then putStrLn (result++"All tests successfully passed.")
  else putStrLn (result++"FAILURE occurred in some assertions!\n")


----------------------------------------------------------------------------
-- The following entities are used to implement the test GUI:

--- The messages sent to the test GUI.
--- Used by the currytest tool.
data ProtocolMsg = TestModule String | TestCase String Bool | TestFinished
                 | TestCompileError

doSend :: Handle -> a -> IO ()
doSend h x = hPutStrLn h (show x)

--- Sends message to GUI for showing test of a module.
--- Used by the currytest tool.
showTestMod :: Handle -> String -> IO ()
showTestMod port modname = doSend port (TestModule modname)


--- Sends message to GUI for showing result of executing a test case.
--- Used by the currytest tool.
showTestCase :: Handle -> (String,Bool) -> IO (String,Bool)
showTestCase port (s,b) =
  doSend port (TestCase s b) >>
  return (s,b)

--- Sends message to GUI for showing end of module test.
--- Used by the currytest tool.
showTestEnd :: Handle -> IO ()
showTestEnd port = doSend port TestFinished

--- Sends message to GUI for showing compilation errors in a module test.
--- Used by the currytest tool.
showTestCompileError :: Handle -> IO ()
showTestCompileError port = doSend port TestCompileError

-- end of module Assertion
