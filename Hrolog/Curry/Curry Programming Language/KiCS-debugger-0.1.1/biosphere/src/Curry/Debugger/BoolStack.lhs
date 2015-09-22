% BoolStack.lhs
% Andreas Baldeau
% April 13, 2009

Overview
========

This module provides an optimized implementation for stacks of `Bool` values.

> module Curry.Debugger.BoolStack (
>     BoolStack,
>     emptyBoolStack,
>     allTrue,
>     pop,
>     push
> ) where

Implementation
==============

Representation
--------------

The stack is repesented by a list of integers. An entry _n_ means there are _n_
`True` values on the stack followed by one `False` value. A list containing only
one `0` represents the empty stack, an empty one is an infinite stack consisting
of only `True` values.

> type BoolStack = [Integer]

> emptyBoolStack :: BoolStack
> emptyBoolStack = [0]

> allTrue :: BoolStack
> allTrue = []

Functions
---------

> pop :: BoolStack -> (BoolStack, Bool)
> pop []       = ([], True)
> pop [0]      = error "pop: Stack underflow"
> pop (0 : os) = (os, False)
> pop (n : os) = (n - 1 : os, True)

> push :: BoolStack -> Bool -> BoolStack
> push []       True  = []
> push []       False = error "push: Cannot push False onto an infinte stack."
> push (b : bs) True  = b + 1 : bs
> push bs       False = 0 : bs

