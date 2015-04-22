A Generic State Transformer
===========================

Since state is a handy thing to have, the standard library includes a
[module][1] `Control.Monad.State` that defines a parameterized version
of the state-transformer monad.  This file is a simplified version of
that library.

We will only allow clients to use the functions declared below.

> {-# LANGUAGE InstanceSigs #-}

> module State (State, get, put, state, runState, evalState, execState) where

The type definition for a generic state transformer is very simple:

> data State s a = S { runState :: s -> (a, s) }

We'll export the S constructor as the function `state`:

> state :: (s -> (a,s)) -> State s a
> state = S

It is a parameterized state-transformer monad where the state is
denoted by type `s` and the return value of the transformer is the
type `a`. We make the above a monad by declaring it to be an instance
of the `Monad` typeclass

> instance Monad (State s) where
>   -- return :: a -> State s a
>   return x   =  state $ \s -> (x,s)
>   (>>=) :: State s a -> (a -> State s b) -> State s b
>   st >>= f   =  state $ \s -> let (a,s') = runState st s in runState (f a) s'

There are two other ways of evaluating the state monad. The first only
returns the final result,

> evalState :: State s a -> s -> a
> evalState st = fst . runState st 

and the second only returns the final state.

> execState :: State s a -> s -> s
> execState st  = snd . runState st 


Accessing and Modifying State
-----------------------------

Since our notion of state is generic, it is useful to write `get` and
`put` functions with which one can *access* and *modify* the state. We
can easily `get` the *current* state via


> get :: State s s
> get = state $ \s -> (s,s)



That is, `get` denotes an action that leaves the state unchanged but
returns the state itself as a value. Note that although `get` *does
not* have a function type (unless you peek under the covers of
`State`), we consider it a monadic "action".

Dually, to *modify* the state to some new value `s'` we can write
the function

> put :: s -> State s ()
> put s' = S $ \s -> ( (), s' )

which denotes an action that ignores (i.e., blows away) the old state
and replaces it with `s'`. Note that the `put s'` is an action that
itself yields nothing (that is, merely the unit value).




[1]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2
