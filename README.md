# Lesson 5 - Connection Pool and Logging

We've added a database connection, but we only have one at the moment.  We'll want to set up a `Pool` to manage multiple connections - opening new ones as necessary and closing old ones as they become idle.  I'm going to use [resource-pool](https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html) to manage our pool - you may prefer another method.

## Setting up Pools in Lib.hs

The function to create a pool is:
```haskell
Data.Pool.createPool :: (IO a) -> (a -> IO ()) -> Int -> NominalDiffTime -> Int -> IO (Pool a)
```

Let's break this down.  `createPool` takes:

1. A function to create a connection, of the form `IO a`.
2. A function to tear down a connectio, of the form `(a -> IO())`.
3. Additional parameters for the number of sub-pools (`Int`), the time before idling out (`NominalDiffTime`, which is a member of the `Num` typeclass which means that we can specify it with a number), and the maximum number of resources to keep open in each subpool (`Int`).

And it returns a `Pool` for whatever `a` we want, wrapped in the `IO` monad.

In this lesson, let's work backward a bit, specifying what we want first and filling out the details afterward.  To start this pool, we'll run:
```haskell
startApp :: IO ()
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  run 8080 (app pool)
```

We'll pass the pool into the server just as we had passed the connection previously.  `Database.PostgreSQL.Simple` closes connections with the appropriately named `close` function, and we're supplying the parameters `1 10 5` for the other arguments (you can choose whatever ones you want instead, if you don't like them).

The only thing left is the `openConnection` function.  The type is going to be whatever `a` we want a `Pool` of - which will be `PGS.Connection`.  From there, we can simply cut-and-paste the connection code we were using previously:
```haskell
openConnection :: IO PGS.Connection
openConnection = PGS.connect PGS.defaultConnectInfo
                 { PGS.connectUser = "blogtutorial"
                 , PGS.connectPassword = "blogtutorial"
                 , PGS.connectDatabase = "blogtutorial"
                 }
```

Lastly, let's change some type signatures to reflect the pass that we're now passing a `Pool` through instead of a `Connection`:
```haskell
readerTToExcept :: Pool.Pool PGS.Connection -> AppM :~> ExceptT ServantErr IO
readerTToExcept pool = Nat (\r -> runReaderT r pool)

app :: Pool.Pool PGS.Connection -> Application
app pool = serve api $ enter (readerTToExcept pool) server
```

## Returning a Connection

If you're looking at the code, you'll see that I use `DBPool` instead of `Pool.Pool PGS.Connection` - after typing out the longer form for a few functions coming up, I made a `type` alias for the combination.  But it compiles to the exact same thing.

So while we're on that topic, let's move over to App.hs and add the alias:
```haskell
type DBPool = Pool Connection
```

Also, our `AppM` no longer returns a `Connection`, so let's update that:
```haskell
type AppM = ReaderT DBPool (ExceptT ServantErr IO)
```

Finally, we'll want a way to grab a `Connection` from the `Pool` while working in the API files.  However, remember all that work we went through in Lesson 4 to abstract away details of our database connections?  We don't want to undo that, do we?  So we'll write a function in App.hs which will handle grabbing a function from the `Pool`:
```haskell
getConn :: DBPool -> AppM Connection
getConn pool = withResource pool return
```

Given a pool (supplied by the `ReaderT` portion of `AppM`), we can return a `Connection` within a monad.  `withResource` takes a pool and a monadic action (the type is `Pool a -> (a -> m b) -> m b`) and returns something in a monad.  Since our API logic will be in the `AppM` monad, we'll use that in the type signature.

If you weren't sure what monad to use, you could just rest on the flexibility of the `return` function to provide the correct one.  Delete or comment out the type signature, add the `{-# LANGUAGE FlexibleContexts #-}` pragma when GHC yells at you to do so, and the compiler will figure out what you need.

## Changing the API

We've already done almost all the work.  The problem left is that Api.User and Api.BlogPost had been accustomed to calling `ask` and receiving a `Connection`.  Now, when they try, they'll get a `DBPool` instead.

But we also have this nice function we just made, `getConn`, which can take a `DBPool` and return an `AppM Connection`.  All we have to do is figure out how to wrangle the monads.

`ask` has the type `ReaderT r m r` - it returns a monadic value in the `ReaderT` monad transformer (and remember, `AppM` is really `ReaderT DBPool (ExceptT ServantErr IO)`, so it is a `ReaderT`).  How do we take a monadic value out of the monad?  Use `do`:
```haskell
do pool <- ask
   con <- getConn pool
   ...
```

`getConn pool` is also within a monad, so we have to extract it as well.  But with that, we are done.  We can even clean this up a bit, removing an extra line and a temporary variable:
```haskell
do con <- ask >>= getConn
```
Any time you see a variable in a `do` block which is only there to chain monadic steps together, you can cut it out and just use bind `>>=`.

Repeat this and change every instance of `con <- ask` to `con <- ask >>= getConn`, and we're done.  `stack build` it and run it.
