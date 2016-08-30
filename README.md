# Lesson 4 - Transformers

Abstractions in disguise.  In Lesson 2, we added a bunch of database connection information.  Now, we need that, or we can't connect to a database, which defeats the purpose of all of that Opaleye work we did.  However, we now have to thread all of that connection information around to the entire program.  That may not be too terrible, but it does require that every single file which uses database connections at all will need to depend on `Database.PostgreSQL.Simple`.  What would happen if we used a different method of connecting to the database?  Is there any way of ensuring that everything will have proper dependecies without us having to manually type them in for every function?  We are Haskell programmers, after all; abstracting away redundancy and automating dependencies is what we are about.

Fear no more: by using the power of Monad Transformers, we'll be able to tuck away all of our connection info in a tidy way.  Furthermore, this will apply to any sort of configuration information you may want to pass along; perhaps environment variables, or portions of your server which you want to keep highly configurable.

In this lesson, we'll work with `ReaderT`, as that provides the simplest monad transformer for our purposes.  In a later lesson, we'll find a more efficient approach.

## Step 1: Update App.hs

Let's go back to App.hs.  Add in the following imports:
```haskell
import Control.Monad.Trans.Reader (ReaderT)
import Database.PostgreSQL.Simple (Connection)
```

And this is the reason why we created it:
```haskell
type AppM = ReaderT Connection (EitherT ServantErr IO)
```
All we do is revise our definition of `AppM`, and most of our heavy lifting work is done.  Everything is already set up to use `AppM`.

## Step 2: Update Lib.hs

We'll need to do a little work in Lib.hs as well.  Servant knows how to use `EitherT ServantErr IO`; it doesn't know what to do with our new `AppM`.

Before we get to the code, let's think about what `AppM` could be.  (I know I gave a solution already; forget that for a moment.)  On the one hand, carrying configuration around with us strongly suggests using `Reader` or `ReaderT`.  However, we need to get back to `EitherT ServantErr IO` through a *natural transformation* (the Servant tutorial has more information on that [here](https://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers); in short, instead of transforming our data directly, like with a Functor, with want a way to transform the transformation of data.  We've got a monad of some sort, most likely a `ReaderT Connection a b` where `a` and `b` are something else, and we need to get that to `EitherT ServantErr IO b`, without really playing around with `b`.)

Maybe there are several ways of doing this.  But a very simple one is to choose `ReaderT Connection (EitherT ServantErr IO)`.  We take connection information from the monad transformer, and apply the transform step of `ReaderT` to get exactly what we wanted: `EitherT ServantErr IO`.  The complicated type signature is really the simplest processing we could do.

The code to carry out this transformation is the following:
```haskell
readerTToEither :: AppM :~> EitherT ServantErr IO
readerTToEither = Nat (\r -> do con <- liftIO $ PGS.connect PGS.defaultConnectInfo
                                                              { PGS.connectUser = "blogtutorial"
                                                              , PGS.connectPassword = "blogtutorial"
                                                              , PGS.connectDatabase = "blogtutorial"
                                                              }
                                runReaderT r con)
```

We'll need to make two other changes to accomodate this is Lib.hs.  First, `server` is now a `ServerT API AppM` instead of a `Server API`:
```haskell
server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
```
And we'll need to actually run the transformation:
```haskell
app :: Application
app = serve api $ enter readerTToEither server
```

Finally, since the database connection information is sent in with the `ReaderT` transformer, we no longer need to add it in to `startApp`:
```haskell
startApp :: IO ()
startApp = run 8080 app
```

## Step 3: Ask, Don't Argue

Now that we've done that, let's go back to our API files.  As usual, I'll go through Api/User.hs, then suggest Api/BlogPost.hs as an exercise.

There are two main steps here.  First, we need to remove all of our references to "PGS.Connection" and the resulting `con` argument.

However, we still need that connection.  So instead, we'll `ask` for it, and receive it from the `AppM` monad we're working within.
```haskell
getUsers :: AppM [UserRead]
getUsers = do con <- ask
              liftIO $ runQuery con usersQuery

getUserByEmail :: Email -> AppM (Maybe UserRead)
getUserByEmail email = do con <- ask
                          liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

verifyUser :: UserWrite -> AppM Bool
verifyUser user = do con <- ask
                     dbUser <- getUserByEmail (userEmail user)
                     return $ compareUsers dbUser user

postUser :: UserWrite -> AppM Int64
postUser user = do con <- ask
                   newUser <- liftIO $ userToPG user
                   liftIO $ runInsert con userTable newUser
```
This is little work at this point, since we already have most of the code set up to use monads and database connections.  However, you might be wondering whether this is all busy work; we just moved the `con` from an argument to `con <- ask`, so what have we gained?

Three things: first, we no longer have to worry about the type of `con`.  It's no longer part of a type signature; as long as we use something Opaleye can play nice with, our API doesn't need to care.  Second, we can much more easily change whether a given function relies on a database connection or not.  Instead of going back and making sure that the connection is passed in properly as an argument every step of the way, we can just ask for it when we need it, and not ask if we don't.

Third, this sets us up nicely for other information we might need through a `ReaderT` monad.  If you want to read off environment variables for some reason, that would be relatively easy to add at this point.

And yes, go back and change Api/BlogPost.hs accordingly.

## Step 4: Update Cabal

Update dependencies, build, exec, curl.  Profit.
