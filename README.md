# Lesson 6 - Logging

This lesson will continue in the same fashion as the previous one - we'll add more capabilities to our server through padding `AppM`.  Along the way, we'll also be spending time in [Hackage](https://hackage.haskell.org) working with Haskell packages.

## Step 1: Make a Config Datatype

Head on over to App.hs, and let's work out what we want to do.  We'll want to add a logger; we could just add one which would alert us to each request sent to the server (and I'll show you how to do that later), but it's much more useful to also have a way to log our own messages as we go.

Having something "as we go" sounds like a perfect use of the `ReaderT` monad, so let's update our datatypes:
```haskell
type DBPool = Pool Connection

data Config = Config
              { getPool :: DBPool
              , getLogger :: LoggerSet
              }

type AppM = ReaderT Config (ExceptT ServantErr IO)
```
Instead of carrying around just a `DBPool`, we put all of our datatypes into a single `Config`.  Currently, we've got just the `DBPool` and a `LoggerSet`, but we can add whatever to want now.

`LoggerSet` comes from [System.Log.FastLogger](https://hackage.haskell.org/package/fast-logger-2.4.6/docs/System-Log-FastLogger.html).  In this lesson, we'll focus on adding a logger to `stdout`, but you can find alternative functions for adding a logger to a file handle as well ([System.IO](https://hackage.haskell.org/package/base-4.9.0.0/docs/System-IO.html) has the resources for using file handles).

Remember how at the end of the last lesson we abstracted out `getConn` and added it to App.hs?  That's going to pay off now, since we now have to change the way we grab a database connection:
```haskell

getConn :: AppM Connection
getConn = ask >>= getConnFromPool . getPool
```

It's an easy change - we just make sure to `getPool` from the `Config` before we call `getConnFromPool`.  But we can do that in one place now rather than across our entire API.

And since we're already implementing a way to grab our connection, no matter how we change types around, let's write a function to add a message to our logger:
```haskell
addToLogger :: LogStr -> AppM ()
addToLogger msg = ask >>= \cfg -> liftIO $ pushLogStrLn (getLogger cfg) msg
```
`ask` for the configuration, then `getLogger` from it and `pushLogStrLn` to add the `msg` to the logger along with a newline (use `pushLogStr` if you don't want newlines).  But if you're paying attention to types, you'll notice that `pushLogStrLn` returns `IO ()`, when we need `AppM ()` instead.  Since `AppM` does include `IO` in one of its transformation steps, we can use `liftIO` to make this transformation.

One thing to note is that `LogStr` is an instance of `IsString`.  What that means is that you can use the `{-# LANGUAGE OverloadedStrings #-}` extension to specify a `LogStr` using normal strings.  This will cut down on imports in the Api files; they don't need to know what a `LogStr` is, and there are plenty of other reasons for using `OverloadedStrings` in a web server (handling `Text` and `ByteString`s, for instance).

## Step 2: Implement the Logger

Next, we'll need to make a logger to put into that config. Let's head over to the `startApp` function.  Using our newfound knowledge from the `FastLogger` hackage page, we know that we can use the `newStdoutLoggerSet` function to create a logger:
```haskell
startApp :: IO ()
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newStdoutLoggerSet defaultBufSize
  run 8080 $ app (Config pool logger)
```
And this by itself can already be useful - we now have a logger which can take any messages we add, threaded through our API via `AppM`.  Just change the appropriate type signatures:
```haskell
readerTToExcept :: Config -> AppM :~> ExceptT ServantErr IO

app :: Config -> Application
```

## Step 3: Middleware

But we can do more.  Servant is built on top of Wai, the Web Application Interface.  And Wai lets us add middleware - in particular, we can set up the server to automatically log requests for us, intead of the server just taking messages which we send in.

The list of things you can do with Wai middleware logging can be found at: [Network.Wai.Middleware.RequestLogger](https://hackage.haskell.org/package/wai-extra-3.0.16.1/docs/Network-Wai-Middleware-RequestLogger.html).  Two standard logging functions found there, `logStdout` and `logStdoutDev`, could have been set up without any of the changes to our cofiguration and types:
```haskell
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  run 8080 $ logStdoutDev $ app pool
```

We just replaced `run 8080 $ app pool` with `run 8080 $ logStdoutDev $ app pool`.  At this level, if we're seeking out information in the docs to help us out, we should be thinking about the types we're using - that will drastically cut down our search space.  So if that's something you're already used to, hop on down to the next code fragment.  Otherwise, let's step through what is going on here.

#### Wai Types: Aside

Before looking `logStdoutDev` up, what sort of signature lets it be placed smack-dab in the middle of a list of arguments like that?  Haskell doesn't let us have an abitrary number of arguments, so how can we be able to throw as many `Middleware` components in there as we need?

The answer is, we need a function which returns the same sort of thing which it takes, so that from a type signature standpoint we don't change anything.  In the case of Wai, the `Middleware` type is an alias for `Application -> Application`.  An `Application` in turn is an alias for `Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived`.  So `Middleware` is anything which can alter the `Request` or the way a `Response` is handled, but which also returns the exact same sort of processing (type-wise) as it received.

---

We already went to the trouble of implementing our new `Config`, so it would be a shame to waste that.  We'll take a slightly more complicated route toward adding a logger:
```haskell
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newStdoutLoggerSet defaultBufSize
  midware <- mkRequestLogger $ def { destination = Logger logger }
  run 8080 $ midware $ app (Config pool logger)
```

We'll make that `logger` just like we did earlier.  But we'll also make `midware` using `mkRequestLogger`. `def` comes from [Data.Default.Class](https://hackage.haskell.org/package/data-default-class-0.1.2.0/docs/Data-Default-Class.html) and lets us specify (not unsurprisingly) a default option, so that we don't have to mess with all of the Wai `Middleware` logging options.  All we want to do is change one part, the destination for the logging, and we can use Haskell's record update syntax for that: `def { destination = Logger logger }` means: give us the `def` for this instance, but with the `destination` field set to `Logger logger`.  This is the same `logger` we're passing to the `Config`, so it will receive both our messages and Wai's messages.

I've left our logger on `stdout` for simplicity, but you can easily output to a file instead:

```haskell
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newFileLoggerSet "log.txt" defaultBufSize
  midware <- mkRequestLogger $ def { destination = Logger logger }
  run 8080 $ midware $ app (Config pool logger)
```

## Step 4: Add Messages

Feel free to have some more fun here.  The repository code adds a few messages: one on startup:
```haskell
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newStdoutLoggerSet defaultBufSize
  midware <- mkRequestLogger $ def { destination = Logger logger }
  pushLogStrLn logger "Hello World"
  run 8080 $ midware $ app (Config pool logger)
```

And more in the API:
```haskell
getPosts :: AppM [BlogPostRead]
getPosts = do con <- getConn
              addToLogger "I got BlogPosts!"
              liftIO $ runQuery con blogPostsQuery
```
(Again, remember to add `OverloadedStrings` if you want to log messages using plain strings.)

## Step 5: Changing Formatting

If you played around with `logStdout` and `logStdoutDev` at all, you saw that `logStdoutDev` prints out pretty colors in a nicely formatted way while `logStdout` just gives a plain line of text, Apache-style.  If you played around further and tried to put the formatted text into a file, you saw that it is pretty much unreadable.  So the last thing to see before closing out this lesson is how to change the format of the logger:
```haskell
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newFileLoggerSet "log.txt" defaultBufSize
  midware <- mkRequestLogger $ def { destination = Logger logger, outputFormat = Apache FromSocket }
  pushLogStrLn logger "Hello World"
  run 8080 $ midware $ app (Config pool logger)
```
You can also specify your own format with `CustomOutputFormat OutputFormatter` and `CustomOutputFormatWithDetails OutputFormatterWithDetails` if you feel like it (the [hackage page](https://hackage.haskell.org/package/wai-extra-3.0.16.1/docs/Network-Wai-Middleware-RequestLogger.html) has information on how to set those up), but `Apache FromSocket` does just fine to give us the straight, unformatted facts.

## Step 6: Dependencies and Run

This step added quite a few dependencies to the .cabal file and to imports (just for Lib.hs and App.hs, though).  If you worked through these examples yourself instead of cloning the repository, give those files a look to make sure that you've included everything you need.  Then, build and exec and enjoy chatting with your server!

We've covered multiple ways to log information: including a logger in `AppM` or just running one as `Middleware`, logging to `stdout` or to a file, and formatting a logger.  We don't want to have to recompile every time we decide to run our server differently, though.  So the next lesson will use environment variables to control how we log, as well as other details such as the port we run on.
