# Lesson 3 - Encryption

Compared to the last lesson, this one is simple.  We will make sure that all passwords are encrypted prior to insertion into the database.

While that is the concrete thing we will study, this lesson is also about working in `IO` to a program.  Haskell can be a wonderful language to program in, but then one gets to basic operations like random numbers.  Instead of importing a library and spitting out a number, like in most other languages, Haskell makes us wrestle with monads and monad transformers.

So a main point of this tutorial is that it really is not so difficult to add `IO` with what we have already set up.  Password encryption is just one particular form of `IO` to work on.

Also, we've done most of our work already.  If you've set up Servant and Opaleye, almost everything from there is tweaking the system for details.  Pat yourself on the back.

## Step 1: Add PasswordStore

Go to "src/Models/User.hs", and add to your imports:
```haskell
import Crypto.PasswordStore
```

## Step 2: Update User Types

We run into a problem: passwords will be encoded as `ByteString`s, but passwords submitted through JSON will be plain `String`s.  (Unless we encrypt beforehand, but then we'd hardly have a tutorial here, would we?)  Just like with the `BlogPost'` concrete types, we'll need to have separate types for `UserRead` and `UserWrite`:
```haskell
type UserRead = User' Email ByteString
type UserWrite = User' Email String
```
And we'll need to update the `ToJSON` and `FromJSON` instances accordingly:
```haskell
instance ToJSON UserRead where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON UserWrite where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              o .: "password"
  parseJSON _ = mzero
```
Notice that, since `UserWrite` has gone back to using `String`s, we no longer have to `pack` the password.

## Step 3: Encrypt on Conversion

We'll edit our conversion function to encrypt the password:
```haskell
userToPG :: UserWrite -> IO UserColumn
userToPG user = do hashedPwd <- flip makePassword 12 . BS.pack . userPassword $ user
                   return
                     User { userEmail = pgString . userEmail $ user
                          , userPassword = pgStrictByteString hashedPwd
                          }
```
`makePassword plaintext_as_bytestring strength` creates a Bcrypted password.  However, this returns an `IO Bystring`, which means that we will have to 'return' our `UserColumn` in the `IO` monad as well.  To accomplish this, just use `do` notation to handle the different steps.  This does mean that we are unable to use our product profunctor any longer (or, at least, that I am unable to; if someone else has an elegant solution to this which can continue to use profunctors without adding obfuscation, please feel free to submit a pull request).

While we're at it, we'll also want to compare a `UserRead` from the database, to a `UserWrite` submitted through JSON, to check to see if they have the same passwords.  Fortunately, this does not require `IO`:
```haskell
compareUsers :: Maybe UserRead -> UserWrite -> Bool
compareUsers Nothing _ = False
compareUsers (Just dbUser) userAttempt = verifyPassword (BS.pack . userPassword $ userAttempt)
                                                        (userPassword dbUser)
```

## Step 4: Update User API

Time to change our "Api/User.hs" file.  We'll be running encryption when we __POST__ a new user, so that's the function which will need to be updated.  Now, we're already working within the `AppM` monad; that is, `ExceptT ServantErr IO`, and it can handle `IO` just fine.  (Which is good, since we've been using plenty of `IO` when running database queries.)  To slip in the encryption step:
```haskell
postUser :: PGS.Connection -> UserWrite -> AppM Int64
postUser con user = do newUser <- liftIO $ userToPG user
                       liftIO $ runInsert con userTable newUser
```
It's almost the same as before.  We just need to grab the `newUser` out of an `IO` action before inserting it into the database.  And anytime you find yourself thinking, "I almost have the value I want, but it's stuck in a monad; what do I do?", then remember that you do `do`.

While we're here and we've added encrypted passwords, let's add a new endpoint for verification:
```haskell
type UserAPI = Get '[JSON] [UserRead]
          :<|> Capture "email" Email :> Get '[JSON] (Maybe UserRead)
          :<|> "verify" :> ReqBody '[JSON] UserWrite :> Post '[JSON] Bool
          :<|> ReqBody '[JSON] UserWrite :> Post '[JSON] Int64

userServer :: PGS.Connection -> Server UserAPI
userServer con = getUsers con
            :<|> getUserByEmail con
            :<|> verifyUser con
            :<|> postUser con

verifyUser :: PGS.Connection -> UserWrite -> AppM Bool
verifyUser con user = do dbUser <- getUserByEmail con (userEmail user)
                         return $ compareUsers dbUser user
```
Add it to the API signature, add it to the server, add a function for it.  In the function, grab a user from the database, and `return` the result of `compareUsers`.

## Step 5: Update Cabal and Run

As usual, make sure that you let Cabal know that you are now dependent on `Crypto.PasswordStore`.  Then `stack build`, `stack exec blog-tutorial-exe`, and `curl` away.
