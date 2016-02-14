# Lesson 1 - Servant API

Here, we'll set up a very basic Servant API.  It won't do much; our __GET__ requests will return hardcoded data, and our __POST__ requests will just tack on new data to a response.  But we will be able to use almost all of the API functionality as we go forward.

Note: The setup I have for separating out files and folders is purely my own.  I think it makes sense, and I'll explain why I chose it, but the important concerns are modularity and composability rather than my own specific idiosyncracies.  Feel free to change any of this for your own needs.

## Step 1: Create App.hs

This is the place where I generally put basic information for the entire application.  For example, I get tired of typing `EitherT ServantErr IO` all of the time, so I've created a type alias `AppM` instead.  Once we get to Lesson 4, we'll see that this setup will greatly ease our transition into more complex transformers.

Similarly, I've created typealiases for `BlogPostId` and `Email` as well.  In theory, the rest of the code should just have to know that it is dealing with *emails* and *ids*, and not worry about the underlying representation.  It is, of course, more complicated than that, since we'll also have to connect up Haskell's representation with the database and with JSON inputs, but this is a start.

## Step 2: Create API directior

The default setup provided by Stack places all of the API information in the Lib.hs file.  That's nice for a quick and dirty website, but we want to get maximum reuse out of our components.  We might be writing other websites with users, for example - it happens from time to time.  So let's create a directory just for API files, and we'll get to work writing *API/User.hs* and *API/BlogPost.hs*.

## Step 3: Set up User API

### Datatype

To start, take the *User* information which Stack graciously provided in Lib.hs, and move it to it's own file.  We'll rename things, since it will now be just part of the API rather than the entire thing.  To keep things simple, we will have just two fields: an email (which will double as a unique identifier later) and a password.  We've already set up the *Email* type alias in the App.hs file; we'll want to refer to *Email*s from the *BlogPost* API file as well, but we don't necessarily want *BlogPost*s to be dependent on our implementation of *User* beyond this one detail.  We'll end up with this:

```{haskell}
data User = User
  { userEmail    :: Email
  , userPassword :: String
  } deriving (Eq, Show)
```

### JSON

Next, let's create JSON representations.  We will use a different representation when converting to JSON as opposed to converting from JSON, so we'll have to roll our own `toJSON` and `parseJSON` functions.  When converting `toJSON`, we'll just package up the `userEmail` field.  However, whene using `parseJSON`, we'll take in both a `userEmail` and a `userPassword`.

(If you have not used AESON before to convert to/from JSON, what we are doing is this: in `toJSON`, we set up an `object`, which matches up JSON fields with whatever we want.  Here, I lined up the field "email" with `userEmail user`, but I could have just set all emails to "bob@juno.com" if I felt like it.  To convert from JSON, we set up a `parseJSON` function, which takes an `object` and parses out the fields using `.:`.  So, for example, `object .: "email"` is something like `javascript object.email` in javascript, which can then be used as part of a *User* datatype.  The main gotcha to watch out for is that AESON uses `Text` instead of `String`, so we have to add `{-# LANGUAGE OverloadedStrings}` if we don't feel like manually packing each `String`.)

### API

What good is an API without an API?  We'll set up the endpoints as such:
```{haskell}
type UserAPI = Get '[JSON] [User]
               :<|> Capture "email" Email :> Get '[JSON] (Maybe User)
               :<|> ReqBody '[JSON] User :> Post '[JSON] [User]
```
We'll have a root endpoint which responds to a __GET__ and returns a List of *User*s in JSON format.  Next, we'll add an endpoint at "/{someone's email}" which will look up our current users and `Maybe` return one.  Finally, we'll let people __POST__ to our mini-server a *User*, and which will return a list of *User*s, also in JSON.

Then, we'll make sure to add a `Proxy` for our API:
```{haskell}
userAPI :: Proxy UserAPI
userAPI = Proxy
```
This is a little bit of boilerplate which lets the Type system interact with values which we pass around.  Don't worry about it too much.

### Server

Above, we've set out our API.  We have our endpoints, what they expect from us, and what we expect from them.  This is really just setting up type signatures, however.  We'll need to implement a server:
```{haskell}
userServer :: Server UserAPI
userServer = getUsers
        :<|> getUserByEmail
        :<|> postUser
```
Make sure that the server is in the same order as the API, otherwise the compiler will yell at you.

And if you give an API a Server, it will want some functions.  So let's set up some basic functions for the server as well:
```{haskell}
getUsers :: AppM [User]
getUsers = return users

getUserByEmail :: Email -> AppM (Maybe User)
getUserByEmail email = return $ listToMaybe $ filter ((== email) . userEmail) users

postUser :: User -> AppM [User]
postUser user = return $ users ++ [user]
```
As you'll notice, we are using `AppM` in our return value.  This was defined in App.hs as `EitherT ServantErr IO`.  If you wanted to, you could type that in directly, and end up with type signatures such as `User -> EitherT ServantErr IO [User]`.  But, a) that is more of a pain to read and type, and b) we'll be changing that up in a future lesson, so abstracting it out to `AppM` now will save time.

## Step 4: Set up BlogPost API

Repeat the above steps to set up a blog post.  Create the file "API/BlogPost.hs".  For a datatype, we'll use:
```{haskell}
data BlogPost = BlogPost
              { bpId         :: BlogPostID
              , bpTitle      :: String
              , bpBody       :: String
              , bpUsersEmail :: Email
              , bpTimestamp  :: DateTime
              }
```
All of the steps will be the same as for the User API.  For an exercise, see how much you can implement on your own without looking at the BlogPost.hs file in the "src/API/" directory.

## Step 5: Put the APIs together

Now we have a User API and a BlogPost API, with their respective servers. We'll need to put them together for our main application.  Let's go back to Lib.hs, and add to the import list:
```{haskell}
import Api.User
import Api.BlogPost
```

Forming an API out of sub-APIs is no different then what we've already done:
```{haskell}
type API = "users" :> UserAPI
           :<|> "posts" :> BlogPostAPI

api :: Proxy API
api = Proxy

server :: Server API
server = userServer
    :<|> blogPostServer
```
We create the type by gluing together the sub-APIs and their respective endpoints.  Then we make a proxy and set up a server.  The server just refers back to the `userServer` and the `blogPostServer` we've already created.

Finally, we need to create an application to do the actually serving.  This should already be part of the Lib.hs code:
```{haskell}
app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app
```

## Step 6: Edit Cabal File

Ok, so the code is written, time to fire this thing up, right?  Not so fast - you could end up with some nasty, uninformative error messages at this point.

First, make sure that you add the modules we've been working on to the Cabal file.  These will be *App* (for the App.hs file holding cross-module information), *Api.User*, and *Api.BlogPost*, in addition to *Lib* which Stack has started you off with.  Put these in the *exposed-modules* section.

Second, add in all the depencies we've used.  If you forget one, Cabal will let you know.  Check the blog-tutorial.cabal file for the specific ones I've added.

## Step 7: Run

At this point, you should have a working Servant server.  Type `stack build` to build it, and `stack exec blog-tutorial-exe` to run it.

To test it out, try some basic curl commands:
```
curl 127.0.0.1:8080/users
curl 127.0.0.1:8080/posts
curl -d '{"email": "nikolatesla@hotmail.com", "password": "123abc"}' -H "Content-type:Application/JSON" 127.0.0.1:8080/users
```
