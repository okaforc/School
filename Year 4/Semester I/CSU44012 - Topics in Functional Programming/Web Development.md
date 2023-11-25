# Scotty
[[04.03 - Scotty and Blaze, DSLS for the web.pdf]]
Scotty is a minimalistic web framework for Haskell.

Take the following example (also at [[Function Examples#^scotty-example|scotty-example]])
```haskell
{-# LANGUAGE OverloadedStrings #-} 
import Web.Scotty 
main = scotty 3000 $ do 
	get "/" $ do 
		html "Hello World!"
```

```haskell
{-# LANGUAGE OverloadedStrings #-} 
```
turns on a Haskell language extension. This one allows the compiler to use the correct type of double-quotes for certain strings. 


#### Line 1
Normally a literal like `"some text"` is of type `String` (which is an alias for `[Char]`). This can be inefficient, so there are some Haskell libraries that implement more efficient String representations, like Bytestring and Text. The OverloadedStrings libraries allows String literals to be polymorphic, so they become 
```haskell
IsString a => a
```
The corresponding class is:
```haskell
class IsString a where
	fromString :: String -> a
```

#### Lines 3-5
```haskell
main = scotty 3000 $ do 
	get "/" $ do 
		html "Hello World!"
```
The main body. Scotty gives us two monads for different kinds of things:
- ScottyM - actions in this monad configure the entire app (e.g., the type of URLs it understands)
- ActionM - actions here process individual requests
The entire app then gets run in the IO monad.

### Routes
Routes can be added using actions.
```haskell
addroute :: StdMethod -> RoutePattern -> ActionM () -> ScottyM ()
-- It's usually easier to implement one of the below helper functions from the library
get :: RoutePattern -> ActionM () -> ScottyM () 
post :: RoutePattern -> ActionM () -> ScottyM () 
matchAny :: RoutePattern -> ActionM () -> ScottyM () 
notFound :: ActionM () -> ScottyM ()
```

Route patterns can be created from strings:
```haskell
capture :: String -> RoutePattern 
regex :: String -> RoutePattern
literal :: String -> RoutePattern
```
or by direct manipulation of the HTTP request:
```haskell
function :: (Request -> Maybe [Param]) -> RoutePattern
```
Note that we write 
```haskell
get "/" $ do ...
```
instead of
```haskell
get (capture "/") $ do ...
```
because we're using the OverloadedStrings library (mentioned above).

We can capture multiple different routes:
```haskell
main = scotty 3000 $ do 
	get "/" $ do 
		html "Hello World!"
	get "/greet" $ do 
		html "Hello Iris! It's Marcy :3"
```

Or have wildcards that become parameters:
```haskell
main = scotty 3000 $ do 
	get "/greet/:name" $ do 
		name <- param "name"
		html $ mconcat ["Hello there, ", name]
```
where
```haskell
param :: Parseable => Text -> ActionM a
```
The Parseable class can read values in as variable types, so we might read a parameter as an `int` rather than a `String`.

### Responses
Once a route has been matched, we provide one or more actions to modify it. In our example, we replace the entire response body (lines 3-5). This is done with the following function:
```haskell
html :: Text -> ActionM ()
```

There are also other ways we could set the response body:
```haskell
text :: Text -> ActionM () 
file :: FilePath -> ActionM () 
json :: ToJSON a => a -> ActionM () 
raw :: ByteString -> ActionM ()
```

We can also configure the header:
```haskell
status :: Status -> ActionM () 
addHeader :: Text -> Text -> ActionM () 
setHeader :: Text -> Text -> ActionM () 
redirect :: Test -> ActionM ()
```

`redirect` is a bit of a special case since any actions after the redirect won't be run.

# Blaze
In the end, however, Scotty doesn't return proper HTML, just the text we supplied with headers claiming to be HTML. To generate actual HTML to display on a webpage, we'll need to use Blaze.

The Blaze library is used for generating HTML (and SVG).

We can import the Blaze modules *qualified*, which differentiates modules based on their name, as some Blaze libraries and functions may overlap with Scotty ones. Because they may be long, we can refer to them `as` another name, similar to Python:

```haskell
import qualified Text.Blaze.Html5 as H 
import qualified Text.Blaze.Html5.Attributes as A 
import qualified Text.Blaze.Html.Renderer.Text as R
```

We can write a new function in our program. It will be passed a string (a `Text` to be specific), which contains something we want to wrap in normal HTML and return as a webpage.

```haskell
response :: Text -> Text
response n = do R.renderHtml $ do
	H.h1 ("Hello" >> H.toHtml n)
```
where:
```haskell
renderHTML :: Html -> Text 
h1 :: Html -> Html 
toHtml :: ToMarkup a => a -> Html
```
`ToMarkup` is a class with instances for regular types (int, bool, etc.).

Using Blaze for more complex markup is fairly easy:
```haskell
longresponse :: Text -> Text 
longresponse n = do R.renderHtml $ do 
	H.head $ H.title "Welcome page" H.body $ do 
		H.h1 "Welcome!" 
		H.p ("Welcome to my Scotty app" >> H.toHtml n)
```

Finally, we can add attributes using the `!` operator:
```haskell
myImage :: H.Html
myImage = H.img H.! H.src "catPicture.jpg" H.! H.alt "Awwww."
```

(see [[Function Examples#^blaze-example|blaze-example]])

