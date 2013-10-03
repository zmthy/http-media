This library is intended to be a comprehensive solution to parsing media
types, including quality parameters, in HTTP headers. It addresses parsing of
the Content-Type and Accept headers, and includes general data types for
matching against the other accept headers as well. It encodes MIME parameters
into a `MediaType` data, and allows the matching of the final value by
comparing quality values from the client.

In the following example, the Accept header is parsed, and then matched
against a list of server options to serve the appropriate media:

```haskell
maybe send406Error sendResource $ parseAccept header >>= mapAccept
    [ ("text/html",        asHtml)
    , ("application/json", asJson)
    ]
```

