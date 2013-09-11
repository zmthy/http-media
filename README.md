This library is intended to be a comprehensive solution to correctly using
quality parameters in HTTP headers. It addresses parsing of the Accept header,
and includes general data types for matching against the other accept headers as
well. It encodes MIME parameters into a 'MediaType' data, and allows the
matching of the final header value by comparing quality values from both the
client and the server.

The most common use of it is expected to resemble this little example:

```haskell
maybe send406Error sendResource $ parseAccept header >>= mapAccept
    [ ("text/html",        asHtml)
    , ("application/json", asJson)
    ]
```

