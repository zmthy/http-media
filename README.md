This library is intended to be a comprehensive solution to parsing media
types, including quality parameters, in HTTP headers. It addresses parsing of
the Content-Type and Accept headers, and includes general data types for
matching against the other accept headers as well. It encodes MIME parameters
into a `MediaType` data, and allows the matching of the final value by
comparing quality values from the client.

In the following example, the Accept header is parsed, and then matched against
a list of server options to serve the appropriate media:

```haskell
getHeader >>= maybe send406Error sendResourceWith . mapAcceptMedia
    [ ("text/html",        asHtml)
    , ("application/json", asJson)
    ]
```

Similarly, the Content-Type header can be used to produce a parser for request
bodies based on the given content type:

```haskell
getContentType >>= maybe send415Error readRequestBodyWith . mapContentMedia
    [ ("application/json", parseJson)
    , ("text/plain",       parseText)
    ]
```

The API is agnostic to your choice of server.

