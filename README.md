http-media
==========

[![Hackage](https://img.shields.io/hackage/v/http-media.svg)](https://hackage.haskell.org/package/http-media)
[![Build Status](https://github.com/zmthy/http-media/workflows/Haskell-CI/badge.svg)](https://github.com/zmthy/http-media/actions?query=workflow%3AHaskell-CI)

This library is intended to be a comprehensive solution to parsing and
selecting quality-indexed values in HTTP headers.  It is capable of
parsing both media types and language parameters from the Accept and
Content header families, and can be extended to match against other
accept headers as well.  Selecting the appropriate header value is
achieved by comparing a list of server options against the
quality-indexed values supplied by the client.

In the following example, the Accept header is parsed and then matched
against a list of server options to serve the appropriate media using
`mapAcceptMedia`:

```haskell
send = getHeader >>= maybe send406Error sendResourceWith . mapAcceptMedia
    [ ("text/html",        asHtml)
    , ("application/json", asJson)
    ]
```

Similarly, the Content-Type header can be used to produce a parser for
request bodies based on the given content type with `mapContentMedia`:

```haskell
recv = getContentType >>= maybe send415Error readRequestBodyWith . mapContentMedia
    [ ("application/json", parseJson)
    , ("text/plain",       parseText)
    ]
```

The API is agnostic to your choice of server.
