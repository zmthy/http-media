Changelog
=========

  Exposed `qualityData` accessor.

  Added `isAcceptable` to allow filtering out unacceptable items.

- [Version 0.8.0.0](https://github.com/zmthy/http-media/releases/tag/v0.8.0.0)

  Removed official support for GHC 7.8.

  A separate `QualityOrder` type can now be extracted from a `Quality`
  value for performing comparisons without the attached data value.

  The most specific match will now be used to assign a quality value to
  each server option, ensuring that less specific matches cannot
  override lower quality values on more specific matches. In particular,
  if a type is considered unacceptable by the client, then a separate
  match with a non-zero quality value cannot make it acceptable.

  Numeric characters are now permitted in the tail of a Language value.

  Added support for Accept-Encoding and Content-Encoding.

  Added support for Accept-Charset and Content-Charset.

  The bounds for QuickCheck have been updated to include the latest
  version.

- [Version 0.7.1.3](https://github.com/zmthy/http-media/releases/tag/v0.7.1.3)

  Package bounds have been updated for GHC 8.6.

- [Version 0.7.1.2](https://github.com/zmthy/http-media/releases/tag/v0.7.1.2)

  The bounds for QuickCheck have been updated to include the latest
  version.

- [Version 0.7.1.1](https://github.com/zmthy/http-media/releases/tag/v0.7.1.1)

  The bounds for base have been updated to include support for GHC 8.2.

- [Version 0.7.1](https://github.com/zmthy/http-media/releases/tag/v0.7.1)

  Travis now tests against a range of Stackage LTS environments, instead
  of using multi-ghc.

  Support for base-4.6 has now been correctly removed in the Cabal file.

- [Version 0.7.0](https://github.com/zmthy/http-media/releases/tag/v0.7.0)

  The Travis configuration has dropped support for GHC 7.6 and added
  support for 8.0.

  More direct constructors for quality values are now available, to
  avoid having to deal with `Maybe` results when you are certain parsing
  a quality string will not fail.

  The bounds for QuickCheck have been updated to include the latest
  version.

- [Version 0.6.4](https://github.com/zmthy/http-media/releases/tag/v0.6.4)

  The bounds for QuickCheck have been updated to include the latest
  version.

- [Version 0.6.3](https://github.com/zmthy/http-media/releases/tag/v0.6.3)

  Parse failures more regularly return a `Maybe` value instead of
  raising an exception.

  The `(//)` smart constructor now accepts wildcard arguments, but only
  in the correct order.

  Most tests will now emit a counter example if their relevant
  properties are violated.  Some tests which were not correctly covering
  their properties have been fixed.

  The `-Werror` flag has been removed from the test suite.

- [Version 0.6.2](https://github.com/zmthy/http-media/releases/tag/v0.6.2)

  The test suite now uses the test-framework library instead of
  cabal-test-quickcheck, and the package no longer depends on Cabal.

- [Version 0.6.1](https://github.com/zmthy/http-media/releases/tag/v0.6.1)

  The type errors and build warnings caused by the BBP have been fixed
  for GHC 7.10.

- [Version 0.6.0](https://github.com/zmthy/http-media/releases/tag/v0.6.0)

  All of the publicly exposed data types now derive an `Ord` instance.
