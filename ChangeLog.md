<a id="x-28CLACK-CORS-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28CLACK-CORS-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.1 (2023-05-28)

* Fix error in value-or-funcall function.

<a id="x-28CLACK-CORS-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2023-05-11)

* Now given header values are always replace original headers returned by the main application.

This change was made to give more control on how headers are returned.
* Another change is that now functions given instead of literal values, should accept two arguments
  instead of one.

First argument is an `env` plist describing the request and second argument is
  `response-headers` plist, returned by the main app. Using both of them, user or the library
  can have more flexible control on how these headers should be produced.

* Also, the library now supports additional header `Access-Control-Allow-Methods`.

<a id="x-28CLACK-CORS-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2023-02-05)

* Initial version.


* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
