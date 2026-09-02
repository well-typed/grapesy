# Revision history for grpc-spec

## 1.1.0 -- 2026-09-02

* Trivial `BuildMetadata` and `ParseMetadata` instances for `[CustomMetadata]`
* New field `responseTrailerNames` of `ResponseHeaders`, with the infrastructure
 around parsing and building response headers changed accordingly
* `SupportsServerRpc` no longer depends on `StaticMetadata`

## 1.0.0 -- 2025-01-22

* First released version.
