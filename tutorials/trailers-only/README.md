# Basics tutorial, taking advantage of the gRPC Trailers-Only case

See `/tutorials/basics` for the more direct `grapesy` translation of the
[official Basics tutorial](https://grpc.io/docs/languages/python/basics/).

In this tutorial we re-implement the server using the low-level API (similar to
what we did in `/tutorials/lowlevel`), but taking advantage of the Trailers-Only
case in `listFeatures`.

We don't re-implement the client (you can run the client from
`tutorials/basics`).

