# Docker image with `grapesy`'s dependencies prebuilt

## Rebuilding

The image will need to rebuilt when `grapesy`'s dependencies change (and
especially when it requires newer versions of packages that aren't yet known in
the cabal package database in the image).

Steps:

1.  Rebuild the image

    ```bash
    grapesy-deps-docker$ docker build . -t edsko/grapesy-deps:latest
    ```

2. Delete the existing image on `hub.docker.com`

3. Upload the new image

   ```bash
   $ docker push edsko/grapesy-deps:latest
   ```

## Testing

To test the image:

```bash
$ docker run --rm -it edsko/grapesy-deps bash
```

then inside the image:

```bash
$ git clone https://github.com/well-typed/grapesy
grapesy$ cabal build all
```

This should _only_ build `grapesy` (all dependencies should already have been
built).
