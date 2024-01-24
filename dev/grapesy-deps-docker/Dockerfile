FROM haskell:9.2.8
RUN mkdir /var/local/git
RUN git clone https://github.com/well-typed/grapesy.git /var/local/git/grapesy
WORKDIR /var/local/git/grapesy
RUN cabal update -w /opt/ghc/9.2.8/bin/ghc
RUN cabal build -w /opt/ghc/9.2.8/bin/ghc all --only-dependencies
WORKDIR /
RUN rm -rf /var/local/git/grapesy
