FROM haskell:7.8

RUN cabal update

ADD ./server/hairfie-stats.cabal /opt/server/hairfie-stats.cabal
RUN cd /opt/server && cabal install --only-dependencies -j4

ADD ./server /opt/server
RUN cd /opt/server && cabal install

ENV PATH /root/.cabal/bin:$PATH

ENV PORT 80
#ENV MONGO_HOST
#ENV MONGO_USER
#ENV MONGO_PASS
#ENV MONGO_DB

EXPOSE 80

WORKDIR /opt/server
CMD ["hairfie-stats"]
