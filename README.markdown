Stats
=====

Build
-----

    docker build -t hairfie-stats .

Run
---

    docker run -i -t -p 3000:80 -e "MONGO_HOST=x" -e "MONGO_DB=x" -e "MONGO_USER=x" -e "MONGO_PASS=x" hairfie-stats
