FROM mozilla/sbt:8u292_1.5.7
COPY . /files
ENTRYPOINT ["/files/entrypoint.sh"]
