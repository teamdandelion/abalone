FROM golang:1.4
MAINTAINER Brian Tiger Chow <btc@perfmode.com>

ADD . /go/src/github.com/briantigerchow/gamecenter
RUN cd /go/src/github.com/briantigerchow/gamecenter/gamecenter  && go install
RUN rm -rf /go/src

EXPOSE 8080

ENTRYPOINT ["gamecenter"]
