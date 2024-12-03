FROM eclipse-temurin:23-jdk-alpine

RUN apk add curl

COPY . /build

WORKDIR /build

RUN ./mill aoc.assembly