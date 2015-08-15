# Yesod-Raml: 

[![Hackage version](https://img.shields.io/hackage/v/yesod-raml.svg?style=flat)](https://hackage.haskell.org/package/yesod-raml)  [![Build Status](https://travis-ci.org/junjihashimoto/yesod-raml.png?branch=master)](https://travis-ci.org/junjihashimoto/yesod-raml)

Yesod-Raml makes routes definition from [RAML](http://raml.org/spec.html) File.

raml style routes definition is inspired by [sbt-play-raml](https://github.com/scalableminds/sbt-play-raml).

## Getting started

Install this from Hackage.

    cabal update && cabal install yesod-raml

## Usage

Use parseRamlRoutes or parseRamlRoutesFile in instead of parseRoutes or parseRoutesFile.

Write RAML with ```handler```-tag for Yesod Handler.

```handler```-tag is not a tag of RAML spec but original one.

You can use ```description```-tag with ```handler: <<handler-name>>``` instead of ```handler```-tag.

Bracket variable(PathPiece) like ```{hogehoge}``` is capitalized.
The variable becomes ```#Hogehoge```.
because variable(PathPiece) of yesod-routes is data-type like String or Text.

Examples are below.

```
type Userid = String

mkYesod "App" [parseRamlRoutes|
#%RAML 0.8
title: Hoge API
baseUri: 'https://hoge/api/{version}'
version: v1
protocols: [ HTTPS ]
/user:
  /{userid}:
# handler tag is used.
    handler: HogeR
    get:
      description: Get user list
    /del:
# handler is written in description-tag
      description: |
	    handler: Hoge2R
      post:
        description: Delete user
|]
```

This is the same as following codes.

As you can see, ```{userid}``` becomes ```#Userid```.

```
type Userid = String

mkYesod "App" [parseRoutes|
/api/v1/user/#Userid HogeR GET
/api/v1/user/#Userid/del Hoge2R POST
|]
```
