# Yesod-Raml-Mock: 

Yesod-Raml-Mock makes mock-handler from example-tag of [RAML](http://raml.org/spec.html) File.

## Usage

```parseRamlMockFile``` makes mock-handler from RAML file.
Example is below.


At first, write example-tag with media type.

```
title: Hoge API
baseUri: 'https://hoge/api/{version}'
version: v1
protocols: [ HTTPS ]
/user:
  /{String}:
    handler: HogeR
    get:
      description: hoger
      responses:
        200:
          body:
            application/json:
              example: '{ "test": "123" }'
```

Second, put ```parseRamlMockFile``` with RAML file.

```
$(parseRamlMockFile  "config/routes.raml")
```

This ```parseRamlMockFile``` makes following handler.

```
getHogeR :: Yesod app => String -> Handler app IO TypedContent
getHogeR _ = return $ TypedContent "application/json" "{ \"test\": \"123\" }"
```
