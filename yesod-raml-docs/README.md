# Yesod-Raml-Docs: 

Yesod-Raml-Docs makes a html-document from [RAML](http://raml.org/spec.html) File.

The template of html document uses Hamlet(Shakespeare).

Though the template is Hamlet, original template is numjucks of [raml2html](https://github.com/kevinrenskers/raml2html).
To convert numjucks to Hamlet, I used [html2hamlet](https://hackage.haskell.org/package/html2hamlet).

## Usage

```parseRamlDocsFile``` makes Html data type from RAML file.
Example is below.

```
getHelpR ::  Handler Html
getHelpR = return  $(parseRamlDocsFile "config/routes.raml")
```
