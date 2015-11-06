# Yesod-Raml-Bin: 

Yesod-Raml-Bin is a utility program for yesod-raml.
The functions are below.

- Verify RAML format
- Convert RAML to HTML
- Convert RAML to Yesod-Route file


## Usage

Verify RAML file. (Curretly just use parser of Data.Yaml.)

```
> yesod-raml-bin verfiy "raml file"
```

Convert RAML to HTML.

```
> yesod-raml-bin tohtml "raml file" "html file"
```

Generate routes file for Yesod.

```
> yesod-raml-bin toroute "raml file" "routes file"
```
