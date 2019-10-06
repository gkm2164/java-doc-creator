# Java Document Creator

## Overview
- Java parser written in Scala language
- Export HTML document which describes your classes, enum classes, interfaces, annotation interfaces, methods, member fields.
- LL(1) parser
- Tokenizer written in Java

![example](img/screenshot.png)

## How to use?
1. Build this project with sbt command 
- ```$ sbt compile```

1. Run with SBT command
- ```$ sbt run -i [base directory of your maven/sbt/gradle project] -o [output file name, html format] -v```
- Options
  - -i --input: set project directory
  - -o --output: set output file name
  - -v --verbose: debug option to see whole parsing mechanism

## Used stack
- Scala 2.12.8, Java 1.8, Cats library for Monad
- HTML5, Bootstrap, JQuery

## Etc.
- Sorry for not pure functional for Tokenizer... Implementing a Aho-corasick algorithm is so hard in FP. But the Parser
  is purely functional.(State + Writer + Either monads)
