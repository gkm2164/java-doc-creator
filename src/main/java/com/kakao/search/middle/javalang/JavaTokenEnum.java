package com.kakao.search.middle.javalang;

public enum JavaTokenEnum {
    CLASS("class", true),
    INTERFACE("interface", true),
    ABSTRACT("abstract", true),
    ENUM("enum", true),
    PUBLIC("public", true),
    PROTECTED("protected", true),
    PRIVATE("private", true),
    STATIC("static", true),
    PACKAGE("package", true),
    IMPORT("import", true),
    FINAL("final", true),
    EXTENDS("extends", true),
    IMPLEMENTS("implements", true),
    DEFAULT("default", true),
    TOKEN("", true),
    STRING("", true),
    CHAR("", true),
    ANNOTATION("@"),
    ANNOTATION_INTERFACE("@interface"),
    LPAR("("),
    RPAR(")"),
    LBRACKET("["),
    RBRACKET("]"),
    LBRACE("{"),
    RBRACE("}"),
    LT("<"),
    LTE("<="),
    GT(">"),
    GTE(">="),
    DOT("."),
    SUBSTITUTE("="),
    EQUAL("=="),
    PLUS("+"),
    PLUSACC("+="),
    INC("++"),
    MINUS("-"),
    MINUSACC("-="),
    DEC("--"),
    MULTIPLY("*"),
    MULTIPLYACC("*="),
    SEMICOLON(";"),
    DIVIDE("/"),
    DIVIDEACC("/="),
    COMMENT_BLOCK("/*", "*/", false),
    COMMENT("//", "\n", false),
    COMMENT_MACRO("//=", "\n"),
    DOUBLE_QUOTE("\"", "\"", STRING),
    QUOTE("'", "'", CHAR),
    COMMA(","),
    COLON(":");


    JavaTokenEnum(String value) {
        this.value = value;
        this.isKeyword = false;
    }

    JavaTokenEnum(String value, boolean isKeyword) {
        this.value = value;
        this.isKeyword = isKeyword;
    }

    JavaTokenEnum(String value, String until) {
        this.value = value;
        this.isKeyword = false;
        this.takeUntil = true;
        this.until = until;
        this.saveTo = this;
    }



    JavaTokenEnum(String value, String until, boolean allowEscape) {
        this.value = value;
        this.isKeyword = false;
        this.takeUntil = true;
        this.until = until;
        this.saveTo = this;
        this.allowEscape = allowEscape;
    }

    JavaTokenEnum(String value, String until, JavaTokenEnum saveTo) {
        this.value = value;
        this.isKeyword = false;
        this.takeUntil = true;
        this.until = until;
        this.saveTo = saveTo;
    }

    public String value;
    boolean isKeyword;
    boolean takeUntil = false;
    String until;
    JavaTokenEnum saveTo;
    public boolean allowEscape = true;
}