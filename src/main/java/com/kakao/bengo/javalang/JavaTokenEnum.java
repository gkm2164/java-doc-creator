package com.kakao.bengo.javalang;

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
    SUPER("super", true),
    IMPLEMENTS("implements", true),
    DEFAULT("default", true),
    THROWS("throws", true),
    TOKEN("", true),
    STRING("", true),
    CHAR("", true),
    ANNOTATION("@"),
    ANNOTATION_INTERFACE("@interface"),
    LEFT_PARENTHESIS("("),
    RIGHT_PARENTHESIS(")"),
    LBRACKET("["),
    RBRACKET("]"),
    LBRACE("{"),
    RBRACE("}"),
    LT("<"),
    LTE("<="),
    GT(">"),
    GTE(">="),
    DOT("."),
    ETC_ARRAY("..."),
    SUBSTITUTE("="),
    EQUAL("=="),
    PLUS("+"),
    PLUS_ACC("+="),
    INC("++"),
    MINUS("-"),
    MINUS_ACC("-="),
    DEC("--"),
    MULTIPLY("*"),
    MULTIPLY_ACC("*="),
    SEMICOLON(";"),
    DIVIDE("/"),
    DIVIDE_ACC("/="),
    COMMENT_BLOCK("/*", "*/", false),
    COMMENT("//", "\n", false),
    COMMENT_MACRO_EXPLAIN("//!", "\n"),
    COMMENT_MACRO_CODE("//=", "\n"),
    COMMENT_MACRO_NAME("//name", "\n"),
    DOUBLE_QUOTE("\"", "\"", STRING),
    QUOTE("'", "'", CHAR),
    COMMA(","),
    COLON(":"),
    NAMESPACE("::");


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

    public final String value;
    final boolean isKeyword;
    boolean takeUntil = false;
    String until;
    JavaTokenEnum saveTo;
    public boolean allowEscape = true;
}