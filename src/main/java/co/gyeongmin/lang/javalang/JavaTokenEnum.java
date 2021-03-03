package co.gyeongmin.lang.javalang;

public enum JavaTokenEnum {
    ABSTRACT("abstract", true),
    AND("&&"),
    ANNOTATION("@"),
    ANNOTATION_INTERFACE("@interface"),
    BIT_OR("|"),
    BIT_OR_ACC("|="),
    BIT_AND("&"),
    BIT_AND_ACC("&="),
    BIT_XOR("^"),
    BIT_XOR_ACC("^="),
    BREAK("break", true),
    CASE("case", true),
    CATCH("catch", true),
    CHAR("", true),
    CLASS("class", true),
    CONTINUE("continue", true),
    COLON(":"),
    COMMENT_BLOCK("/*", "*/", false),
    COMMENT("//", "\n", false),
    COMMENT_MACRO_EXPLAIN("//!", "\n"),
    COMMENT_MACRO_CODE("//=", "\n"),
    COMMENT_MACRO_NAME("//name", "\n"),
    COMMA(","),
    DEC("--"),
    DEFAULT("default", true),
    DIVIDE("/"),
    DIVIDE_ACC("/="),
    DO("do", true),
    DOT("."),
    ETC_ARRAY("..."),
    EQUAL("=="),
    ELSE("else", true),
    ENUM("enum", true),
    EXTENDS("extends", true),
    EXCLAMATION_MARK("!"),
    FALSE("false", true),
    FINAL("final", true),
    FINALLY("finally", true),
    FOR("for", true),
    GT(">"),
    GTE(">="),
    IF("if", true),
    IMPORT("import", true),
    IMPLEMENTS("implements", true),
    INC("++"),
    INSTANCEOF("instanceof", true),
    INTERFACE("interface", true),
    LAMBDA_BODY_START("->"),
    LBRACKET("["),
    LBRACE("{"),
    LEFT_PARENTHESIS("("),
    LEFT_SHIFT("<<"),
    LT("<"),
    LTE("<="),
    MINUS("-"),
    MINUS_ACC("-="),
    MULTIPLY("*"),
    MULTIPLY_ACC("*="),
    MODULAR("%"),
    NAMESPACE("::"),
    NEGATE("~"),
    NEW("new", true),
    NOT_EQUAL("!="),
    NULL("null", true),
    NUMBER("", true),
    OR("||"),
    PACKAGE("package", true),
    PLUS("+"),
    PLUS_ACC("+="),
    PRIMITIVE_BYTE("byte", true),
    PRIMITIVE_SHORT("short", true),
    PRIMITIVE_INT("int", true),
    PRIMITIVE_LONG("long", true),
    PRIMITIVE_CHAR("char", true),
    PRIMITIVE_FLOAT("float", true),
    PRIMITIVE_DOUBLE("double", true),
    PRIMITIVE_BOOLEAN("boolean", true),
    PROTECTED("protected", true),
    PRIVATE("private", true),
    PUBLIC("public", true),
    QUOTE("'", "'", CHAR),
    QUESTION_MARK("?"),
    RBRACKET("]"),
    RBRACE("}"),
    RETURN("return", true),
    RIGHT_PARENTHESIS(")"),
    RIGHT_SHIFT(">>"),
    SEMICOLON(";"),
    STRICTFP("strictfp", true),
    STATIC("static", true),
    SUBSTITUTE("="),
    SUPER("super", true),
    SWITCH("switch", true),
    STRING("", true),
    DOUBLE_QUOTE("\"", "\"", STRING),
    SYNCHRONIZED("synchronized", true),
    TRANSIENT("transient", true),
    THIS("this", true),
    THROWS("throws", true),
    THROW("throw", true),
    TOKEN("", true),
    TRUE("true", true),
    TRY("try", true),
    U_RIGHT_SHIFT(">>>"),
    VOLATILE("volatile", true),
    WHILE("while", true);


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
    public final boolean isKeyword;
    public boolean takeUntil = false;
    public String until;
    public JavaTokenEnum saveTo;
    public boolean allowEscape = true;
}
