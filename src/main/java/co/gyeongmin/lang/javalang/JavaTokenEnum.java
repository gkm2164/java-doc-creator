package co.gyeongmin.lang.javalang;

public enum JavaTokenEnum {
    CLASS("class", true),
    INTERFACE("interface", true),
    ABSTRACT("abstract", true),
    ENUM("enum", true),
    PUBLIC("public", true),
    PROTECTED("protected", true),
    PRIVATE("private", true),
    STRICTFP("strictfp", true),
    STATIC("static", true),
    TRANSIENT("transient", true),
    PACKAGE("package", true),
    IMPORT("import", true),
    FINAL("final", true),
    EXTENDS("extends", true),
    THIS("this", true),
    SUPER("super", true),
    IMPLEMENTS("implements", true),
    DEFAULT("default", true),
    CONTINUE("continue", true),
    DO("do", true),
    SWITCH("switch", true),
    BREAK("break", true),
    THROWS("throws", true),
    RETURN("return", true),
    THROW("throw", true),
    CASE("case", true),
    INSTANCEOF("instanceof", true),
    NEW("new", true),
    IF("if", true),
    ELSE("else", true),
    TRY("try", true),
    CATCH("catch", true),
    FINALLY("finally", true),
    FOR("for", true),
    WHILE("while", true),
    PRIMITIVE_BYTE("byte", true),
    PRIMITIVE_SHORT("short", true),
    PRIMITIVE_INT("int", true),
    PRIMITIVE_LONG("long", true),
    PRIMITIVE_CHAR("char", true),
    PRIMITIVE_FLOAT("float", true),
    PRIMITIVE_DOUBLE("double", true),
    PRIMITIVE_BOOLEAN("boolean", true),
    TOKEN("", true),
    NUMBER("", true),
    STRING("", true),
    CHAR("", true),
    TRUE("true", true),
    FALSE("false", true),
    NULL("null", true),
    SYNCHRONIZED("synchronized", true),
    VOLATILE("volatile", true),
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
    LEFT_SHIFT("<<"),
    RIGHT_SHIFT(">>"),
    U_RIGHT_SHIFT(">>>"),
    DOT("."),
    OR("||"),
    AND("&&"),
    BIT_OR("|"),
    BIT_OR_ACC("|="),
    BIT_AND("&"),
    BIT_AND_ACC("&="),
    BIT_XOR("^"),
    BIT_XOR_ACC("^="),
    QUESTION_MARK("?"),
    EXCLAMATION_MARK("!"),
    NOT_EQUAL("!="),
    LAMBDA_BODY_START("->"),
    ETC_ARRAY("..."),
    SUBSTITUTE("="),
    EQUAL("=="),
    PLUS("+"),
    NEGATE("~"),
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
    MODULAR("%"),
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
    public final boolean isKeyword;
    boolean takeUntil = false;
    String until;
    JavaTokenEnum saveTo;
    public boolean allowEscape = true;
}
