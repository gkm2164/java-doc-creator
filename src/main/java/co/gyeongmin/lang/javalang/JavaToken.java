package co.gyeongmin.lang.javalang;

import java.util.HashMap;
import java.util.Map;

public class JavaToken {
    private final JavaTokenEnum e;
    private final String value;

    private static final Map<String, JavaTokenEnum> token;

    static {
        token = new HashMap<>();
        for (JavaTokenEnum r : JavaTokenEnum.values()) {
            if (r != JavaTokenEnum.STRING && r != JavaTokenEnum.TOKEN) {
                token.put(r.value, r);
            }
        }
    }

    public JavaToken(JavaTokenEnum e, String value) {
        this.e = e;
        this.value = value;
    }

    public static <T> JavaToken newToken(T t) {
        String str = t.toString();
        if (token.containsKey(str)) {
            JavaTokenEnum e = token.get(str);
            return new JavaToken(e, e.value);
        }

        if (str.matches("^[-+]?\\d+(\\.\\d+)?$")) {
            return new JavaToken(JavaTokenEnum.NUMBER, str);
        }
        return new JavaToken(JavaTokenEnum.TOKEN, str);
    }

    public JavaTokenEnum getE() {
        return this.e;
    }

    public String getValue() {
        return this.value;
    }
}