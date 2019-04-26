package com.kakao.search.middle.javalang;

import java.util.HashMap;
import java.util.Map;

public class JavaToken {
    private JavaTokenEnum e;
    private String value;

    private static Map<String, JavaTokenEnum> token;

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

    static <T> JavaToken newToken(T t) {
        String str = t.toString();
        if (token.containsKey(str)) {
            JavaTokenEnum e = token.get(str);
            return new JavaToken(e, e.value);
        }

        return new JavaToken(JavaTokenEnum.TOKEN, str);
    }

    public JavaTokenEnum getE() {
        return e;
    }

    public String getValue() {
        return value;
    }
}