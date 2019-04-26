package com.kakao.search.middle.javalang.defs;

import java.util.List;

public class JavaType {
    String pkgName;
    String name;
    List<JavaFunction> functions;

    public class JavaFunction {
        String returnType;
        String name;
        List<JavaArgDef> arguments;

        class JavaArgDef {
            String typename;
            String name;

        }
    }

    String getFullName() {
        return pkgName + "." + name;
    }
}
