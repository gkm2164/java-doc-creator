package com.kakao.search.middle.javalang.defs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Code {
    String packageName;
    Map<String, String> imports;
    List<JavaDef> docClasses;

    Code() {
        this.imports = new HashMap<>();
        this.docClasses = new ArrayList<>();
    }

    public void setPackageName(String pkgName) {
        this.packageName = pkgName;
    }

    public void addImport(JavaImport imp) {
        imports.put(imp.key, imp.fullName);
    }

    public void addDocClasses(JavaDef def) {
        docClasses.add(def);
    }
}
