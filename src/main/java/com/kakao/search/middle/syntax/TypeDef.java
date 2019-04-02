package com.kakao.search.middle.syntax;

public class TypeDef extends SomeDef {
    public String typeName;

    @Override
    public String printAsGoStyle() {
        return String.format("type %s", typeName);
    }
}