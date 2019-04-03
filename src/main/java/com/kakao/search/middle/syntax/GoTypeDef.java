package com.kakao.search.middle.syntax;

public class GoTypeDef implements GoSomeDef {
    public String typeName;

    @Override
    public String getIdenticalName() {
        return typeName;
    }

    @Override
    public String printAsGoStyle() {
        return String.format("type %s", typeName);
    }
}