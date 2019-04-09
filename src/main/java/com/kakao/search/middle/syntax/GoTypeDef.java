package com.kakao.search.middle.syntax;

public class GoTypeDef implements GoSomeDef {
    public String typeName;
    public boolean isInterface;

    @Override
    public String getIdenticalName() {
        return typeName;
    }

    @Override
    public String printAsGoStyle() {
        return String.format("type %s", typeName);
    }
}