package com.kakao.bengo.godoc.golang.syntax;

import java.util.ArrayList;
import java.util.List;

public class GoFunctionDef implements GoSomeDef {
    public String receiverName; // 리시버 명
    public String receiverType; //리시버 타입 명
    public String funcName; // 함수 명
    public String returnType;
    public final List<GoFuncArg> args;

    public GoFunctionDef() {
        receiverName = "";
        receiverType = "";
        funcName = "";
        returnType = "";
        args = new ArrayList<>();
    }
}