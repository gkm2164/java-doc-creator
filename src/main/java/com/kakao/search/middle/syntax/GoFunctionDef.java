package com.kakao.search.middle.syntax;

import java.util.ArrayList;
import java.util.List;

public class GoFunctionDef implements GoSomeDef {
    public String receiverName; // 리시버 명
    public String receiverType; //리시버 타입 명
    public String funcName; // 함수 명
    public String returnType;
    public List<GoFuncArg> args;

    public GoFunctionDef() {
        receiverName = "";
        receiverType = "";
        funcName = "";
        returnType = "";
        args = new ArrayList<>();
    }

    @Override
    public String getIdenticalName() {
        return funcName;
    }

    public String printAsGoStyle() {
        StringBuilder sb = new StringBuilder();
        sb.append("func ");
        if (!receiverName.equals("")) {
            sb.append("(").append(receiverName).append(" ").append(receiverType).append(") ");
        }

        sb.append(funcName).append("(");

        if (!args.isEmpty()) {
            boolean isFirstIter = true;
            for (GoFuncArg arg: args) {
                if (!isFirstIter) {
                    sb.append(", ");
                }
                sb.append(arg.name).append(" ").append(arg.typeName);
                isFirstIter = false;
            }
        }

        sb.append(") ").append(returnType);

        return sb.toString();
    }
}