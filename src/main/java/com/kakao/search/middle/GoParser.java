package com.kakao.search.middle;

import com.kakao.search.middle.exceptions.TokenNotAcceptedException;
import com.kakao.search.middle.syntax.*;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

public class GoParser {

    static class GoToken {
        GoTokenEnum e;
        String value;
        int charAt;
        int lineAt;

        GoToken(GoTokenEnum e, String value, int charAt, int lineAt) {
            this.e = e;
            this.value = value.trim();
            this.charAt = charAt;
            this.lineAt = lineAt;
        }

        GoToken(GoTokenEnum e, int charAt, int lineAt) {
            this.e = e;
            this.value = e.value;
            this.charAt = charAt;
            this.lineAt = lineAt;
        }

        static GoToken create(String str, int charAt, int lineAt) {
            str = str.trim();
            if (str.equals("func")) {
                return new GoToken(GoTokenEnum.FUNC, charAt, lineAt);
            } else if (str.equals("type")) {
                return new GoToken(GoTokenEnum.TYPE, charAt, lineAt);
            }

            return new GoToken(GoTokenEnum.STRING, str, charAt, lineAt);
        }
    }


    enum GoTokenEnum {
        TYPE("type"),
        FUNC("func"),
        LBRACKET("("),
        RBRACKET(")"),
        COMMA(","),
        STRING("");

        GoTokenEnum(String v) {
            this.value = v;
        }

        String value;
    }

    private static GoToken[] tokenize(String def) {
        ArrayList<GoToken> ret = new ArrayList<>();
        StringBuffer sb = new StringBuffer();
        int charNum = 0;
        int lineNum = 0;
        for (int i = 0; i < def.length(); i++) {
            char ch = def.charAt(i);
            switch (ch) {
                case '(': {
                    if (sb.length() > 0) {
                        ret.add(GoToken.create(sb.toString(), charNum, lineNum));
                        sb = new StringBuffer();
                    }
                    ret.add(new GoToken(GoTokenEnum.LBRACKET, charNum, lineNum));
                    break;
                }
                case ')': {
                    if (sb.length() > 0) {
                        ret.add(GoToken.create(sb.toString(), charNum, lineNum));
                        sb = new StringBuffer();
                    }
                    ret.add(new GoToken(GoTokenEnum.RBRACKET, charNum, lineNum));
                    break;
                }
                case ',': {
                    if (sb.length() > 0) {
                        ret.add(GoToken.create(sb.toString(), charNum, lineNum));
                        sb = new StringBuffer();
                    }
                    ret.add(new GoToken(GoTokenEnum.COMMA, charNum, lineNum));
                    break;
                }
                case '\n':
                    lineNum += 1;
                    charNum = 0;
                case ' ': {
                    if (sb.length() > 0) {
                        ret.add(GoToken.create(sb.toString(), charNum, lineNum));
                        sb = new StringBuffer();
                    }
                    break;
                }
                default:
                    sb.append(ch);
                    break;
            }
            charNum++;
        }

        if (sb.length() > 0) {
            ret.add(GoToken.create(sb.toString(), charNum, lineNum));
        }

        GoToken[] r = new GoToken[ret.size()];
        ret.toArray(r);
        return r;
    }

    private static void assertToken(GoToken token, GoTokenEnum should) throws TokenNotAcceptedException {
        if (token.e != should) {
            throw new TokenNotAcceptedException(String.format("line: %d, char: %d token not accepted", token.lineAt, token.charAt));
        }
    }

    private static void assertTokenNot(GoToken token, GoTokenEnum shouldNot) throws TokenNotAcceptedException {
        if (token.e == shouldNot) {
            throw new TokenNotAcceptedException(String.format("line: %d, char: %d token not accepted", token.lineAt, token.charAt));
        }
    }

    private static void assertTokenOr(GoToken token, GoTokenEnum[] shoulds) throws TokenNotAcceptedException {
        boolean ret = false;
        for (GoTokenEnum en : shoulds) {
            ret = ret || token.e == en;
        }

        if (!ret) {
            throw new TokenNotAcceptedException(String.format("line: %d, char: %d token not accepted", token.lineAt, token.charAt));
        }
    }

    public static void debugToken(GoToken[] tokens) {
        int num = 0;
        for (GoToken e : tokens) {
            System.out.print(num++ + ": " + e.e);
            if (e.e == GoTokenEnum.STRING) {
                System.out.print("(" + e.value + ")");
            }
            System.out.println();
        }
    }

    public static GoSomeDef parse(String def) throws TokenNotAcceptedException {
        GoToken[] tokens = tokenize(def);
//        debugToken(tokens);

        // should start with "func" or "type"

        assertTokenOr(tokens[0], new GoTokenEnum[]{GoTokenEnum.FUNC, GoTokenEnum.TYPE});
        if (tokens[0].e == GoTokenEnum.FUNC) { // if it starts with "func",
            GoFunctionDef fd = new GoFunctionDef();
            // then next token should be "(" for receiver, or string for function name
            assertTokenOr(tokens[1], new GoTokenEnum[]{GoTokenEnum.LBRACKET, GoTokenEnum.STRING});
            int fnStart = 1;
            if (tokens[1].e == GoTokenEnum.LBRACKET) { // if it starts with "(",
                fnStart = 5;
                assertToken(tokens[2], GoTokenEnum.STRING); // it should start with receiver name
                assertToken(tokens[3], GoTokenEnum.STRING); // and the type name should be followed
                assertToken(tokens[4], GoTokenEnum.RBRACKET); // also, ")" required to enclose the receiver type definition.

                fd.receiverName = tokens[2].value;
                fd.receiverType = tokens[3].value;
            }

            assertToken(tokens[fnStart], GoTokenEnum.STRING);
            fd.funcName = tokens[fnStart].value;

            assertToken(tokens[fnStart + 1], GoTokenEnum.LBRACKET);
            int idx = fnStart + 2;
            Queue<GoToken> pendingVar = new LinkedList<>();
            while (idx < tokens.length && tokens[idx].e != GoTokenEnum.RBRACKET) {
                assertToken(tokens[idx], GoTokenEnum.STRING);
                int untilIdx = idx + 1;
                int pars = 0;
                // Comma(',') 혹은 Rbracket(')')을 기준으로 argument list 생성
                while (untilIdx < tokens.length && !(pars == 0 && tokens[untilIdx].e == GoTokenEnum.COMMA)
                        && !(pars == 0 && tokens[untilIdx].e == GoTokenEnum.RBRACKET)) {
                    if (tokens[untilIdx].e == GoTokenEnum.LBRACKET) {
                        pars++;
                    } else if (tokens[untilIdx].e == GoTokenEnum.RBRACKET) {
                        pars--;
                    }

                    untilIdx++;
                }

                // in case of multiple parameter definition over 1 type, like
                // func x(a, b, c int) int ...
                // definition should be in incomplete state
                pendingVar.add(tokens[idx]);

                if (untilIdx - idx > 1) {
                    int pendingVarCount = pendingVar.size();
                    ArrayList<GoFuncArg> args = new ArrayList<>(pendingVarCount);
                    ArrayList<String> tnList = new ArrayList<>();

                    for (int i = idx + 1; i < untilIdx; i++) {
                        if (tokens[i].e == GoTokenEnum.LBRACKET) {
                            tnList.add(tokens[i].value + tokens[i + 1].value);
                            i += 1;
                        } else if (i + 1 < untilIdx && tokens[i + 1].e == GoTokenEnum.RBRACKET) {
                            tnList.add(tokens[i].value + tokens[i + 1].value);
                        } else {
                            tnList.add(tokens[i].value);
                        }
                    }
                    String typeName = String.join(" ", tnList);

                    while (!pendingVar.isEmpty()) {
                        GoToken tk = pendingVar.remove();
                        GoFuncArg item = new GoFuncArg();
                        item.typeName = typeName;
                        item.name = tk.value;
                        args.add(item);
                    }

                    fd.args.addAll(args);
                }

                idx = untilIdx;


                if (tokens[untilIdx].e == GoTokenEnum.COMMA) {
                    idx += 1;
                }
            }

            assertToken(tokens[idx], GoTokenEnum.RBRACKET);
            idx++;

            if (idx < tokens.length) { // return type
                StringBuilder sb = new StringBuilder();
                for (int i = idx; i < tokens.length; i++) {
                    switch (tokens[i].e) {
                        case LBRACKET: // => doesn't require next space
                            sb.append("(");
                            break;
                        case RBRACKET: // => require next space
                            sb.append(") ");
                            break;
                        case COMMA: //= require next space
                            sb.append(", ");
                            break;
                        default:
                            sb.append(tokens[i].value);
                            if (i + 1 < tokens.length && (tokens[i + 1].e != GoTokenEnum.COMMA && tokens[i + 1].e != GoTokenEnum.RBRACKET)) {
                                sb.append(" ");
                            }
                    }

                    fd.returnType = sb.toString();
                }
            }
//            if (fd.funcName.equals("DefineError")) throw new TokenNotAcceptedException("debug");

            return fd;
        }

        // type Something struct...
        assertToken(tokens[0], GoTokenEnum.TYPE);
        GoTypeDef ret = new GoTypeDef();
        assertToken(tokens[1], GoTokenEnum.STRING);
        ret.typeName = tokens[1].value;
        if (tokens[2].value.startsWith("interface")) {
            ret.isInterface = true;
        }

        return ret;
    }

    public static int findDefStop(String code) {
        int start = 0;
        while (start < code.length() && code.charAt(start++) != '\n');

        final String avoidWord = "interface";

        if (code.substring(start).startsWith("type")) {
            int r = start + 5;
            while (code.charAt(r) != ' ' && code.charAt(r) != '\n') r++;
            return r;
        } else if (code.substring(start).startsWith("func")) {
            for (int i = start + 5; i < code.length(); i++) {
                if (code.charAt(i) == '{') {
                    String maybeAvoidWord = code.substring(i - avoidWord.length(), i);
                    if (!maybeAvoidWord.equals(avoidWord)) {
                        return i;
                    }
                }
            }
        }
        return -1;
    }
}
