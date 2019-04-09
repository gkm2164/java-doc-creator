package com.kakao.search.middle;

import com.kakao.search.middle.exceptions.TokenNoMoreToConsumeException;
import com.kakao.search.middle.exceptions.TokenNotAcceptedException;
import com.kakao.search.middle.syntax.GoFuncArg;
import com.kakao.search.middle.syntax.GoFunctionDef;
import com.kakao.search.middle.syntax.GoSomeDef;
import com.kakao.search.middle.syntax.GoTypeDef;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;
import java.util.function.Predicate;
import java.util.logging.Logger;

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

    public static class GoTokenIterator implements Iterator<GoToken> {
        private int idx = 0;
        private GoToken[] tokens = null;

        private Logger log = Logger.getLogger("GoTokenIterator");

        public GoTokenIterator(GoToken[] tokens) {
            this.tokens = tokens;
        }

        @Override
        public boolean hasNext() {
            return idx < tokens.length;
        }

        public GoToken glanceNext() {
            if (idx < tokens.length) {
                return tokens[idx];
            }
            return null;
        }

        @Override
        public GoToken next() {
            GoToken ret = tokens[idx++];
            log.finest(ret.e + ", " + ret.value);
            return ret;
        }

        GoToken assertToken(Predicate<GoToken> validator) throws TokenNotAcceptedException, TokenNoMoreToConsumeException {
            if (!hasNext()) {
                throw new TokenNoMoreToConsumeException("");
            }

            GoToken token = next();
            if (!validator.test(token)) {
                throw new TokenNotAcceptedException(String.format("line: %d, char: %d token not accepted", token.lineAt, token.charAt));
            }

            return token;
        }

        GoToken assertTokenOr(GoTokenEnum[] shoulds) throws TokenNotAcceptedException, TokenNoMoreToConsumeException {
            return assertToken((token) -> {
                for (GoTokenEnum should : shoulds) {
                    if (token.e == should) {
                        return true;
                    }
                }
                return false;
            });
        }
    }

    public static GoSomeDef parse(String def) throws TokenNotAcceptedException, TokenNoMoreToConsumeException {
        GoTokenIterator it = new GoTokenIterator(tokenize(def));
        Logger log = Logger.getLogger("GoParser.parse");

        log.fine(def);
//        debugToken(tokens);

        // should start with "func" or "type"
        GoToken defType = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.FUNC, GoTokenEnum.TYPE});
        if (defType.e == GoTokenEnum.FUNC) { // if it starts with "func",
            GoFunctionDef fd = new GoFunctionDef();
            // then next token should be "(" for receiver, or string for function name
            GoToken mayFuncName = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.LBRACKET, GoTokenEnum.STRING});
            if (mayFuncName.e == GoTokenEnum.LBRACKET) { // if it starts with "(",
                GoToken receiverName = it.assertToken((x) -> x.e == GoTokenEnum.STRING); // it should start with receiver name
                GoToken receiverType = it.assertToken((x) -> x.e == GoTokenEnum.STRING); // and the type name should be followed
                it.assertToken((x) -> x.e == GoTokenEnum.RBRACKET); // also, ")" required to enclose the receiver type definition.

                fd.receiverName = receiverName.value;
                fd.receiverType = receiverType.value;
                mayFuncName = it.next();
            }

            fd.funcName = mayFuncName.value;
            it.assertToken((x) -> x.e == GoTokenEnum.LBRACKET);
            Queue<GoToken> pendingVar = new LinkedList<>();

            while (it.hasNext()) {
                GoToken mayFinish = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.RBRACKET, GoTokenEnum.COMMA, GoTokenEnum.STRING});
                if (mayFinish.e == GoTokenEnum.RBRACKET) {
                    break;
                }
                pendingVar.add(mayFinish);
                GoToken mayComma = it.next();
                if (mayComma.e == GoTokenEnum.COMMA) {
                    pendingVar.add(mayComma);
                    continue;
                }

                log.fine("suspect to be a argument's typename: " + mayComma.value);

                ArrayList<GoToken> typeNameTokens = new ArrayList<>();
                typeNameTokens.add(mayComma);
                int pars = 0;
                // Comma(',') 혹은 Rbracket(')')을 기준으로 argument list 생성
                GoToken argCtx = it.next();
                while (!((pars == 0 && argCtx.e == GoTokenEnum.COMMA) || (pars == 0 && argCtx.e == GoTokenEnum.RBRACKET))) {
                    log.fine("parse arg: " + argCtx.value);
                    switch (argCtx.e) {
                        case LBRACKET:
                            pars++;
                            break;
                        case RBRACKET:
                            pars--;
                            break;
                        default:
                            break;
                    }
                    typeNameTokens.add(argCtx);

                    if (!it.hasNext()) break;
                    argCtx = it.next();
                }

                // in case of multiple parameter definition over 1 type, like
                // func x(a, b, c int) int ...
                // definition should be in incomplete state


                int pendingVarCount = pendingVar.size();
                ArrayList<GoFuncArg> args = new ArrayList<>(pendingVarCount);
                ArrayList<String> tnList = new ArrayList<>();
                int until = typeNameTokens.size();
                for (int i = 0; i < until; i++) {
                    if (typeNameTokens.get(i).e == GoTokenEnum.LBRACKET) {
                        tnList.add(typeNameTokens.get(i).value + typeNameTokens.get(i + 1).value);
                        i += 1;
                    } else if (i + 1 < until && typeNameTokens.get(i + 1).e == GoTokenEnum.RBRACKET) {
                        tnList.add(typeNameTokens.get(i).value + typeNameTokens.get(i + 1).value);
                        i += 1;
                    } else if (i + 1 < until && typeNameTokens.get(i + 1).e == GoTokenEnum.COMMA) {
                        tnList.add(typeNameTokens.get(i).value + typeNameTokens.get(i + 1).value);
                        i += 1;
                    } else {
                        tnList.add(typeNameTokens.get(i).value);
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

                if (argCtx.e == GoTokenEnum.RBRACKET) {
                    break;
                }
            }

            if (it.hasNext()) { // return type
                StringBuilder sb = new StringBuilder();
                while (it.hasNext()) {
                    GoToken token = it.next();
                    switch (token.e) {
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
                            sb.append(token.value);
                            GoToken glance = it.glanceNext();
                            if (it.hasNext() && glance != null) {
                                if (glance.e != GoTokenEnum.COMMA && glance.e != GoTokenEnum.RBRACKET) {
                                    sb.append(" ");
                                }
                            }
                    }

                    fd.returnType = sb.toString();
                }
            }

            return fd;
        }

        // type Something struct...

        GoTypeDef ret = new GoTypeDef();
        GoToken typeName = it.assertToken((x) -> x.e == GoTokenEnum.STRING);
        ret.typeName = typeName.value;
        GoToken aliasType = it.next();
        if (aliasType.value.startsWith("interface")) {
            log.fine("it is interface!: " + ret.typeName);
            ret.isInterface = true;
        }

        return ret;
    }

    public static int findDefStop(String code) {
        int start = 0;
        while (start < code.length() && code.charAt(start++) != '\n') ;

        final String avoidWord = "interface";

        if (code.substring(start).startsWith("type")) {
            int r = start + 5;
            while (code.charAt(r) != ' ' && code.charAt(r) != '\n') r++;
            while (code.charAt(r) != '\n') r++;
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
