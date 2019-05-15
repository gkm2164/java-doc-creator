package co.gyeongmin.scala.godoc.golang;

import co.gyeongmin.scala.godoc.exceptions.TokenNoMoreToConsumeException;
import co.gyeongmin.scala.godoc.exceptions.TokenNotAcceptedException;
import co.gyeongmin.scala.godoc.golang.syntax.GoFuncArg;
import co.gyeongmin.scala.godoc.golang.syntax.GoFunctionDef;
import co.gyeongmin.scala.godoc.golang.syntax.GoSomeDef;
import co.gyeongmin.scala.godoc.golang.syntax.GoTypeDef;

import java.util.*;
import java.util.function.Predicate;
import java.util.logging.Logger;

//= Go 문서 파서
public class GoParser {
    //= Go 토큰 데이터 타입
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

        boolean is(GoTokenEnum en) {
            return e == en;
        }

        boolean isOneOf(GoTokenEnum... enums) {
            return e.isOneOf(enums);
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

        public boolean isOneOf(GoTokenEnum... arr) {
            for (GoTokenEnum e : arr) {
                if (this == e) {
                    return true;
                }
            }

            return false;
        }
    }

    private static HashMap<Character, GoTokenEnum> charTokenMap = new HashMap<>();
    private static HashSet<Character> specialChars = new HashSet<>();

    static {
        charTokenMap.put('(', GoTokenEnum.LBRACKET);
        charTokenMap.put(')', GoTokenEnum.RBRACKET);
        charTokenMap.put(',', GoTokenEnum.COMMA);

        String s = "(),\n ";
        for (int i = 0; i < s.length(); i++) {
            specialChars.add(s.charAt(i));
        }
    }

    private static GoToken[] tokenize(String def) {
        ArrayList<GoToken> ret = new ArrayList<>();

        StringBuffer sb = new StringBuffer();
        int charNum = 0;
        int lineNum = 0;


        for (int i = 0; i < def.length(); i++) {
            char ch = def.charAt(i);
            if (specialChars.contains(ch) && sb.length() > 0) {
                ret.add(GoToken.create(sb.toString(), charNum, lineNum));
                sb = new StringBuffer();
            }

            switch (ch) {
                case '(':
                case ')':
                case ',':
                    ret.add(new GoToken(charTokenMap.get(ch), charNum, lineNum));
                    break;
                case '\n':
                    lineNum += 1;
                    charNum = 0;
                case ' ':
                    break;
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
//
//    public static void debugToken(GoToken[] tokens) {
//        int num = 0;
//        for (GoToken e : tokens) {
//            System.out.print(num++ + ": " + e.e);
//            if (e.e == GoTokenEnum.STRING) {
//                System.out.print("(" + e.value + ")");
//            }
//            System.out.println();
//        }
//    }

    private static class GoTokenIterator implements Iterator<GoToken> {
        private int idx = 0;
        private GoToken[] tokens = null;

        private Logger log = Logger.getLogger("GoTokenIterator");

        GoTokenIterator(GoToken[] tokens) {
            this.tokens = tokens;
        }

        @Override
        public boolean hasNext() {
            return idx < tokens.length;
        }

        GoToken glanceNext() {
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

        GoToken assertTokenEq(GoTokenEnum e) throws TokenNotAcceptedException, TokenNoMoreToConsumeException {
            return assertToken((token) -> token.e == e);
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

        // should start with "func" or "type"
        GoToken defType = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.FUNC, GoTokenEnum.TYPE});
        if (defType.e == GoTokenEnum.FUNC) { // if it starts with "func",
            GoFunctionDef fd = new GoFunctionDef();
            // then next token should be "(" for receiver, or string for function name
            GoToken mayFuncName = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.LBRACKET, GoTokenEnum.STRING});
            if (mayFuncName.e == GoTokenEnum.LBRACKET) { // if it starts with "(",
                GoToken receiverName = it.assertTokenEq(GoTokenEnum.STRING); // it should start with receiver name
                GoToken receiverType = it.assertTokenEq(GoTokenEnum.STRING); // and the type name should be followed
                it.assertTokenEq(GoTokenEnum.RBRACKET); // also, ")" required to enclose the receiver type definition.

                fd.receiverName = receiverName.value;
                fd.receiverType = receiverType.value;
                mayFuncName = it.next();
            }

            fd.funcName = mayFuncName.value;
            it.assertTokenEq(GoTokenEnum.LBRACKET); // after the function name, no matter arguments exist or not, '(' should come.
            Queue<GoToken> pendingVar = new LinkedList<>();

            // until now... "func x(" or "func (r *SomeType) x("

            // argument parse
            while (it.hasNext()) {
                // may ')' for stop, WORD for continue
                GoToken mayFinishOrString = it.assertTokenOr(new GoTokenEnum[]{GoTokenEnum.RBRACKET, GoTokenEnum.STRING});
                if (mayFinishOrString.is(GoTokenEnum.RBRACKET)) {
                    break;
                }
                // STRING COMMA || STRING STRING
                pendingVar.add(mayFinishOrString);
                GoToken mayCommaOrElse = it.next();
                if (mayCommaOrElse.is(GoTokenEnum.COMMA)) {
                    continue;
                }

                ArrayList<GoToken> typeNameTokens = new ArrayList<>();
                int pars = 0;
                GoToken argCtx = mayCommaOrElse;
                for (; it.hasNext()
                        && !(pars == 0 && argCtx.isOneOf(GoTokenEnum.COMMA, GoTokenEnum.RBRACKET));
                     argCtx = it.next()) {
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
                }

                // in case of multiple parameter definition over 1 type, like
                // func x(a, b, c int) int ...
                // definition should be in incomplete state
                int pendingVarCount = pendingVar.size();
                ArrayList<GoFuncArg> args = new ArrayList<>(pendingVarCount);
                ArrayList<String> tnList = new ArrayList<>();
                int until = typeNameTokens.size();
                for (int i = 0; i < until; i++) {
                    if (typeNameTokens.get(i).is(GoTokenEnum.LBRACKET)
                            || (i + 1 < until && typeNameTokens.get(i + 1).isOneOf(GoTokenEnum.RBRACKET, GoTokenEnum.COMMA))) {
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

                if (argCtx.is(GoTokenEnum.RBRACKET)) {
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
                            GoToken glanced = it.glanceNext();
                            if (it.hasNext() && glanced != null) {
                                if (!glanced.isOneOf(GoTokenEnum.COMMA, GoTokenEnum.RBRACKET)) {
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
        GoToken typeName = it.assertTokenEq(GoTokenEnum.STRING);

        GoTypeDef ret = new GoTypeDef();
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
