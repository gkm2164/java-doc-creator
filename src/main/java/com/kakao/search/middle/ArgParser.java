package com.kakao.search.middle;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ArgParser {
    public static class ParseAfter {
        public String typeName;
        public String desc;
        public String after;
    }

    public static class Argument {
        public String name;
        public String typeName;
        public String desc;

    }

    private static ParseAfter parseAfter(String str, int ptr) {
        StringBuffer sb = new StringBuffer();
        ParseAfter ret = new ParseAfter();

        for (int i = ptr; i < str.length(); i++) {
            if (str.charAt(i) == '{') {
                ret.typeName = sb.toString();
                sb = new StringBuffer();
                int j = i + 1;
                for (; j < str.length(); j++) {
                    if (str.charAt(j) == '}') {
                        break;
                    }
                    sb.append(str.charAt(j));
                }
                ret.desc = sb.toString();
                sb = new StringBuffer();
                continue;
            } else if (str.charAt(i) == ',') {
                ret.after = str.substring(i + 1);
                return ret;
            }

            sb.append(str.charAt(i));
        }

        return ret;
    }

    public static List<Argument> parseArgList(String str) {
        int sPtr = 0;
        ArrayList<Argument> ret = new ArrayList<>();
        StringBuffer sb = new StringBuffer();
        while(sPtr < str.length()) {
            if (str.charAt(sPtr) == ':') {
                ParseAfter remains = parseAfter(str, ++sPtr);
                Argument arg = new Argument();
                arg.name = sb.toString();
                arg.typeName = remains.typeName;
                arg.desc = remains.desc;
                ret.add(arg);

                sb = new StringBuffer();

                str = remains.after == null ? "" : remains.after;
                sPtr = 0;
                continue;
            }
            sb.append(str.charAt(sPtr++));
        }

        return ret;
    }
}
