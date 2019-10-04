package co.gyeongmin.lang.javalang;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;


public class Tokenizer {
    static class Pair<K, V> {
        K key;
        V value;

        Pair(K key, V value) {
            this.key = key;
            this.value = value;
        }

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }
    }

    private static class Node {
        private final String prefix;
        private boolean isTerminal;
        private JavaTokenEnum tokenEnum;
        private boolean flush;
        private Node fail;
        private final HashMap<Character, Node> next = new HashMap<>();

        Node(String prefix) {
            this.prefix = prefix;
            this.isTerminal = false;
        }

        void setTerminal(JavaTokenEnum e) {
            this.isTerminal = true;
            this.tokenEnum = e;
        }

        Node getOrPut(char ch, Supplier<Node> nodeCreator) {
            if (!next.containsKey(ch)) {
                next.put(ch, nodeCreator.get());
            }
            return next.get(ch);
        }
    }

    private final Node root = new Node("ROOT");
    private static final Tokenizer tokenizer;


    static {
        tokenizer = new Tokenizer();
        for (JavaTokenEnum value : JavaTokenEnum.values()) {
            if (value.isKeyword)
                continue;
            tokenizer.insertToken(value);
        }

        tokenizer.buildFail();
    }


    private void insertToken(JavaTokenEnum e) {
        Node elem = root;
        String word = e.value;
        for (int i = 0; i < word.length(); i++) {
            char ch = word.charAt(i);

            int invariantIdx = i;
            elem = elem.getOrPut(ch, () -> new Node(word.substring(0, invariantIdx + 1)));
            if (i == 0) elem.flush = true;
        }
        elem.setTerminal(e);
    }

    private void buildFail() {
        Queue<Node> nodeQueue = new LinkedList<>();
        root.next.values().forEach((node) -> {
            node.fail = root;
            nodeQueue.add(node);
        });

        while (!nodeQueue.isEmpty()) {
            Node n = nodeQueue.poll();
            n.next.forEach((ch, node) -> {
                node.fail = n.fail.next.getOrDefault(ch, root);
                nodeQueue.add(node);
            });
        }
    }

    private static final Set<Character> WHITESPACES =
            "\t\n ".chars().mapToObj(c -> (char) c).collect(Collectors.toSet());

    private static boolean eqCharArray(char[] codes, int j, char[] untilChar) {
        if (codes.length - j < untilChar.length) return false;
        for (int i = 0; i < untilChar.length; i++) {
            if (codes[j + i] != untilChar[i]) return false;
        }
        return true;
    }

    private static Pair<JavaToken, Integer> takeUntil(char[] codes, int i, Node elem) {
        final boolean allowEscape = elem.tokenEnum.allowEscape;
        final char[] untilChar = elem.tokenEnum.until.toCharArray();

        int j = i;
        boolean escape = false;
        final StringBuilder buf = new StringBuilder();
        buf.append(elem.prefix);

        while (j < codes.length && ((allowEscape && escape) || !eqCharArray(codes, j, untilChar))) {
            if (escape) escape = false;
            else if (codes[j] == '\\') escape = true;
            buf.append(codes[j]);
            j++;
        }

        if (j < codes.length && untilChar.length > 1 || !WHITESPACES.contains(untilChar[0])) {
            buf.append(untilChar);
        }
        final JavaToken ret = new JavaToken(elem.tokenEnum.saveTo, buf.toString(), j);
        return new Pair<>(ret, j + untilChar.length - 1);
    }

    public static List<JavaToken> tokenize(String code) {
        final char[] codes = code.toCharArray();
        final List<JavaToken> ret = new ArrayList<>();
        Node elem = tokenizer.root;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < codes.length; i++) {
            char ch = codes[i];
            if (elem.next.containsKey(ch)) {
                elem = elem.next.get(ch);
                if (elem.flush && sb.length() > 0) {
                    ret.add(JavaToken.newToken(sb.toString(), i));
                    sb = new StringBuilder();
                }
                sb.append(ch);
            } else {
                /* 없을 경우
                1. 이전 노드가 terminal 상태일 경우
                 - terminal이면서 takeUntil이 있을 경우 -> 문자열, 코멘트
                 - terminal인데, takeUntil이 없을 경우 -> 그냥 밀어넣음
                2. 아닐 경우
                 - 만난 문자가 whitespace 일 경우 ==> flush
                 - 만난 문자가 token일 것 같은 경우 ==> buffer에 밀어넣음
                [공통]. fail로 노드 이동*/
                if (elem.isTerminal) {
                    if (elem.tokenEnum.takeUntil) {
                        final Pair<JavaToken, Integer> tokenWithIdx = takeUntil(codes, i, elem);
                        ret.add(tokenWithIdx.getKey());
                        i = tokenWithIdx.getValue();
                        elem = tokenizer.root;
                        sb = new StringBuilder();
                        continue;
                    } else {
                        ret.add(new JavaToken(elem.tokenEnum, elem.prefix, i));
                    }
                    sb = new StringBuilder();
                }

                if (WHITESPACES.contains(ch)) {
                    if (sb.length() > 0) {
                        ret.add(JavaToken.newToken(sb.toString(), i));

                    }
                    sb = new StringBuilder();

                    while (i + 1 < codes.length && WHITESPACES.contains(codes[i + 1])) {
                        i++;
                    }
                    elem = tokenizer.root;
                } else {
                    sb.append(ch);
                    while (elem != tokenizer.root && !elem.next.containsKey(ch)) {
                        elem = elem.fail;
                    }

                    if (elem.next.containsKey(ch)) {
                        elem = elem.next.get(ch);
                    }
                }
            }
        }

        if (sb.length() > 0) {
            ret.add(JavaToken.newToken(sb.toString(), codes.length));
        }

        return ret;
    }
}