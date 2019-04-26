// comment test
package com.kakao.search.middle.javalang;

import com.kakao.search.middle.javalang.defs.JavaDef;
import javafx.util.Pair;

import javax.annotation.Resource;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.Map;

//=desc: {아이고 힘들어라 2}
//=end
@Resource
public class JavaParser {

    //=desc: {아이고 힘들어라}
    //=args: [
    //   code: {코드}
    // ]
    //=end
    private void parse(Map<String, JavaDef> pkgTree, String code) throws FileNotFoundException {
        Iterator<JavaToken> it = Tokenizer.tokenize(code).iterator();
        try (PrintWriter pw = new PrintWriter("./sample.txt")) {
            while (it.hasNext()) {
                JavaToken jt = it.next();
                JavaTokenEnum jte = jt.getE();
                switch (jte) {
                    case PACKAGE:
                        String fullPackageName = parsePackage(it);
                        System.out.println("full package name: " + fullPackageName);
                        break;
                    case IMPORT:
                        Pair<String, String> imp = importRelation(it);
                        if (imp == null) continue;
                        System.out.println("import package: " + imp.getKey() + " -> " + imp.getValue());
                        break;
                    default:
                        pw.write(jt.getE() + "\t" + jt.getValue() + "\n");
                        break;
                }
            }
        }
    }

    private String parsePackage(Iterator<JavaToken> it) {
        StringBuilder fsb = new StringBuilder();

        while (it.hasNext()) {
            JavaToken pkgToken = it.next();
            if (pkgToken.getE() == JavaTokenEnum.SEMICOLON) {
                return fsb.toString();
            }
            fsb.append(pkgToken.getValue());
        }

        return "";
    }

    private Pair<String, String> importRelation(Iterator<JavaToken> it) {
        StringBuilder fullImport = new StringBuilder();
        while (it.hasNext()) {
            JavaToken importCtx = it.next();
            if (importCtx.getE() == JavaTokenEnum.SEMICOLON) {
                String ret = fullImport.toString();
                return new Pair<>(ret.substring(ret.lastIndexOf('.') + 1), ret);
            }

            fullImport.append(importCtx.getValue());
        }

        return null;
    }

    public static void main(String[] args) throws IOException {
        try (FileInputStream fis = new FileInputStream("./src/main/java/com/kakao/search/middle/javalang/JavaParser.java")) {
            byte[] buf = new byte[4096];
            StringBuilder sb = new StringBuilder();
            int read;
            while ((read = fis.read(buf)) > 0) {
                String s = new String(buf, 0, read);
                sb.append(s);
            }

            String code = sb.toString();
//            new JavaParser().parse(code);
        }
    }
} // comment is exist
