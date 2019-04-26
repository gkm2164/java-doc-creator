package com.kakao.search.middle.javalang.defs;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class JavaDef {
    protected Map<String, JavaDef> children = new HashMap<>();
}

class JavaPackage extends JavaDef {

}

class JavaClass extends JavaDef {

}

class JavaInterface extends JavaDef {

}

class JavaFunction extends JavaDef {

}

class JavaMember extends JavaDef {

}