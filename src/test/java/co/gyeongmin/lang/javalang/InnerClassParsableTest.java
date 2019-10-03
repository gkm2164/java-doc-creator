package co.gyeongmin.lang.javalang;

public class InnerClassParsableTest {
    public Object testFunction() {
        return new Object() {
            class TestClass {
                Integer testFunction() {
                    return 30;
                }
            }

            @Override
            public String toString() {
                return new TestClass().testFunction().toString();
            }
        };
    }
}
