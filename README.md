# EvalEx
A small library for evaluating mathematical expressions in strings.

## TODO:
- better error reporting for syntax errors
- Improve catching exceptions.

## Examples:
```d
    import std.stdio;
    import evalex;
    void main(){
    string text = "sin(0.5)*2^3-5*4";
        
    auto evaluator = new Eval!double(text);

    auto result = evaluator.result;

    writeln(result);
}

```

// Refer to the unit tests for capabilities
```d
import std.typecons;
import std.math;
import std.meta;

// Define test cases with expressions and expected results
auto testCases(T)() {
    return [
        // Basic arithmetic
        tuple("-2+3", T(-2+3)),
        tuple("5*4-3/6", T(5*4-3/6.0)),
        tuple("(2+3)*4", T((2+3)*4)),
        tuple("2^3", T(8.0)),
        tuple("1.0-sqrt(95.2 - 8.3/3.0)", T(1.0-sqrt(95.2 - 8.3/3.0))),
        tuple("sin(π/2)", sin(T(PI)/2)), // handle symbol pi
        tuple("log10(100) + log2(8) - log(e) * exp2(3)", T(log10(100.0) + log2(8.0) - log(E) * exp2(3.0))),

        // Trigonometric functions
        tuple("sin(0.5)", sin(T(0.5))),
        tuple("cos(0.5)", cos(T(0.5))),
        tuple("tan(0.5)", tan(T(0.5))),
        tuple("atan(0.5)", atan(T(0.5))),
        tuple("sin(0.5+2)", sin(T(0.5+2))),
        tuple("cos(0.5-2)", cos(T(0.5-2))),
        tuple("tan(pi*2)", tan(T(PI*2))),
        tuple("atan(0.5/2)", atan(T(0.5/2))),
        tuple("asin(0.5/2)", asin(T(0.5/2))),
        tuple("acos(0.5/2)", acos(T(0.5/2))),

        // Nested functions
        tuple("sin(cos(tan(0.5)))", sin(cos(tan(T(0.5))))),

        // Complex expressions
        tuple("1.5+2.5*3-atan(0.5)", T(1.5+2.5*3-atan(0.5))),
        tuple("(2^3-5)*pow(2, 2)+sin(0.5)", T((8-5)*pow(2.0, 2.0)+sin(0.5))),
        tuple("sin(0.5)*2^3-5*4", T(sin(0.5)*8-5*4)),
        tuple("sin(0.5*(2+3))-cos(0.25^2)", T(sin(0.5*(2+3))-cos(0.25^^2))),
        tuple("3 * (exp2(4) + sin(0.5 * pi)) - atan(1) + 16^0.5 / exp(2) + cos(0.25 * pi) * tan(0.75 * pi)", 
            T(3.0 * (exp2(4.0) + sin(0.5 * PI)) - atan(1.0) + 16^^0.5 / exp(2.0) + cos(0.25 * T(PI)) * tan(0.75 * T(PI)))),

        tuple("pi", T(PI)),
        tuple("pi * 2", T(2.0 * PI)),
        tuple("sin(pi / 2)", T(1.0)),
        tuple("cos(pi)", T(-1.0)),
        tuple("tan(pi / 4)", T(1.0)),
        tuple("atan(pi / 4)", T(atan(PI/4)))
    ];
}

alias FloatingPoints = AliasSeq!(real, double, float);

static foreach (T; FloatingPoints) {
    unittest {
        foreach (testCase; testCases!T()) {
            auto expression = testCase[0];
            auto expected = testCase[1];

            auto evaluator = new Eval!T(expression);

            auto result = evaluator.result;

            // Test that the evaluated result matches the expected result
            string assertMsg = "Type: " ~ T.stringof ~ ", Expression: " ~ expression ~ ", Expected: " ~ expected.to!string ~ ", Got: " ~ result.to!string;
            assert(isClose(result, expected), assertMsg);
            writeln(assertMsg);
        }
    }
}
```