module evalex;

import std.stdio;
import std.traits : isFloatingPoint;
import std.format, std.conv;
import std.ascii : isDigit;
import std.uni : isWhite;
import std.variant;
import std.range : empty;
import std.algorithm.searching : canFind;
import std.algorithm : startsWith;
import std.math;

final class Eval(TypeNumber)
if (isFloatingPoint!TypeNumber) {

    alias TokenValue = VariantN!(maxSize!(TypeNumber, dstring));

    private Lexer!(TypeNumber, TokenValue) lexer;
    private Parser!(TypeNumber, TokenValue) parser;
    private Interpreter!(TypeNumber, TokenValue) interpreter;

    this(){
        lexer = new Lexer!(TypeNumber, TokenValue)();
        parser = new Parser!(TypeNumber, TokenValue)(lexer);
        interpreter = new Interpreter!(TypeNumber, TokenValue)(parser);

    }

    TypeNumber evaluate(dstring expression){
        if (expression.empty) {
            throw new LexicalException("Empty input text");
        }

        lexer.reset(expression);
        parser.reset();
        return interpreter.interpret();
    }

    TypeNumber evaluate(string expression){
        return evaluate(expression.to!dstring);
    }
}

class EvalException : Exception {
    this(string message) {
        super(message);
    }
}

final class LexicalException : EvalException {
    this(string message) {
        super(message);
    }
}

final class ParserException : EvalException {
    this(string message) {
        super(message);
    }
}

final class InterpreterException : EvalException {
    this(string message) {
        super(message);
    }
}

private:

version(LOGGER){
    void writeln_log(T...)(T args){
        writeln(args);
        
    }
}

// Define token types
enum TokenType {
    NUMBER,
    PI,
    PLUS,
    MINUS,
    PARAMSEP,
    MUL,
    DIV,
    LPAREN, 
    RPAREN,
    EULER,
    EXP,
    EXPNAMED,
    EXP2NAMED,
    LOG,
    LOG10,
    LOG2,
    POW,
    SQRT,
    SIN,
    COS,
    TAN,
    ATAN,
    ATAN2,
    ASIN,
    ACOS,
    EOF
}

immutable TokenType[1] highPrecedence = [TokenType.EXP];

immutable TokenType[2] factorOps = [TokenType.MUL, TokenType.DIV];
immutable TokenType[2] termOps   = [TokenType.PLUS, TokenType.MINUS];

immutable TokenType[6] trigonometricUnary = [TokenType.SIN, TokenType.COS, TokenType.TAN, 
                                            TokenType.ATAN, TokenType.ASIN, TokenType.ACOS];
immutable TokenType[6] exponentialUnary = [TokenType.SQRT, TokenType.EXPNAMED, TokenType.EXP2NAMED, TokenType.LOG,
                                          TokenType.LOG10, TokenType.LOG2];

immutable TokenType[2] miscBinaryWithSep = [TokenType.ATAN2, TokenType.POW];

// Define the Token class
struct _Token(TokenValue) {
    TokenType type;
    TokenValue value;

    string toString(){
        return format("Token(%s, %s)", type.stringof, value.to!string);
    }
}

final class Lexer(TypeNumber, TokenValue) {

    alias Token = _Token!TokenValue;

    dstring text;
    size_t pos;
    Nullable!dchar currentChar;


    void reset(dstring text){
        this.text = text;
        pos = 0;
        currentChar = text[pos];
    }

    this(){
    }

    void setText(dstring text){
        if (text.empty) {
            throw new LexicalException("Empty input text");
        }

        this.text = text;
        pos = 0;
        currentChar = text[pos];
    }

    void error(){
        throw new LexicalException("Invalid character '" ~ text[pos].to!string ~ "' in the position " ~ pos.to!string);
    }

    void advance(size_t step = 1) {
        pos += step;
        if (pos > text.length - 1)
            currentChar.nullify();
        else
            currentChar = text[pos];
    }

    void skipWhitespace() {
        while (!currentChar.isNull && currentChar.get.isWhite)
            advance();
    }

    TypeNumber getValue(){
        dstring result;
        while (!currentChar.isNull && (currentChar.get.isDigit || currentChar.get == '.')){
            result ~= currentChar.get();
            advance();
        }
        return result.to!TypeNumber;
        
    }

    Token getNextToken(){
        while (!currentChar.isNull) {

            version(LOGGER) writeln_log("Current character: ", currentChar.get);

            if (currentChar.get.isWhite) {
                // Print skipping whitespace
                version(LOGGER) writeln_log("Skipping whitespace...");
                skipWhitespace();
                continue;
            }

            // Print parsing token
            version(LOGGER) writeln_log("Parsing token...");

            // Check if currentChar is the beginning of a number
            if (currentChar.get.isDigit) {
                auto token = Token(TokenType.NUMBER, TokenValue(getValue()));
                version(LOGGER) writeln_log("Parsed token: ", token);
                return token;
            }

            // Handle other tokens
            switch (currentChar.get) {
                // Add debug print for each token case
                case 'π':
                    advance(1);
                    return Token(TokenType.PI, TokenValue("pi"));
                    break;
                case 'p':
                    if (text[pos+1] == 'i') {
                        advance(2);
                        return Token(TokenType.PI, TokenValue("pi"));
                    } else if (text[pos+1..$].startsWith("ow")) {
                        version(LOGGER) writeln_log("Parsed token: pow");
                        advance(3);
                        return Token(TokenType.POW, TokenValue("pow"));
                    }
                    break;
                case '+':
                    version(LOGGER) writeln_log("Parsed token: PLUS");
                    advance();
                    return Token(TokenType.PLUS, TokenValue("+"));
                case '-':
                    version(LOGGER) writeln_log("Parsed token: MINUS");
                    advance();
                    return Token(TokenType.MINUS, TokenValue("-"));
                case ',':
                    version(LOGGER) writeln_log("Parsed token: PARAMSEP");
                    advance();
                    return Token(TokenType.PARAMSEP, TokenValue(","));
                case '*':
                    version(LOGGER) writeln_log("Parsed token: MUL");
                    advance();
                    return Token(TokenType.MUL, TokenValue("*"));
                case '/':
                    version(LOGGER) writeln_log("Parsed token: DIV");
                    advance();
                    return Token(TokenType.DIV, TokenValue("/"));
                case '(':
                    version(LOGGER) writeln_log("Parsed token: LPAREN");
                    advance();
                    return Token(TokenType.LPAREN, TokenValue("("));
                case ')':
                    version(LOGGER) writeln_log("Parsed token: RPAREN");
                    advance();
                    return Token(TokenType.RPAREN, TokenValue(")"));
                case '^':
                    version(LOGGER) writeln_log("Parsed token: EXP");
                    advance();
                    return Token(TokenType.EXP, TokenValue("^"));
                case 'e':
                    if (text[pos+1..$].startsWith("xp2")) {
                        version(LOGGER) writeln_log("Parsed token: exp2");
                        advance(4);
                        return Token(TokenType.EXP2NAMED, TokenValue("exp2"));
                    }else if (text[pos+1..$].startsWith("xp")) {
                        version(LOGGER) writeln_log("Parsed token: exp");
                        advance(3);
                        return Token(TokenType.EXPNAMED, TokenValue("exp"));
                    } else {
                        version(LOGGER) writeln_log("Parsed token: e");
                        advance();
                        return Token(TokenType.EULER, TokenValue("e"));
                    }
                    break;
                case 's':
                    if (text[pos+1..$].startsWith("in")) {
                        version(LOGGER) writeln_log("Parsed token: sin");
                        advance(3);
                        return Token(TokenType.SIN, TokenValue("sin"));
                    } else if (text[pos+1..$].startsWith("qrt")) {
                        version(LOGGER) writeln_log("Parsed token: sqrt");
                        advance(4);
                        return Token(TokenType.SQRT, TokenValue("sqrt"));
                    }
                    break;
                case 'c':
                    if (text[pos+1..$].startsWith("os")) {
                        version(LOGGER) writeln_log("Parsed token: cos");
                        advance(3);
                        return Token(TokenType.COS, TokenValue("cos"));
                    }
                    break;
                case 't':
                    if (text[pos+1..$].startsWith("an")) {
                        version(LOGGER) writeln_log("Parsed token: tan");
                        advance(3);
                        return Token(TokenType.TAN, TokenValue("tan"));
                    }
                    break;
                case 'a':
                    if (text[pos+1..$].startsWith("tan2")) {
                        version(LOGGER) writeln_log("Parsed token: atan2");
                        advance(5);
                        return Token(TokenType.ATAN2, TokenValue("atan2"));
                    } else if (text[pos+1..$].startsWith("tan")) {
                        version(LOGGER) writeln_log("Parsed token: atan");
                        advance(4);
                        return Token(TokenType.ATAN, TokenValue("atan"));
                    } else if (text[pos+1..$].startsWith("cos")) {
                        version(LOGGER) writeln_log("Parsed token: acos");
                        advance(4);
                        return Token(TokenType.ACOS, TokenValue("acos"));
                    } else if (text[pos+1..$].startsWith("sin")) {
                        version(LOGGER) writeln_log("Parsed token: asin");
                        advance(4);
                        return Token(TokenType.ASIN, TokenValue("asin"));
                    }
                    break;
                case 'l':
                    if (text[pos+1..$].startsWith("og10")) {
                        version(LOGGER) writeln_log("Parsed token: log10");
                        advance(5);
                        return Token(TokenType.LOG10, TokenValue("log10"));
                    } else if (text[pos+1..$].startsWith("og2")) {
                        version(LOGGER) writeln_log("Parsed token: log2");
                        advance(4);
                        return Token(TokenType.LOG2, TokenValue("log2"));
                    } else if (text[pos+1..$].startsWith("og")) {
                        version(LOGGER) writeln_log("Parsed token: log");
                        advance(3);
                        return Token(TokenType.LOG, TokenValue("log"));
                    }
                    break;
                default:
                    error();
            }
        }
        
        // If currentChar is null, return EOF token
        version(LOGGER) writeln_log("Parsed token: EOF");
        return Token(TokenType.EOF, TokenValue(null));
    }
}

// Define the AST node types
interface _ASTNode(TypeNumber) {}

final class BinaryOpNode(TypeNumber, TokenValue) : _ASTNode!TypeNumber {
    alias ASTNode = _ASTNode!TypeNumber;
    alias Token = _Token!TokenValue;

    ASTNode left;
    Token token;
    ASTNode right;

    alias op = token;

    this(ASTNode left, Token op, ASTNode right){
        this.left = left;
        this.token = op;
        this.right = right;
    }
}

final class NumberNode(TypeNumber, TokenValue) : _ASTNode!TypeNumber {
    alias ASTNode = _ASTNode!TypeNumber;
    alias Token = _Token!TokenValue;

    TypeNumber value;
    Token token;

    this(Token token){
        this.token = token;
        this.value = token.value.get!TypeNumber;
    }
}

final class UnaryOpNode(TypeNumber, TokenValue) : _ASTNode!TypeNumber {
    alias ASTNode = _ASTNode!TypeNumber;
    alias Token = _Token!TokenValue;

    Token token;
    ASTNode exprNode;

    alias op = token;

    this(Token op, ASTNode exprNode){
        this.token = op;
        this.exprNode = exprNode;
    }
}

final class Parser(TypeNumber, TokenValue) {
    alias ASTNode = _ASTNode!TypeNumber;
    alias Token = _Token!TokenValue;

    Lexer!(TypeNumber, TokenValue) lexer;
    Token currentToken;

    void reset(){
        this.currentToken = this.lexer.getNextToken();
    }

    this(Lexer!(TypeNumber, TokenValue) lexer){
        this.lexer = lexer;
    }

    void error(){
        throw new ParserException("Invalid syntax");
    }

    void eat(TokenType tokenType){
        if (currentToken.type == tokenType)
            currentToken = lexer.getNextToken();
        else
            error();
    }

    ASTNode factor() {
        auto token = currentToken;
        switch(token.type) {
            case TokenType.PLUS:
                eat(TokenType.PLUS);
                return new UnaryOpNode!(TypeNumber, TokenValue)(token, factor());
            case TokenType.MINUS:
                eat(TokenType.MINUS);
                return new UnaryOpNode!(TypeNumber, TokenValue)(token, factor());
            case TokenType.NUMBER:
                eat(TokenType.NUMBER);
                return new NumberNode!(TypeNumber, TokenValue)(token);
            case TokenType.PI:
                eat(TokenType.PI);
                return new NumberNode!(TypeNumber, TokenValue)(Token(TokenType.NUMBER, TokenValue(PI)));
            case TokenType.EULER:
                eat(TokenType.EULER);
                return new NumberNode!(TypeNumber, TokenValue)(Token(TokenType.NUMBER, TokenValue(E)));
            case TokenType.LPAREN:
                eat(TokenType.LPAREN);
                auto node = expr();
                eat(TokenType.RPAREN);
                return node;
            default:
                // handle unary functions
                if (trigonometricUnary[].canFind(token.type) || exponentialUnary[].canFind(token.type)) {
                    eat(token.type);
                    eat(TokenType.LPAREN);
                    auto node = expr();
                    eat(TokenType.RPAREN);
                    return new UnaryOpNode!(TypeNumber, TokenValue)(token, node);
                }
                // handle binary stuff (functions with two params such as atan2 and pow)
                else if (miscBinaryWithSep[].canFind(token.type)) {
                    eat(token.type);
                    eat(TokenType.LPAREN);
                    auto leftNode = expr();
                    eat(TokenType.PARAMSEP);
                    auto rightNode = expr();
                    eat(TokenType.RPAREN);
                    return new BinaryOpNode!(TypeNumber, TokenValue)(leftNode, token, rightNode);
                } else {
                    throw new ParserException("Unexpected token: '" ~ token.value.to!string ~ "' in position " ~ lexer.pos.to!string);
                }
        }
    }

    ASTNode highPrecedenceOps() {
        auto node = factor();

        while (highPrecedence[].canFind(currentToken.type)){
            auto token = currentToken;
            if(token.type == TokenType.EXP){
                eat(TokenType.EXP);
                node = new BinaryOpNode!(TypeNumber, TokenValue)(node, token, factor());
            }
        }
        return node;
    }

    ASTNode term() {
        auto node = highPrecedenceOps();

        while (factorOps[].canFind(currentToken.type)){
            auto token = currentToken;
            eat(token.type);
            node = new BinaryOpNode!(TypeNumber, TokenValue)(node, token, highPrecedenceOps());
        }

        return node;
    }

    ASTNode expr(){
        auto node = term();

        while (termOps[].canFind(currentToken.type)){
            auto token = currentToken;
            eat(token.type);
            node = new BinaryOpNode!(TypeNumber, TokenValue)(node, token, term());
        }

        return node;
    }

    ASTNode parse(){
        return expr();
    }
}

interface NodeVisitor(TypeNumber) {
    alias ASTNode = _ASTNode!TypeNumber;
    TypeNumber visit(ASTNode node);
}

final class Interpreter(TypeNumber, TokenValue) : NodeVisitor!TypeNumber {
    
    alias ASTNode = _ASTNode!TypeNumber;

    Parser!(TypeNumber, TokenValue) parser;

    this(Parser!(TypeNumber, TokenValue) parser){
        this.parser = parser;
    }

    TypeNumber visit(ASTNode node) {
        // Check if the node is a BinaryOpNode
        if (auto binOpNode = cast(BinaryOpNode!(TypeNumber, TokenValue))node) {
            return visit(binOpNode);
        }
        // Check if the node is a UnaryOpNode
        else if (auto unaryOp = cast(UnaryOpNode!(TypeNumber, TokenValue))node) {
            return visit(unaryOp);
        }
        // Check if the node is a NumberNode
        else if (auto numberNode = cast(NumberNode!(TypeNumber, TokenValue))node) {
            return visit(numberNode);
        }
        // Handle other node types if needed
        else {
            // Handle the case when the node is of an unknown type
            throw new InterpreterException("Unsupported node type. The operation requested is not supported.");
        }
    }

    TypeNumber interpret(){
        auto tree = parser.parse();
        return visit(tree);
    }

private:
    TypeNumber visit(BinaryOpNode!(TypeNumber, TokenValue) node) {
        switch(node.op.type) {
            case TokenType.PLUS:
                return visit(node.left) + visit(node.right);
            case TokenType.MINUS:
                return visit(node.left) - visit(node.right);
            case TokenType.MUL:
                return visit(node.left) * visit(node.right);
            case TokenType.DIV:
                return visit(node.left) / visit(node.right);
            case TokenType.EXP:
                return visit(node.left) ^^ visit(node.right);
            case TokenType.ATAN2:
                return atan2(visit(node.left), visit(node.right));
            case TokenType.POW:
                return pow(visit(node.left), visit(node.right));
            default:
                throw new InterpreterException("Unsupported binary operator: " ~ node.op.value.to!string);
        }
    }

    TypeNumber visit(NumberNode!(TypeNumber, TokenValue) node){
        return node.value;
    }

    TypeNumber visit(UnaryOpNode!(TypeNumber, TokenValue) node) {
        auto op = node.op.type;
        switch(op) {
            case TokenType.PLUS:
                return +visit(node.exprNode);
            case TokenType.MINUS:
                return -visit(node.exprNode);
            case TokenType.SQRT:
                return sqrt(visit(node.exprNode));
            case TokenType.EXPNAMED:
                return exp(visit(node.exprNode));
            case TokenType.EXP2NAMED:
                return exp2(visit(node.exprNode));
            case TokenType.LOG10:
                return log10(visit(node.exprNode));
            case TokenType.LOG2:
                return log2(visit(node.exprNode));
            case TokenType.LOG:
                return log(visit(node.exprNode));
            case TokenType.SIN:
                return sin(visit(node.exprNode));
            case TokenType.COS:
                return cos(visit(node.exprNode));
            case TokenType.TAN:
                return tan(visit(node.exprNode));
            case TokenType.ATAN:
                return atan(visit(node.exprNode));
            case TokenType.ASIN:
                return asin(visit(node.exprNode));
            case TokenType.ACOS:
                return acos(visit(node.exprNode));
            default:
                throw new InterpreterException("Unsupported unary operator: " ~ node.op.value.to!string);
        }
    }

}

/** Example 1

void main(){
    string text = "sin(0.5)*2^3-5*4";
        
    auto evaluator = new Eval!double();

    auto result = evaluator.evaluate(text);

    writeln(result);
}
*/

/** Example 2 - terminal calculator

void main(){
    string text;
    auto evaluator = new Eval!double();

    while(true){
        try{
            write("calc> ");
            text = readln();
        }catch(Exception e){
            break;
        }
        if(text is null)
            continue;
    
        try {
            auto result = evaluator.evaluate(text);
            writeln(result);
        } catch (Exception e) {
            writeln("Error: ", e.msg);
        }
    }
}
*/

// unittests
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
        auto evaluator = new Eval!T();

        foreach (testCase; testCases!T()) {
            auto expression = testCase[0];
            auto expected = testCase[1];

            auto result = evaluator.evaluate(expression);

            // Test that the evaluated result matches the expected result
            string assertMsg = "Type: " ~ T.stringof ~ ", Expression: " ~ expression ~ ", Expected: " ~ expected.to!string ~ ", Got: " ~ result.to!string;
            assert(isClose(result, expected), assertMsg);
            writeln(assertMsg);
        }
    }
}

// unittest for Exceptions
unittest {
    import std.exception : assertThrown;
    
    // Test invalid expression
    assertThrown!ParserException({
        auto evaluator = new Eval!double();
        evaluator.evaluate("2 + * 3");
    }());

    // Test undefined function (should throw a LexicalException for now)
    assertThrown!LexicalException({
        auto evaluator = new Eval!double();
        evaluator.evaluate("unknown(5)");
    }());

    // Test mismatched parentheses
    assertThrown!ParserException({
        auto evaluator = new Eval!double();
        evaluator.evaluate("(2 + 3");
    }());

    // Test invalid characters
    assertThrown!LexicalException({
        auto evaluator = new Eval!double();
        evaluator.evaluate("2 # 3");
    }());

    // Test empty expression
    assertThrown!LexicalException({
        auto evaluator = new Eval!double();
        evaluator.evaluate("");
    }());
}