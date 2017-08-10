/*
 * [The "BSD license"]
 *  Copyright (c) 2014 Terence Parr
 *  Copyright (c) 2014 Sam Harwell
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * A Java 8 grammar for ANTLR 4 derived from the Java Language Specification
 * chapter 19.
 *
 * NOTE: This grammar results in a generated parser that is much slower
 *       than the Java 7 grammar in the grammars-v4/java directory. This
 *     one is, however, extremely close to the spec.
 *
 * You can test with
 *
 *  $ antlr4 Java8.g4
 *  $ javac *.java
 *  $ grun Java8 compilationUnit *.java
 *
 * Or,
~/antlr/code/grammars-v4/java8 $ java Test .
/Users/parrt/antlr/code/grammars-v4/java8/./Java8BaseListener.java
/Users/parrt/antlr/code/grammars-v4/java8/./Java8Lexer.java
/Users/parrt/antlr/code/grammars-v4/java8/./Java8Listener.java
/Users/parrt/antlr/code/grammars-v4/java8/./Java8Parser.java
/Users/parrt/antlr/code/grammars-v4/java8/./Test.java
Total lexer+parser time 30844ms.
 */
grammar Java8;

/*
 * Productions from §3 (Lexical Structure)
 */

literal
	:	IntegerLiteral
	|	FloatingPointLiteral
	|	BooleanLiteral
	|	CharacterLiteral
	|	StringLiteral
	|	NullLiteral
	;

/*
 * Productions from §4 (Types, Values, and Variables)
 */

type
	:	primitiveType
	|	referenceType
	;

primitiveType
	:	annotation* numericType
	|	annotation* 'boolean'
	;

numericType
	:	integralType
	|	floatingPointType
	;

integralType
	:	'byte'
	|	'short'
	|	'int'
	|	'long'
	|	'char'
	;

floatingPointType
	:	'float'
	|	'double'
	;

referenceType
	:	classOrInterfaceType
	|	typeVariable
	|	arrayType
	;

classOrInterfaceType
	:	annIdentifierWithTypeArguments ('.' annIdentifierWithTypeArguments)*
	;

annIdentifierWithTypeArguments
	:	annotation* Identifier typeArguments?
	;

typeVariable
	:	annIdentifier
	;

arrayType
	:	primitiveType dims
	|	classOrInterfaceType dims
	|	typeVariable dims
	;

dims
	:	dim+
	;

dim
	:	annotation* '[' ']'
	;

unannDims
	:	unannDim+
	;

unannDim
	:	'[' ']'
	;

typeParameter
	:	annotation* Identifier typeBound?
	;

typeBound
	:	'extends' typeVariable
	|	'extends' classOrInterfaceType ('&' classOrInterfaceType)*
	;

typeArguments
	:	'<' typeArgumentList '>'
	;

typeArgumentList
	:	typeArgument (',' typeArgument)*
	;

typeArgument
	:	referenceType
	|	wildcard
	;

wildcard
	:	annotation* '?' wildcardBounds?
	;

wildcardBounds
	:	isExtends='extends' referenceType
	|	isSuper='super' referenceType
	;

/*
 * Productions from §6 (Names)
 */

dottedIdentifier
	:	Identifier ('.' Identifier)*
	;

/*
 * Productions from §7 (Packages)
 */

compilationUnit
	:	packageDeclaration? importDeclaration* typeDeclaration* EOF
	;

packageDeclaration
	:	packageModifier* 'package' dottedIdentifier ';'
	;

packageModifier
	:	annotation
	;

importDeclaration
	:	'import' (isStatic='static')? dottedIdentifier ('.' isWildcard='*')? ';'
	;

typeDeclaration
	:	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

/*
 * Productions from §8 (Classes)
 */

classDeclaration
	:	normalClassDeclaration
	|	enumDeclaration
	;

normalClassDeclaration
	:	classModifier* 'class' Identifier typeParameters? superclass? superinterfaces? classBody
	;

classModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'final'
	|	'strictfp'
	;

typeParameters
	:	'<' typeParameterList '>'
	;

typeParameterList
	:	typeParameter (',' typeParameter)*
	;

superclass
	:	'extends' classOrInterfaceType
	;

superinterfaces
	:	'implements' interfaceTypeList
	;

interfaceTypeList
	:	classOrInterfaceType (',' classOrInterfaceType)*
	;

classBody
	:	'{' classBodyDeclaration* '}'
	;

classBodyDeclaration
	:	classMemberDeclaration
	|	instanceInitializer
	|	staticInitializer
	|	constructorDeclaration
	;

classMemberDeclaration
	:	fieldDeclaration
	|	methodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

fieldDeclaration
	:	fieldModifier* unannType variableDeclaratorList ';'
	;

fieldModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'static'
	|	'final'
	|	'transient'
	|	'volatile'
	;

variableDeclaratorList
	:	variableDeclarator (',' variableDeclarator)*
	;

variableDeclarator
	:	variableDeclaratorId ('=' variableInitializer)?
	;

variableDeclaratorId
	:	Identifier dims?
	;

variableInitializer
	:	expression
	|	arrayInitializer
	;

unannType
	:	unannPrimitiveType
	|	unannReferenceType
	;

unannPrimitiveType
	:	numericType
	|	'boolean'
	;

unannReferenceType
	:	unannClassOrInterfaceType
	|	unannTypeVariable
	|	unannArrayType
	;

unannClassOrInterfaceType
	:	unannIdentifierWithTypeArguments ('.' annIdentifierWithTypeArguments)*
	;

unannIdentifierWithTypeArguments
	:	Identifier typeArguments?
	;

unannTypeVariable
	:	Identifier
	;

unannArrayType
	:	(unannPrimitiveType | unannClassOrInterfaceType | unannTypeVariable) dims
	;

annIdentifier
	:	annotation* Identifier
	;

methodDeclaration
	:	methodModifier* methodHeader methodBody
	;

methodModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'final'
	|	'synchronized'
	|	'native'
	|	'strictfp'
	;

methodHeader
	:	(typeParameters annotation*)? result methodDeclarator throws_?
	;

result
	:	unannType
	|	'void'
	;

methodDeclarator
	:	Identifier '(' formalParameterList? ')' dims?
	;

formalParameterList
	:	(formalParameter | receiverParameter) (',' formalParameter)* (',' expandedFormalParameter)?
	|	expandedFormalParameter
	;

formalParameter
	:	variableModifier* unannType variableDeclaratorId
	;

variableModifier
	:	annotation
	|	'final'
	;

expandedFormalParameter
	:	variableModifier* unannType annotation* '...' variableDeclaratorId
	;

receiverParameter
	:	annotation* unannType (Identifier '.')? 'this'
	;

throws_
	:	'throws' exceptionTypeList
	;

exceptionTypeList
	:	exceptionType (',' exceptionType)*
	;

exceptionType
	:	classOrInterfaceType
	|	typeVariable
	;

methodBody
	:	block
	|	';'
	;

instanceInitializer
	:	block
	;

staticInitializer
	:	'static' block
	;

constructorDeclaration
	:	constructorModifier* constructorDeclarator throws_? constructorBody
	;

constructorModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	;

constructorDeclarator
	:	typeParameters? Identifier '(' formalParameterList? ')'
	;

constructorBody
	:	'{' explicitConstructorInvocation? blockStatements? '}'
	;

explicitConstructorInvocation
	:	typeArguments? isThis='this' '(' argumentList? ')' ';'
	|	((dottedIdentifier | primary) '.')? typeArguments? 'super' '(' argumentList? ')' ';'
	;

enumDeclaration
	:	classModifier* 'enum' Identifier superinterfaces? enumBody
	;

enumBody
	:	'{' enumConstantList? ','? enumBodyDeclarations? '}'
	;

enumConstantList
	:	enumConstant (',' enumConstant)*
	;

enumConstant
	:	enumConstantModifier* Identifier ('(' argumentList? ')')? classBody?
	;

enumConstantModifier
	:	annotation
	;

enumBodyDeclarations
	:	';' classBodyDeclaration*
	;

/*
 * Productions from §9 (Interfaces)
 */

interfaceDeclaration
	:	normalInterfaceDeclaration
	|	annotationTypeDeclaration
	;

normalInterfaceDeclaration
	:	interfaceModifier* 'interface' Identifier typeParameters? extendsInterfaces? interfaceBody
	;

interfaceModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'strictfp'
	;

extendsInterfaces
	:	'extends' interfaceTypeList
	;

interfaceBody
	:	'{' interfaceMemberDeclaration* '}'
	;

interfaceMemberDeclaration
	:	constantDeclaration
	|	interfaceMethodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

constantDeclaration
	:	constantModifier* unannType variableDeclaratorList ';'
	;

constantModifier
	:	annotation
	|	'public'
	|	'static'
	|	'final'
	;

interfaceMethodDeclaration
	:	interfaceMethodModifier* methodHeader methodBody
	;

interfaceMethodModifier
	:	annotation
	|	'public'
	|	'abstract'
	|	'default'
	|	'static'
	|	'strictfp'
	;

annotationTypeDeclaration
	:	interfaceModifier* '@' 'interface' Identifier annotationTypeBody
	;

annotationTypeBody
	:	'{' annotationTypeMemberDeclaration* '}'
	;

annotationTypeMemberDeclaration
	:	annotationTypeElementDeclaration
	|	constantDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

annotationTypeElementDeclaration
	:	annotationTypeElementModifier* unannType Identifier '(' ')' dims? defaultValue? ';'
	;

annotationTypeElementModifier
	:	annotation
	|	'public'
	|	'abstract'
	;

defaultValue
	:	'default' elementValue
	;

annotation
	:	normalAnnotation
	|	markerAnnotation
	|	singleElementAnnotation
	;

normalAnnotation
	:	'@' dottedIdentifier '(' elementValuePairList? ')'
	;

elementValuePairList
	:	elementValuePair (',' elementValuePair)*
	;

elementValuePair
	:	Identifier '=' elementValue
	;

elementValue
	:	conditionalExpression
	|	elementValueArrayInitializer
	|	annotation
	;

elementValueArrayInitializer
	:	'{' elementValueList? ','? '}'
	;

elementValueList
	:	elementValue (',' elementValue)*
	;

markerAnnotation
	:	'@' dottedIdentifier
	;

singleElementAnnotation
	:	'@' dottedIdentifier '(' elementValue ')'
	;

/*
 * Productions from §10 (Arrays)
 */

arrayInitializer
	:	'{' variableInitializerList? ','? '}'
	;

variableInitializerList
	:	variableInitializer (',' variableInitializer)*
	;

/*
 * Productions from §14 (Blocks and Statements)
 */

block
	:	'{' blockStatements? '}'
	;

blockStatements
	:	blockStatement+
	;

blockStatement
	:	localVariableDeclarationStatement
	|	classDeclaration
	|	statement
	;

localVariableDeclarationStatement
	:	localVariableDeclaration ';'
	;

localVariableDeclaration
	:	variableModifier* unannType variableDeclaratorList
	;

statement
	:	statementWithoutTrailingSubstatement
	|	labeledStatement
	|	ifThenStatement
	|	ifThenElseStatement
	|	whileStatement
	|	forStatement
	;

statementNoShortIf
	:	statementWithoutTrailingSubstatement
	|	labeledStatementNoShortIf
	|	ifThenElseStatementNoShortIf
	|	whileStatementNoShortIf
	|	forStatementNoShortIf
	;

statementWithoutTrailingSubstatement
	:	block
	|	emptyStatement
	|	expressionStatement
	|	assertStatement
	|	switchStatement
	|	doStatement
	|	breakStatement
	|	continueStatement
	|	returnStatement
	|	synchronizedStatement
	|	throwStatement
	|	tryStatement
	;

emptyStatement
	:	';'
	;

labeledStatement
	:	Identifier ':' statement
	;

labeledStatementNoShortIf
	:	Identifier ':' statementNoShortIf
	;

expressionStatement
	:	statementExpression ';'
	;

statementExpression
	:	assignment
	|	preIncrementExpression
	|	preDecrementExpression
	|	postIncrementExpression
	|	postDecrementExpression
	|	methodInvocation
	|	classInstanceCreationExpression
	;

ifThenStatement
	:	'if' '(' expression ')' statement
	;

ifThenElseStatement
	:	'if' '(' expression ')' ifStatement=statementNoShortIf 'else' elseStatement=statement
	;

ifThenElseStatementNoShortIf
	:	'if' '(' expression ')' ifStatement=statementNoShortIf 'else' elseStatement=statementNoShortIf
	;

assertStatement
	:	'assert' value=expression (':' error=expression)? ';'
	;

switchStatement
	:	'switch' '(' expression ')' switchBlock
	;

switchBlock
	:	'{' switchBlockStatementGroup* switchLabel* '}'
	;

switchBlockStatementGroup
	:	switchLabel+ blockStatements
	;

switchLabel
	:	'case' constantExpression ':'
	|	'case' enumConstantName ':'
	|	'default' ':'
	;

enumConstantName
	:	Identifier
	;

whileStatement
	:	'while' '(' expression ')' statement
	;

whileStatementNoShortIf
	:	'while' '(' expression ')' statementNoShortIf
	;

doStatement
	:	'do' statement 'while' '(' expression ')' ';'
	;

forStatement
	:	basicForStatement
	|	enhancedForStatement
	;

forStatementNoShortIf
	:	basicForStatementNoShortIf
	|	enhancedForStatementNoShortIf
	;

basicForStatement
	:	'for' '(' forInit? ';' expression? ';' forUpdate? ')' statement
	;

basicForStatementNoShortIf
	:	'for' '(' forInit? ';' expression? ';' forUpdate? ')' statementNoShortIf
	;

forInit
	:	statementExpressionList
	|	localVariableDeclaration
	;

forUpdate
	:	statementExpressionList
	;

statementExpressionList
	:	statementExpression (',' statementExpression)*
	;

enhancedForStatement
	:	'for' '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statement
	;

enhancedForStatementNoShortIf
	:	'for' '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statementNoShortIf
	;

breakStatement
	:	'break' Identifier? ';'
	;

continueStatement
	:	'continue' Identifier? ';'
	;

returnStatement
	:	'return' expression? ';'
	;

throwStatement
	:	'throw' expression ';'
	;

synchronizedStatement
	:	'synchronized' '(' expression ')' block
	;

tryStatement
	:	'try' block catches
	|	'try' block catches? finally_
	|	'try' '(' resource (';' resource)* ';'? ')' block catches? finally_?
	;

catches
	:	catchClause catchClause*
	;

catchClause
	:	'catch' '(' catchFormalParameter ')' block
	;

catchFormalParameter
	:	variableModifier* catchType variableDeclaratorId
	;

catchType
	:	unannClassOrInterfaceType ('|' classOrInterfaceType)*
	;

finally_
	:	'finally' block
	;

resource
	:	variableModifier* unannType variableDeclaratorId '=' expression
	;

/*
 * Productions from §15 (Expressions)
 */

primary
	:	(	primaryNoNewArray_lfno_primary
		|	arrayCreationExpression
		)
		(	primaryNoNewArray_lf_primary
		)*
	;

primaryNoNewArray
	:	literal
	|	classReference
	|	voidClassReference
	|	thisReference
	|	'(' expression ')'
	|	classInstanceCreationExpression
	|	fieldAccess
	|	arrayAccess
	|	methodInvocation
	|	methodReference
	;

primaryNoNewArray_lfno_arrayAccess
	:	literal
	|	classReference
	|	voidClassReference
	|	thisReference
	|	'(' expression ')'
	|	classInstanceCreationExpression
	|	fieldAccess
	|	methodInvocation
	|	methodReference
	;

primaryNoNewArray_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	arrayAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lfno_primary
	:	literal
	|	classReference
	|	primitiveClassReference
	|	voidClassReference
	|	thisReference
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	arrayAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary
	:	literal
	|	classReference
	|	primitiveClassReference
	|	voidClassReference
	|	thisReference
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

classReference
	:	dottedIdentifier unannDims? '.' 'class'
	;

primitiveClassReference
	:	unannPrimitiveType unannDims? '.' 'class'
	;

voidClassReference
	:	'void' '.' 'class'
	;

thisReference
	:	(dottedIdentifier '.')? 'this'
	;

classInstanceCreationExpression
	:	'new' standaloneConstructorType '(' argumentList? ')' classBody?
	|	dottedIdentifier '.' 'new' nestedConstructorType '(' argumentList? ')' classBody?
	|	primary '.' 'new' nestedConstructorType '(' argumentList? ')' classBody?
	;

classInstanceCreationExpression_lf_primary
	:	'.' 'new' nestedConstructorType '(' argumentList? ')' classBody?
	;

classInstanceCreationExpression_lfno_primary
	:	'new' standaloneConstructorType '(' argumentList? ')' classBody?
	|	dottedIdentifier '.' 'new'  '(' argumentList? ')' classBody?
	;

standaloneConstructorType
	:	typeArguments? annIdentifier ('.' annIdentifier)* typeArgumentsOrDiamond?
	;

nestedConstructorType
	:	typeArguments? annIdentifier typeArgumentsOrDiamond?
	;


typeArgumentsOrDiamond
	:	typeArguments
	|	'<' '>'
	;

fieldAccess
	:	(primary | (dottedIdentifier '.')? 'super') '.' Identifier
	;

fieldAccess_lf_primary
	:	'.' Identifier
	;

fieldAccess_lfno_primary
	:	(dottedIdentifier '.')? 'super' '.' Identifier
	;

arrayAccess
	:	(	dottedIdentifier | primaryNoNewArray_lfno_arrayAccess )
		( '[' expression ']' )+
	;

arrayAccess_lf_primary
	:	primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
		(	'[' expression ']' )+
	;

arrayAccess_lfno_primary
	:	(	dottedIdentifier |	primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary)
		(	'[' expression ']' )+
	;

methodInvocation
	:	methodInvocation_lfno_primary
	|	primary methodInvocation_lf_primary
	;

methodInvocation_lf_primary
	:	'.' typeArguments? Identifier '(' argumentList? ')'
	;

methodInvocation_lfno_primary
	:	Identifier '(' argumentList? ')'
	|	dottedIdentifier methodInvocation_lf_primary
	|	onSuper='super' methodInvocation_lf_primary
	|	dottedIdentifier '.' onSuper='super' methodInvocation_lf_primary
	;

argumentList
	:	expression (',' expression)*
	;

methodReference
	:	dottedIdentifier '::' typeArguments? Identifier
	|	referenceType '::' typeArguments? Identifier
	|	primary '::' typeArguments? Identifier
	|	'super' '::' typeArguments? Identifier
	|	dottedIdentifier '.' 'super' '::' typeArguments? Identifier
	|	classOrInterfaceType '::' typeArguments? 'new'
	|	arrayType '::' 'new'
	;

methodReference_lf_primary
	:	'::' typeArguments? Identifier
	;

methodReference_lfno_primary
	:	dottedIdentifier '::' typeArguments? Identifier
	|	referenceType '::' typeArguments? Identifier
	|	'super' '::' typeArguments? Identifier
	|	dottedIdentifier '.' 'super' '::' typeArguments? Identifier
	|	classOrInterfaceType '::' typeArguments? 'new'
	|	arrayType '::' 'new'
	;

arrayCreationExpression
	:	'new' (primitiveType | classOrInterfaceType) (dimExprs dims? | dims arrayInitializer)
	;

dimExprs
	:	dimExpr dimExpr*
	;

dimExpr
	:	annotation* '[' expression ']'
	;

constantExpression
	:	expression
	;

expression
	:	lambdaExpression
	|	assignmentExpression
	;

lambdaExpression
	:	lambdaParameters '->' lambdaBody
	;

lambdaParameters
	:	Identifier
	|	'(' formalParameterList? ')'
	|	'(' inferredFormalParameterList ')'
	;

inferredFormalParameterList
	:	Identifier (',' Identifier)*
	;

lambdaBody
	:	expression
	|	block
	;

assignmentExpression
	:	conditionalExpression
	|	assignment
	;

assignment
	:	leftHandSide assignmentOperator expression
	;

leftHandSide
	:	dottedIdentifier
	|	fieldAccess
	|	arrayAccess
	;

assignmentOperator
	:	'='
	|	'*='
	|	'/='
	|	'%='
	|	'+='
	|	'-='
	|	'<<='
	|	'>>='
	|	'>>>='
	|	'&='
	|	'^='
	|	'|='
	;

conditionalExpression
	:	conditionalOrExpression
	|	conditionalOrExpression '?' expression ':' conditionalExpression
	;

conditionalOrExpression
	:	conditionalAndExpression
	|	conditionalOrExpression '||' conditionalAndExpression
	;

conditionalAndExpression
	:	inclusiveOrExpression
	|	conditionalAndExpression '&&' inclusiveOrExpression
	;

inclusiveOrExpression
	:	exclusiveOrExpression
	|	inclusiveOrExpression '|' exclusiveOrExpression
	;

exclusiveOrExpression
	:	andExpression
	|	exclusiveOrExpression '^' andExpression
	;

andExpression
	:	equalityExpression
	|	andExpression '&' equalityExpression
	;

equalityExpression
	:	relationalExpression
	|	equalityExpression operator='==' relationalExpression
	|	equalityExpression operator='!=' relationalExpression
	;

relationalExpression
	:	shiftExpression
	|	relationalExpression operator='<' shiftExpression
	|	relationalExpression operator='>' shiftExpression
	|	relationalExpression operator='<=' shiftExpression
	|	relationalExpression operator='>=' shiftExpression
	|	relationalExpression operator='instanceof' referenceType
	;

shiftExpression
	:	additiveExpression
	|	shiftExpression shiftOperator additiveExpression
	;

shiftOperator
	:	'<' '<'
	|	'>' '>'
	|	'>' '>' '>'
	;

additiveExpression
	:	multiplicativeExpression
	|	additiveExpression operator='+' multiplicativeExpression
	|	additiveExpression operator='-' multiplicativeExpression
	;

multiplicativeExpression
	:	unaryExpression
	|	multiplicativeExpression operator='*' unaryExpression
	|	multiplicativeExpression operator='/' unaryExpression
	|	multiplicativeExpression operator='%' unaryExpression
	;

unaryExpression
	:	preIncrementExpression
	|	preDecrementExpression
	|	operator='+' unaryExpression
	|	operator='-' unaryExpression
	|	unaryExpressionNotPlusMinus
	;

preIncrementExpression
	:	operator='++' unaryExpression
	;

preDecrementExpression
	:	operator='--' unaryExpression
	;

unaryExpressionNotPlusMinus
	:	postfixExpression
	|	operator='~' unaryExpression
	|	operator='!' unaryExpression
	|	castExpression
	;

postfixExpression
	:	(	primary
		|	dottedIdentifier
		)
		postfixExpressionOperator*
	;

postIncrementExpression
	:	postfixExpression '++'
	;

postDecrementExpression
	:	postfixExpression '--'
	;

postfixExpressionOperator
	:	'--'
	|	'++'
	;

castExpression
	:	'(' primitiveType ')' unaryExpression
	|	'(' referenceType ('&' classOrInterfaceType)* ')' unaryExpressionNotPlusMinus
	|	'(' referenceType ('&' classOrInterfaceType)* ')' lambdaExpression
	;

// LEXER

// §3.9 Keywords

ABSTRACT : 'abstract';
ASSERT : 'assert';
BOOLEAN : 'boolean';
BREAK : 'break';
BYTE : 'byte';
CASE : 'case';
CATCH : 'catch';
CHAR : 'char';
CLASS : 'class';
CONST : 'const';
CONTINUE : 'continue';
DEFAULT : 'default';
DO : 'do';
DOUBLE : 'double';
ELSE : 'else';
ENUM : 'enum';
EXTENDS : 'extends';
FINAL : 'final';
FINALLY : 'finally';
FLOAT : 'float';
FOR : 'for';
IF : 'if';
GOTO : 'goto';
IMPLEMENTS : 'implements';
IMPORT : 'import';
INSTANCEOF : 'instanceof';
INT : 'int';
INTERFACE : 'interface';
LONG : 'long';
NATIVE : 'native';
NEW : 'new';
PACKAGE : 'package';
PRIVATE : 'private';
PROTECTED : 'protected';
PUBLIC : 'public';
RETURN : 'return';
SHORT : 'short';
STATIC : 'static';
STRICTFP : 'strictfp';
SUPER : 'super';
SWITCH : 'switch';
SYNCHRONIZED : 'synchronized';
THIS : 'this';
THROW : 'throw';
THROWS : 'throws';
TRANSIENT : 'transient';
TRY : 'try';
VOID : 'void';
VOLATILE : 'volatile';
WHILE : 'while';

// §3.10.1 Integer Literals

IntegerLiteral
	:	DecimalIntegerLiteral
	|	HexIntegerLiteral
	|	OctalIntegerLiteral
	|	BinaryIntegerLiteral
	;

fragment
DecimalIntegerLiteral
	:	DecimalNumeral IntegerTypeSuffix?
	;

fragment
HexIntegerLiteral
	:	HexNumeral IntegerTypeSuffix?
	;

fragment
OctalIntegerLiteral
	:	OctalNumeral IntegerTypeSuffix?
	;

fragment
BinaryIntegerLiteral
	:	BinaryNumeral IntegerTypeSuffix?
	;

fragment
IntegerTypeSuffix
	:	[lL]
	;

fragment
DecimalNumeral
	:	'0'
	|	NonZeroDigit (Digits? | Underscores Digits)
	;

fragment
Digits
	:	Digit (DigitsAndUnderscores? Digit)?
	;

fragment
Digit
	:	'0'
	|	NonZeroDigit
	;

fragment
NonZeroDigit
	:	[1-9]
	;

fragment
DigitsAndUnderscores
	:	DigitOrUnderscore+
	;

fragment
DigitOrUnderscore
	:	Digit
	|	'_'
	;

fragment
Underscores
	:	'_'+
	;

fragment
HexNumeral
	:	'0' [xX] HexDigits
	;

fragment
HexDigits
	:	HexDigit (HexDigitsAndUnderscores? HexDigit)?
	;

fragment
HexDigit
	:	[0-9a-fA-F]
	;

fragment
HexDigitsAndUnderscores
	:	HexDigitOrUnderscore+
	;

fragment
HexDigitOrUnderscore
	:	HexDigit
	|	'_'
	;

fragment
OctalNumeral
	:	'0' Underscores? OctalDigits
	;

fragment
OctalDigits
	:	OctalDigit (OctalDigitsAndUnderscores? OctalDigit)?
	;

fragment
OctalDigit
	:	[0-7]
	;

fragment
OctalDigitsAndUnderscores
	:	OctalDigitOrUnderscore+
	;

fragment
OctalDigitOrUnderscore
	:	OctalDigit
	|	'_'
	;

fragment
BinaryNumeral
	:	'0' [bB] BinaryDigits
	;

fragment
BinaryDigits
	:	BinaryDigit (BinaryDigitsAndUnderscores? BinaryDigit)?
	;

fragment
BinaryDigit
	:	[01]
	;

fragment
BinaryDigitsAndUnderscores
	:	BinaryDigitOrUnderscore+
	;

fragment
BinaryDigitOrUnderscore
	:	BinaryDigit
	|	'_'
	;

// §3.10.2 Floating-Point Literals

FloatingPointLiteral
	:	DecimalFloatingPointLiteral
	|	HexadecimalFloatingPointLiteral
	;

fragment
DecimalFloatingPointLiteral
	:	Digits '.' Digits? ExponentPart? FloatTypeSuffix?
	|	'.' Digits ExponentPart? FloatTypeSuffix?
	|	Digits ExponentPart FloatTypeSuffix?
	|	Digits FloatTypeSuffix
	;

fragment
ExponentPart
	:	ExponentIndicator SignedInteger
	;

fragment
ExponentIndicator
	:	[eE]
	;

fragment
SignedInteger
	:	Sign? Digits
	;

fragment
Sign
	:	[+-]
	;

fragment
FloatTypeSuffix
	:	[fFdD]
	;

fragment
HexadecimalFloatingPointLiteral
	:	HexSignificand BinaryExponent FloatTypeSuffix?
	;

fragment
HexSignificand
	:	HexNumeral '.'?
	|	'0' [xX] HexDigits? '.' HexDigits
	;

fragment
BinaryExponent
	:	BinaryExponentIndicator SignedInteger
	;

fragment
BinaryExponentIndicator
	:	[pP]
	;

// §3.10.3 Boolean Literals

BooleanLiteral
	:	'true'
	|	'false'
	;

// §3.10.4 Character Literals

CharacterLiteral
	:	'\'' SingleCharacter '\''
	|	'\'' EscapeSequence '\''
	;

fragment
SingleCharacter
	:	~['\\\r\n]
	;

// §3.10.5 String Literals

StringLiteral
	:	'"' StringCharacters? '"'
	;

fragment
StringCharacters
	:	StringCharacter+
	;

fragment
StringCharacter
	:	~["\\\r\n]
	|	EscapeSequence
	;

// §3.10.6 Escape Sequences for Character and String Literals

fragment
EscapeSequence
	:	'\\' [btnfr"'\\]
	|	OctalEscape
    |   UnicodeEscape // This is not in the spec but prevents having to preprocess the input
	;

fragment
OctalEscape
	:	'\\' OctalDigit
	|	'\\' OctalDigit OctalDigit
	|	'\\' ZeroToThree OctalDigit OctalDigit
	;

fragment
ZeroToThree
	:	[0-3]
	;

// This is not in the spec but prevents having to preprocess the input
fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

// §3.10.7 The Null Literal

NullLiteral
	:	'null'
	;

// §3.11 Separators

LPAREN : '(';
RPAREN : ')';
LBRACE : '{';
RBRACE : '}';
LBRACK : '[';
RBRACK : ']';
SEMI : ';';
COMMA : ',';
DOT : '.';

// §3.12 Operators

ASSIGN : '=';
GT : '>';
LT : '<';
BANG : '!';
TILDE : '~';
QUESTION : '?';
COLON : ':';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&&';
OR : '||';
INC : '++';
DEC : '--';
ADD : '+';
SUB : '-';
MUL : '*';
DIV : '/';
BITAND : '&';
BITOR : '|';
CARET : '^';
MOD : '%';
ARROW : '->';
COLONCOLON : '::';

ADD_ASSIGN : '+=';
SUB_ASSIGN : '-=';
MUL_ASSIGN : '*=';
DIV_ASSIGN : '/=';
AND_ASSIGN : '&=';
OR_ASSIGN : '|=';
XOR_ASSIGN : '^=';
MOD_ASSIGN : '%=';
LSHIFT_ASSIGN : '<<=';
RSHIFT_ASSIGN : '>>=';
URSHIFT_ASSIGN : '>>>=';

// §3.8 Identifiers (must appear after all keywords in the grammar)

Identifier
	:	JavaLetter JavaLetterOrDigit*
	;

fragment
JavaLetter
	:	[a-zA-Z$_] // these are the "java letters" below 0x7F
	|	// covers all characters above 0x7F which are not a surrogate
		~[\u0000-\u007F\uD800-\uDBFF]
		{Character.isJavaIdentifierStart(_input.LA(-1))}?
	|	// covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
		[\uD800-\uDBFF] [\uDC00-\uDFFF]
		{Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
	;

fragment
JavaLetterOrDigit
	:	[a-zA-Z0-9$_] // these are the "java letters or digits" below 0x7F
	|	// covers all characters above 0x7F which are not a surrogate
		~[\u0000-\u007F\uD800-\uDBFF]
		{Character.isJavaIdentifierPart(_input.LA(-1))}?
	|	// covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
		[\uD800-\uDBFF] [\uDC00-\uDFFF]
		{Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
	;

//
// Additional symbols not defined in the lexical specification
//

AT : '@';
ELLIPSIS : '...';

//
// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;
