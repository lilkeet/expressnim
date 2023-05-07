 

{.experimental: "strictFuncs".}

import
  std/[pegs, options],
  structure

let syntax = peg"""syntax <- schema_decl+
expressChar <- [!-~] # Every valid character in Express

# Keywords! ####################################################################
ABS <- i'abs'
ABSTRACT <- i'abstract'
ACOS <- i'acos'
AGGREGATE <- i'aggregate'
ALIAS <- i'alias'
AND <- i'and'
ANDOR <- i'andor'
ARRAY <- i'array'
AS <- i'as'
ASIN <- i'asin'
ATAN <- i'atan'
BAG <- i'bag'
BASED_ON <- i'based_on'
BEGIN <- i'begin'
BINARY <- i'binary'
BLENGTH <- i'blength'
BOOLEAN <- i'boolean'
BY <- i'by'
CASE <- i'case'
CONSTANT <- i'constant'
CONST_E <- i'const_e'
COS <- i'cos'
DERIVE <- i'derive'
DIV <- i'div'
ELSE <- i'else'
END <- i'end'
END_ALIAS <- i'end_alias'
END_CASE <- i'end_case'
END_CONSTANT <- i'end_constant'
END_ENTITY <- i'end_entity'
END_FUNCTION <- i'end_function'
END_IF <- i'end_if'
END_LOCAL <- i'end_local'
END_PROCEDURE <- i'end_procedure'
END_REPEAT <- i'end_repeat'
END_RULE <- i'end_rule'
END_SCHEMA <- i'end_schema'
END_SUBTYPE_CONSTRAINT <- i'end_subtype_constraint'
END_TYPE <- i'end_type'
ENTITY <- i'entity'
ENUMERATION <- i'enumeration'
ESCAPE <- i'escape'
EXISTS <- i'exists'
EXTENSIBLE <- i'extensible'
EXP <- i'exp'
FALSE <- i'false'
FIXED <- i'fixed'
FOR <- i'for'
FORMAT <- i'format'
FROM <- i'from'
FUNCTION <- i'function'
GENERIC <- i'generic'
GENERIC_ENTITY <- i'generic_entity'
HIBOUND <- i'hibound'
HIINDEX <- i'hiindex'
IF <- i'if'
IN <- i'in'
INSERT <- i'insert'
INTEGER <- i'integer'
INVERSE <- i'inverse'
LENGTH <- i'length'
LIKE <- i'like'
LIST <- i'list'
LOBOUND <- i'lobound'
LOCAL <- i'local'
LOG <- i'log'
LOG10 <- i'log10'
LOG2 <- i'log2'
LOGICAL <- i'logical'
LOINDEX <- i'loindex'
MOD <- i'mod'
NOT <- i'not'
NUMBER <- i'number'
NVL <- i'nvl'
ODD <- i'odd'
OF <- i'of'
ONEOF <- i'oneof'
OPTIONAL <- i'optional'
OR <- i'or'
OTHERWISE <- i'otherwise'
PI <- i'pi'
PROCEDURE <- i'procedure'
QUERY <- i'query'
REAL <- i'real'
REFERENCE <- i'reference'
REMOVE <- i'remove'
RENAMED <- i'renamed'
REPEAT <- i'repeat'
RETURN <- i'return'
ROLESOF <- i'rolesof'
RULE <- i'rule'
SCHEMA <- i'schema'
SELECT <- i'select'
SELF <- i'self'
SET <- i'set'
SIN <- i'sin'
SIZEOF <- i'sizeof'
SKIP <- i'skip'
SQRT <- i'sqrt'
STRING <- i'string'
SUBTYPE <- i'subtype'
SUBTYPE_CONSTRAINT <- i'subtype_constraint'
SUPERTYPE <- i'supertype'
TAN <- i'tan'
THEN <- i'then'
TO <- i'to'
TOTAL_OVER <- i'total_over'
TRUE <- i'true'
TYPE <- i'type'
TYPEOF <- i'typeof'
UNIQUE <- i'unique'
UNKNOWN <- i'unknown'
UNTIL <- i'until'
USE <- i'use'
USEDIN <- i'usedin'
VALUE <- i'value'
VALUE_IN <- i'value_in'
VALUE_UNIQUE <- i'value_unique'
VAR <- i'var'
WHERE <- i'where'
WHILE <- i'while'
WITH <- i'with'
XOR <- i'xor'

# Keyword Grouping! ############################################################
builtInConst <- CONST_E / PI / SELF / [?]
builtInFunc <- ABS / ACOS / ASIN / ATAN / BLENGTH / COS / EXISTS / EXP /
               FORMAT / HIBOUND / HIINDEX / LENGTH / LOBOUND / LOINDEX / LOG /
               LOG2 / LOG10 / NVL / ODD / ROLESOF / SIN / SIZEOF / SQRT / TAN /
               TYPEOF / USEDIN / VALUE / VALUE_IN / VALUE_UNIQUE
builtInProc <- INSERT / REMOVE



# Whitespace! ##################################################################
#### Normal whitespace
horizontalTab <- \009
lineFeed <- \010
carriageReturn <- \013
normalWhiteSpace <- horizontalTab / lineFeed / carriageReturn / [ ] / \n

#### Remarks
# Remarks must be treated as whitespace, in order to meet the ISO's
# requirements to be an EXPRESS parser.
embeddedMarker <- [()*]
embeddedRemark <- "(*" (embeddedRemark / (!embeddedMarker expressChar))* "*)"
tailRemark <- "--" (!\n expressChar)* \n

remark <- embeddedRemark / tailRemark

whiteSpace <- (normalWhiteSpace / remark)+
ws <- whiteSpace


# Type Descriptions! ###########################################################
#### Simple types
precision <- numeric_expression
width <- numeric_expression
width_spec <- [(]  width  [)]  FIXED?

binaryTypeDesc <- BINARY width_spec?
boolTypeDesc <- BOOLEAN
intTypeDesc <- INTEGER
logicalTypeDesc <- LOGICAL
numberTypeDesc <- NUMBER
realTypeDesc <- REAL  ([(]  precision  [)])?
strTypeDesc <- STRING  width_spec?

simpleTypes <- binaryTypeDesc / boolTypeDesc / intTypeDesc / logicalTypeDesc /
               numberTypeDesc / realTypeDesc / strTypeDesc

#### Aggregation types
boundA <- numeric_expression
boundB <- numeric_expression
bounds <- '['  boundA  [:]  boundB  ']'

arrayTypeDesc <- ARRAY  bounds  OF  OPTIONAL?  UNIQUE?
                 instantiableTypes
bagTypeDesc <- BAG  bounds?  OF  instantiableTypes
listTypeDesc <- LIST  bounds?  OF  UNIQUE?  instantiableTypes
setTypeDesc <- SET  bounds?  OF  instantiableTypes

aggregationTypes <- arrayTypeDesc / bagTypeDesc / listTypeDesc / setTypeDesc

#### Other types
enumeration_items <- [(] enumIdent ([,] enumIdent)* [)]
enumerationTypeDesc <- EXTENSIBLE? ENUMERATION ((OF enumeration_items) /
                       (BASED_ON typeIdent (WITH enumeration_items)?))?

select_list <- [(] namedTypes ([,] namedTypes)* [)]
select_type <- (EXTENSIBLE GENERIC_ENTITY?)? SELECT (select_list
                / (BASED_ON typeIdent (WITH select_list)?))?

#### Generalized types
# Generalized types are a generalization of other types.
# Generics are a generalized type that covers every possible type.


generalizedArrayTypeDesc <- ARRAY  bounds?  OF  OPTIONAL?  UNIQUE?
                             parameterTypes
generalizedBagTypeDesc <- BAG  bounds?  OF  parameterTypes
generalizedListTypeDesc <- LIST  bounds?  OF  UNIQUE?
                           parameterTypes
generalizedSetTypeDesc <- SET  bounds?  OF  parameterTypes


generalizedAggregationTypes <- generalizedArrayTypeDesc /
                               generalizedBagTypeDesc /
                               generalizedListTypeDesc /
                               generalizedSetTypeDesc

genericTypeId <- simpleIdent # This is the label given to the type in
                          # a generic. Traditionally in Nim, this is
                          # something like 'T', 'S', or 'U'.

aggregateTypeDesc <- AGGREGATE  ([:]  genericTypeId)?  OF
                     parameterTypes

generic_entity_type <- GENERIC_ENTITY  ([:]  genericTypeId)?
genericTypeDesc <- GENERIC  ([:]  genericTypeId)?

generalizedTypes <- aggregateTypeDesc / generalizedAggregationTypes /
                    generic_entity_type / genericTypeDesc

#### Type groupings
concreteTypes <- aggregationTypes / simpleTypes / typeIdent
instantiableTypes <- concreteTypes / entityIdent
parameterTypes <- generalizedTypes / namedTypes / simpleTypes
namedTypes <- entityIdent / typeIdent
constructedTypes <- enumerationTypeDesc / select_type
underlyingTypes <- concreteTypes / constructedTypes


# Expressions! #################################################################
# These can be evaluated and return a value.
#### Literals
bit <- [01]
hexdigit <- \d / [a-fA-F]
octet <- hexdigit hexdigit
encodedChar <- octet octet octet octet
escapedApostrophe <- ['] [']
notApostropheOrNewLine <- (!['] (!\n) expressChar

binaryLit <- [%] bit+
encodedStrLit <- ["] encodedChar+ ["]
intLit <- \d+
realLit <- (\d+) [.] (\d*) (([eE] ([+-]?) \d+)?)
simpleStrLit <- ['] ((escapedApostrophe / (!(['] / \n) exp)*) [']
stringLit <- simpleStrLit / encodedStrLit
logicalLit <- TRUE / FALSE / UNKNOWN

literal <- binaryLit / realLit / intLit / stringLit / logicalLit

#### Idents
simpleIdent <- \a (\w*)

constIdent <- simpleIdent
funcIdent <- simpleIdent
attributeIdent <- simpleIdent
typeIdent <- simpleIdent # A custom alias for a type, like a `typedef` in C.
entityIdent <- simpleIdent # An `object` in Nim, a `struct` in C.
parameterIdent <- simpleIdent
ruleIdent <- simpleIdent
ruleLabelIdent <- simpleIdent
varIdent <- simpleIdent
enumIdent <- simpleIdent
procIdent <- simpleIdent
schemaIdent <- simpleIdent
subtypeConstraintIdent <- simpleIdent


renameIdent <- constIdent / entityIdent / funcIdent / procIdent / typeIdent


population <- entityIdent

#### Operators
unaryOp <- [+-] / NOT
addLikeOp <- [+-] / OR / XOR
multiplicationLikeOp <- '||' / [*//] / DIV / MOD / AND
relationalOp <-  '<<-' / '><-' / '<>' / ':<>:' / ':<-:' / [<>=] / IN / LIKE
intervalOp <- [<] / '<<-'


#### Factors
parameter <- expression
parameterList <- [(] parameter ([,] parameter)* [)]
funcCall <- funcIdent parameterList?

constFactor <- builtInConst / constIdent

generalReference <- parameterIdent / varIdent

qualifiableFactor <- attributeIdent / constFactor / funcCall /
                      generalReference / population


attributeQualifier <- [.] attributeIdent
groupQualifier <- [\\] entityIdent

index <- numeric_expression
indexA <- index
indexB <- index
indexQualifier <- '[' indexA ([:] indexB)? ']'

qualifier <- attributeQualifier / groupQualifier / indexQualifier

primary <- literal / (qualifiableFactor qualifier*)


aggregateInitializer <- '[' (element ([,] element)* )? ']'
entityConstructor <- entityIdent [(] (expression ([,] expression)*)? [)]

intervalLow <- numeric_expression
intervalItem <- numeric_expression
intervalHigh <- numeric_expression
interval <- ’{’ intervalLow intervalOp intervalItem intervalOp intervalHigh ’}’


logicalExpression <- expression
aggregateSource <- numeric_expression
query_expression <- QUERY [(] varIdent '<*' aggregateSource [//]
                    logicalExpression [)]
enumReference <- (typeIdent [.])? enumIdent

simple_factor <- aggregateInitializer / entityConstructor /
                 enumReference / interval / query_expression /
                 (unaryOp? (([(] expression [)]) / primary))
factor <- simple_factor ('**' simple_factor)?

term <- factor (multiplicationLikeOp factor)*
numeric_expression <- term (addLikeOp term)*

expression <- numeric_expression (relationalOp numeric_expression)?


#### TypeDesc Expressions
# This is the same as `int|float|string` in Nim:
oneOfExpr <- ONEOF [(] supertypeExpr ([,] supertypeExpr)* [)]


supertypeTerm <- entityIdent / oneOfExpr / ([(] supertypeExpr [)])
supertypeFactor <- supertypeTerm (AND supertypeTerm)*
supertypeExpr <- supertypeFactor (ANDOR supertypeFactor)*

subtypeConstraintExpr <- OF [(] supertypeExpr [)]

#### Other
totalOverExpr <- TOTAL_OVER [(] entityIdent ([,] entityIdent)* [)] [;]


# Statements! ##################################################################
# These are non-declarative actions taken; procedure-like code.

nullStmt <- [;]
skipStmt <- SKIP [;]

assignmentStmt <- generalReference (qualifier*) ':<-' expression [;]

selector <- expression
oneCase <- expression
caseStmt <- CASE selector OF
              (oneCase ([,] oneCase)* [:] stmt)*
              (OTHERWISE [:] stmt)?
            END_CASE [;]

aliasStmt <- ALIAS varIdent FOR generalReference (qualifier*) [;]
              (stmt+)
             END_ALIAS [;]

compoundStmt <- BEGIN
                  stmt+
                END [;]

escapeStmt <- ESCAPE [;]

ifStmt <- IF logicalExpression THEN
            stmt+
          (ELSE
            stmt+)?
          END_IF [;]

procCallStmt <- procIdent parameterList? [;]


increment <- numeric_expression
incrementControl <- varIdent ':<-' boundA TO boundB (BY increment)?

whileControl <- WHILE logicalExpression

untilControl <- UNTIL logicalExpression

repeatControl <- incrementControl? whileControl? untilControl?

repeatStmt <- REPEAT repeatControl [;]
                stmt+
              END_REPEAT [;]


returnStmt <- RETURN ([(] expression [)])? [;]

stmt <- aliasStmt / assignmentStmt / caseStmt / compoundStmt / escapeStmt /
        ifStmt / nullStmt / procCallStmt / repeatStmt / returnStmt /
        skipStmt

# Declarations! ################################################################

constant_decl <- CONSTANT
                   (constIdent [:] instantiableTypes ':<-' expression [;])+
                 END_CONSTANT [;]


parameterDecl <- parameterIdent ([,] parameterIdent)* [:] parameterTypes

procDecl <- PROCEDURE procIdent ([(] VAR? parameterDecl ([;] VAR?
            parameterDecl)* [)])? [;]
              declaration* constant_decl? localDecl?
              stmt*
            END_PROCEDURE [;]


localDecl <- LOCAL
               (varIdent ([,] varIdent)* [:] parameterTypes (':<-' expression)?
               [;])+
             END_LOCAL [;]



abstractEntityDecl <- ABSTRACT
abstractSupertypeDecl <- ABSTRACT SUPERTYPE subtypeConstraintExpr?

supertype_rule <- SUPERTYPE subtypeConstraintExpr


supertypeConstraint <- abstractEntityDecl / abstractSupertypeDecl /
                        supertype_rule

subtypeDecl <- SUBTYPE OF [(] entityIdent ([,] entityIdent)* [)]

attributeDecl <- simpleIdent / redeclared_attribute
explicit_attr <- attributeDecl ([,] attributeDecl)* [:] OPTIONAL?
                 parameterTypes [;]


deriveClause <- DERIVE
                   (attributeDecl [:] parameterTypes ':<-' expression [;])+

inverseClause <- INVERSE
                    (attributeDecl [:] ((SET / BAG) bounds? OF)? entityIdent
                     FOR (entityIdent [.])? attributeIdent [;])+

referenced_attribute <- attributeIdent / qualified_attribute
unique_clause <- UNIQUE
                   ((ruleIdent [:])? referenced_attribute ([,]
                     referenced_attribute)* [;])+


whereClause <- WHERE
                 ((ruleLabelIdent [:])? expression [;])+

entityDecl <- ENTITY entityIdent
                supertypeConstraint?
                subtypeDecl? [;]

                explicit_attr*

              deriveClause?
              inverseClause?
              unique_clause?
              whereClause?
              END_ENTITY [;]



funcDecl <- FUNCTION funcIdent ([(] parameterDecl ([;] parameterDecl)* [)])?
            [:] parameterTypes [;]
              declaration*
              constant_decl?
              localDecl?
              stmt+
            END_FUNCTION [;]

subtype_constraint_decl <- SUBTYPE_CONSTRAINT subtypeConstraintIdent FOR
                           entityIdent [;]
                             abstractSupertype? totalOverExpr?
                             (supertypeExpr [;])?
                           END_SUBTYPE_CONSTRAINT [;]



declaration <- entityDecl / funcDecl / procDecl /
               subtype_constraint_decl / typeDecl


rule_decl <- RULE ruleIdent FOR [(] entityIdent ([,] entityIdent)* [)] [;]
               declaration*
               constant_decl?
               localDecl?
               stmt*
             whereClause
             END_RULE [;]


abstractSupertype <- ABSTRACT SUPERTYPE [;]

typeDecl <- TYPE typeIdent [<-] underlyingTypes [;]
            whereClause?
            END_TYPE [;]






reference_clause <- REFERENCE FROM schemaIdent ([(] resource_or_rename
                    ([,] resource_or_rename)* [)])? [;]

named_type_or_rename <- namedTypes (AS (entityIdent / typeIdent))?
use_clause <- USE FROM schemaIdent ([(] named_type_or_rename
              ([,] named_type_or_rename)* [)])? [;]
interfaceSpecification <- reference_clause / use_clause

schema_decl <- SCHEMA schemaIdent schema_version_id? [;]
                 interfaceSpecification*
                 constant_decl?
                 (declaration / rule_decl)*
               END_SCHEMA [;]

# Placeholder! #################################################################




qualified_attribute <- SELF groupQualifier attributeQualifier
redeclared_attribute <- qualified_attribute (RENAMED simpleIdent)?
repetition <- numeric_expression
resource_or_rename <- resource_ref (AS renameIdent)?
resource_ref <- constIdent / entityIdent / funcIdent / procIdent /
                typeIdent
schema_version_id <- stringLit


"""
