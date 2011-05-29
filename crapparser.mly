%{
  open Printf

  let parse_error s =
    print_endline s;
    flush stdout

  open Guts

%}

%token SEPARATOR
%token NEWLINE

%token <float> NUM
%token <string> ID

%token ASSIGN
%token PLUS MINUS MULTIPLY DIVIDE
%token P_PLUS P_MINUS P_SCALE_UP P_SCALE_DOWN

%token P_CREATE

%token LOOP_START LOOP_END

%token PRINT_STACK PRINT_DICTIONARY

%token LT GT

%token LINE CIRCLE ELLIPSE

%token NGONLOOP

%token MAKE_CIRCLE MAKE_ELLIPSE MAKE_NGON

%start input
%type <unit> input

%type <unit> exp

%type <Types.thing> thing
%type <Types.thing list> things
%type <Types.thing list> lst
%type <Types.thing list> line
%type <Types.thing list> circle
%type <Types.thing * Types.thing * Types.thing * Types.thing> loop_end

%%

input:  | cline                 { }
        | input NEWLINE cline   { }

cline:     cline1                { }
         | SEPARATOR cline1      { }
         | cline1 SEPARATOR      { }
         | SEPARATOR cline1 SEPARATOR {}
;

cline1:    /* empty */          { }
        | exp                  { }
        | cline1 SEPARATOR exp  { }
;

exp:      NUM               { process_command (Types.Num $1) }
        | ID                { process_command (Types.Id $1) }
        | PLUS              { process_command Types.Plus }
        | MINUS             { process_command Types.Minus }
        | MULTIPLY          { process_command Types.Multiply }
        | DIVIDE            { process_command Types.Divide }
        | P_PLUS            { process_command Types.PointPlus }
        | P_MINUS           { process_command Types.PointMinus }
        | P_SCALE_UP        { process_command Types.PointScaleUp }
        | P_SCALE_DOWN      { process_command Types.PointScaleDown }
        | P_CREATE          { process_command Types.MakePoint }
        | PRINT_STACK       { process_command Types.PrintStack }
        | PRINT_DICTIONARY  { process_command Types.PrintDictionary }
        | ASSIGN            { process_command Types.Assign }
        | MAKE_CIRCLE       { process_command Types.MakeCircle }
        | MAKE_ELLIPSE      { process_command Types.MakeEllipse }
        | MAKE_NGON         { process_command Types.MakeNgon }
        | line              { process_command (Types.Line $1) }
        | circle            { process_command (Types.Circle $1) }
        | ellipse           { process_command Types.Ellipse }
        | LOOP_START        { process_command Types.LoopStart }
        | loop_end          { process_command (Types.Ngonloop $1)}
;

line:                 lst SEPARATOR LINE    { $1 }
;

circle:               lst SEPARATOR CIRCLE  { $1 }
;

ellipse:              ELLIPSE               { }
;

lst:                  LT SEPARATOR things SEPARATOR GT
                                            { $3 }
;

things:               thing                 { [$1] }
                    | things SEPARATOR thing
                                            { List.append $1 [$3] }
;

thing:                ID                    { Types.String $1 }
                    | NUM                   { Types.Number $1 }
;

loop_end:             LOOP_END SEPARATOR thing SEPARATOR thing SEPARATOR thing SEPARATOR thing SEPARATOR NGONLOOP
                                            { ($3, $5, $7, $9) }
;

%%
